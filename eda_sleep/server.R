library(tidyr)
library(plotly)
library(ggcorrplot)
options(warn = -1)

source("preprocessing.R", local = TRUE)

server <- function(input, output){
  
  # Get the data depending on the days selected for the study
  getData <- function(){
    data <- read.csv("sleep_data.csv")
    if(input$weekdays == "Monday"){
      data <- data[data$weekdays == "Monday",]
    }
    if(input$weekdays == "Tuesday"){
      data <- data[data$weekdays == "Tuesday",]
    }
    if(input$weekdays == "Wednesday"){
      data <- data[data$weekdays == "Wednesday",]
    }
    if(input$weekdays == "Thursday"){
      data <- data[data$weekdays == "Thursday",]
    }
    if(input$weekdays == "Friday"){
      data <- data[data$weekdays == "Friday",]
    }
    if(input$weekdays == "Saturday"){
      data <- data[data$weekdays == "Saturday",]
    }
    if(input$weekdays == "Sunday"){
      data <- data[data$weekdays == "Sunday",]
    }
    return(data)
  }
  
  output$graph <- renderPlotly({
    
    # Get the right data according to the selected dates and ignore the days when the data has not been collected
    # for any reason
    data <- getData()
    date_ini <- input$date_range[1]
    date_fin <- input$date_range[2]
    data_plot <- data[as.Date(data$date) >= date_ini & as.Date(data$date) <= date_fin,]
    data_plot <- data_plot[data_plot$total_time != "00:00",]
    
    if(input$type_graph == "Sleep time series"){
      fig <- plot_ly(data_plot, x = ~date, y = ~total_time, type = 'scatter', mode = 'lines',  hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_lines(y = "08:00", x = range(data_plot$date), line = list(color = "green"), inherit = FALSE, showlegend = FALSE, hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_lines(y = "07:00", x = range(data_plot$date), line = list(color = "orange"), inherit = FALSE, showlegend = FALSE,hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_lines(y = "06:00", x = range(data_plot$date), line = list(color = "red"), inherit = FALSE, showlegend = FALSE,hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        layout(title = "Sleep time series", yaxis = list(title = "Time sleeping"), xaxis = list(title = "Date"))
    }
    
    if(input$type_graph == "Sleep phases series"){
      fig <- plot_ly(data_plot, x = ~date, y = ~deepSleepTime_time, type = 'scatter', mode = 'lines', name = "Deep sleep time", hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_trace(x = ~date, y = ~shallowSleepTime_time, name = "Shallow sleep time", hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_trace(x = ~date, y = ~wakeTime_time, name = "Wake time", hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        add_trace(x = ~date, y = ~REMTime_time, name = "REM sleep time", hovertemplate ='%{x}, %{y}<extra></extra>')%>%
        layout(title = "Sleep phases series", yaxis = list(title = "Time sleeping sleeping by phase"), xaxis = list(title = "Date"),
               showlegend = T, title ="")
    }
    
    if(input$type_graph == "Sleep phases (mean)"){
      mean_porcs <- c(mean(data_plot$shallowSleepTimePorc, na.rm = TRUE), mean(data_plot$deepSleepTimePorc, na.rm = TRUE), 
                      mean(data_plot$REMTimePorc, na.rm = TRUE), mean(data_plot$wakeTimePorc, na.rm = TRUE))
      mean_porcs <- round(c(100 - mean_porcs[4], mean_porcs), 2)
      
      # Formatting of time data
      getTime <- function(x){
        hours <- floor(x/60)
        minutes <- round(x%%60)
        if(nchar(as.character(hours)) == 1){
          hours <- gsub(" ", "", paste("0", hours, collapse = ""))
        }
        if(nchar(as.character(minutes)) == 1){
          minutes <- gsub(" ", "", paste("0", minutes, collapse = ""))
        }
        return(paste(hours, minutes, sep=":"))
      }
      
      fig <- plot_ly(data_plot, x = c("Total time", "Shallow sleep time", "Deep sleep time", "REM sleep time", "Wake time"),
                     y = c(sapply(mean(data_plot$total), getTime), sapply(mean(data_plot$shallowSleepTime), getTime), 
                           sapply(mean(data_plot$deepSleepTime), getTime),
                           sapply(mean(data_plot$REMTime), getTime), sapply(mean(data_plot$wakeTime), getTime)), 
                     customdata = mean_porcs, 
                     hovertemplate ='%{x}, %{y} (%{customdata}%)<extra></extra>',
                     type = "bar", color = c("blue", "green", "red", "purple", "orange"))%>%
        layout(title = "Sleep phases", yaxis = list(title = "Time sleeping sleeping by phase"), 
                        xaxis = list(title = "Phases", categoryorder = "total descending"), showlegend = F, title = "")
      
    }
    
    if(input$type_graph == "Correlation"){
      
      data_aux <- data[data$total > 0,]
      data_aux <- data.frame("total" = data_aux$total, "deep" = data_aux$deepSleepTimePorc, 
                             "shallow" = data_aux$shallowSleepTimePorc, "REM" = data_aux$REMTimePorc)
      
      # Correlation when less than 7.5, 6.5 and 5.5 hours of sleep
      m7.5 <- data_aux[data_aux$total < 450,]
      m6.5 <- data_aux[data_aux$total < 390,]
      m5.5 <- data_aux[data_aux$total < 330,]
      
      corr <- cor(data_aux)
      p.mat <- cor_pmat(data_aux)
      corr.plot <- ggcorrplot(
        corr, hc.order = TRUE, type = "lower", outline.col = "white",
        p.mat = p.mat
      )
      fig1 <- ggplotly(corr.plot, height = 500, width = 500)

      corr <- cor(m7.5)
      p.mat <- cor_pmat(data_aux)
      corr.plot <- ggcorrplot(
        corr, hc.order = TRUE, type = "lower", outline.col = "white",
        p.mat = p.mat
      )
      fig2 <- ggplotly(corr.plot, height = 500, width = 500)
      
      corr <- cor(m6.5)
      p.mat <- cor_pmat(data_aux)
      corr.plot <- ggcorrplot(
        corr, hc.order = TRUE, type = "lower", outline.col = "white",
        p.mat = p.mat
      )
      fig3 <- ggplotly(corr.plot, height = 500, width = 500)
      
      corr <- cor(m5.5)
      p.mat <- cor_pmat(data_aux)
      corr.plot <- ggcorrplot(
        corr, hc.order = TRUE, type = "lower", outline.col = "white",
        p.mat = p.mat
      )
      fig4 <- ggplotly(corr.plot, height = 500, width = 900)

      fig <- subplot(fig1, fig2, fig3, fig4, nrows = 2, margin = c(0.1, 0.1, 0.1, 0.2))%>% 
        layout(title = 'Correlation between total time sleeping and the porcentage of each sleep phase')
      
      annotations = list( 
        list( 
          x = 0.2,  
          y = 1.0,  
          text = "All the data",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.8,  
          y = 1,  
          text = "Less than 7:30h sleep time",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),  
        list( 
          x = 0.2,  
          y = 0.45,  
          text = "Less than 6:30 sleep time",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ),
        list( 
          x = 0.8,  
          y = 0.45,  
          text = "Less than 5:30h sleep time",  
          xref = "paper",  
          yref = "paper",  
          xanchor = "center",  
          yanchor = "bottom",  
          showarrow = FALSE 
        ))
      
      m <- list(
        l = 50,
        r = 50,
        b = 100,
        t = 100,
        pad = 4
      )
      
      fig <- fig %>%layout(annotations = annotations, margin = m) 
      
    }
    
    
    fig
  })
  
  
  output$table <- renderTable({
    data <- getData()
    data <- data[data$deepSleepTime > 0,]
    tab <- data.frame("Total" = c(paste(sapply(min(data$total), getTime)),
                                            paste(sapply(max(data$total), getTime)),
                                            paste(sapply(mean(data$total), getTime)),
                                            paste(sapply(quantile(data$total)[2], getTime)),
                                            paste(sapply(median(data$total), getTime)),
                                            paste(sapply(quantile(data$total)[4], getTime)),
                                            paste(sapply(sd(data$total), getTime))
                                            ),
                      "Deep" = c(paste(sapply(min(data$deepSleepTime), getTime)),
                                            paste(sapply(max(data$deepSleepTime), getTime)),
                                            paste(sapply(mean(data$deepSleepTime), getTime)),
                                            paste(sapply(quantile(data$deepSleepTime)[2], getTime)),
                                            paste(sapply(median(data$deepSleepTime), getTime)),
                                            paste(sapply(quantile(data$deepSleepTime)[4], getTime)),
                                            paste(sapply(sd(data$deepSleepTime), getTime))
                                            ),
                      "Shallow" = c(paste(sapply(min(data$shallowSleepTime), getTime)),
                                            paste(sapply(max(data$shallowSleepTime), getTime)),
                                            paste(sapply(mean(data$shallowSleepTime), getTime)),
                                            paste(sapply(quantile(data$shallowSleepTime)[2], getTime)),
                                            paste(sapply(median(data$shallowSleepTime), getTime)),
                                            paste(sapply(quantile(data$shallowSleepTime)[4], getTime)),
                                            paste(sapply(sd(data$shallowSleepTime), getTime))
                                            ),
                      "REM" = c(paste(sapply(min(data$REMTime), getTime)),
                                            paste(sapply(max(data$REMTime), getTime)),
                                            paste(sapply(mean(data$REMTime), getTime)),
                                            paste(sapply(quantile(data$REMTime)[2], getTime)),
                                            paste(sapply(median(data$REMTime), getTime)),
                                            paste(sapply(quantile(data$REMTime)[4], getTime)),
                                            paste(sapply(sd(data$REMTime), getTime))
                                          ),
                      "Wake" = c(paste(sapply(min(data$wakeTime), getTime)),
                                           paste(sapply(max(data$wakeTime), getTime)),
                                           paste(sapply(mean(data$wakeTime), getTime)),
                                           paste(sapply(quantile(data$wakeTime)[2], getTime)),
                                           paste(sapply(median(data$wakeTime), getTime)),
                                           paste(sapply(quantile(data$wakeTime)[4], getTime)),
                                           paste(sapply(sd(data$wakeTime), getTime)))
    )
                      
    rownames(tab) <- c("Min", "Max", "Mean", "P25%", "Median", "P75%", "Sd")
    tab
  }, rownames = TRUE)
  
}