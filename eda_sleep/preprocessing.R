


# Initial data
data <- read.csv("SLEEP_1660129098400.csv")
head(data)

# Remove the first row (empty data)
data <- data[2:dim(data)[1],]
head(data)

# Get the NA values of each variable
sapply(data, function(x) sum(is.na(x)))

# Remove the naps variable because it is 100% NAs
data$naps <- NULL
head(data)

# Create a variable for the weekday of each date
weekdays <- weekdays(as.Date(data$date))
weekdays

# Translate it into English
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
dias <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
for(i in 1:7){
  weekdays <- replace(weekdays, weekdays == dias[i], days[i])
}
weekdays

# Add new variable
data <- cbind(data, weekdays)
head(data)

# Get the total amount of time propperly sleeping
total <- rowSums(data[,c(2,3,7)])
total  

data <- cbind(data, total)
head(data)

# Porcentage of each sleep phase
deepSleepTimePorc <- round(data$deepSleepTime/data$total, 4) * 100
shallowSleepTimePorc <- round(data$shallowSleepTime/data$total, 4) * 100
wakeTimePorc <- round(data$wakeTime/data$total, 4) * 100
REMTimePorc <- round(data$REMTime/data$total, 4) * 100
data <- cbind(data, deepSleepTimePorc, shallowSleepTimePorc, wakeTimePorc, REMTimePorc)
head(data)

# Reorder the data frame columns
data <- data[, c(1, 8, 5, 6, 2, 10, 3, 11, 4, 12, 7, 13, 9)]
head(data)

# Convert times into hours and minutes
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

data$deepSleepTime_time <- sapply(data$deepSleepTime, getTime)
data$shallowSleepTime_time <- sapply(data$shallowSleepTime, getTime)
data$wakeTime_time <- sapply(data$wakeTime, getTime)
data$REMTime_time <- sapply(data$REMTime, getTime)
data$total_time <- sapply(data$total, getTime)
head(data)



# Store the new dataset
write.csv(data, "sleep_data.csv", row.names = FALSE)
