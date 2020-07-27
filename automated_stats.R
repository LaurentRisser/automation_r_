# Install packages:
# install.packages("dplyr")
# install.packages("tidyr")

# Load packages:
library(dplyr)
library(tidyr)
library(lubridate)


stats <- function(file_name, path_to_file) {

# Read csv (comma-delimited file). This will read only the three needed columns (type, date and resolved cases):
input <- read.csv(paste(path_to_file,file_name,sep=""), sep = ",", header = TRUE, colClasses = c("character","character","character"), na.strings = NA)[,c(2,8,12)]

# Change column titles to remove blank spaces:
colnames(input) <- c("Type","Due_date","Resolved_cases")

# Filter out the resolved cases:
input <- filter(input, Resolved_cases == "")

# Adapt date:
input$Due_date <- sub(" -.*", "", input$Due_date)
input$Due_date <- parse_date_time(input$Due_date, '%y-%m-%d %I:%M:%S %p')
input$Due_date <- as.POSIXct(input$Due_date, tz = "UTC")

# Create table to insert the output:
types_n <- length(unique(input$Type))
output <- data.frame(matrix(NA, nrow = 2, ncol = types_n), row.names = c("Current count", "Next day"))
colnames(output) <- c("Banking","E & O","Licence","Backcheck","Name Change","Verify Corporation")  # c(unique(input$Type)) 

# Parameters for output:
unique_types<- c("banking","eocoverage","licence","backcheck","namechange","articles") # c(unique(input$Type)) 
current_date = Sys.Date()

if (weekdays(current_date) != "Friday") {
  nextday_count <- as.POSIXct(paste(current_date + days(1),"12:00:00"), tz = "UTC")
  twodays_count <- as.POSIXct(paste(current_date + days(2),"12:00:00"), tz = "UTC")
} else {
  nextday_count <- as.POSIXct(paste(current_date + days(3),"12:00:00"), tz = "UTC")
  twodays_count <- as.POSIXct(paste(current_date + days(4),"12:00:00"), tz = "UTC")
}
y=1

# Loop to insert counts into the output:
for (type in unique_types) {
  x=1
  output[x,y] <- filter(input, Type == type) %>% filter(., Due_date <= nextday_count) %>% count(.)
  x = x+1
  output[x,y] <- filter(input, Type == type) %>% filter(., Due_date <= twodays_count) %>% count(.)
  y=y+1
}
colnames(output) <- c("Banking","E & O Coverage","Licence","Backcheck","Name Change","Verify Corporation")
write.table(output, file=paste(path_to_file,"stats-",current_date,".csv", sep=""), sep = ",", quote = FALSE, row.names = TRUE, col.names = NA)
}