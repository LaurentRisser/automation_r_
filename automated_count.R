# Install packages:
#install.packages("dplyr")
#install.packages("tidyr")

# Load packages:
library(dplyr)
library(tidyr)

counts <- function(file_name, path_to_file) {

# Set your working directory:
setwd("C:/Users/s0047576/Desktop/Script/")

# Read csv (comma-delimited file). This will read only the three needed columns (type, date and agent):
input <- read.csv("input_file.csv", sep = ",", header = TRUE, colClasses = c("character","character","character"))[,c(2,11,12)]

# Change column titles to remove blank spaces:
colnames(input) <- c("Type","Resolved_date","Resolved_by")
input$Resolved_date <- sub(" .*", "", input$Resolved_date)

# Get the number of unique dates and agents:
days_n <- length(unique(input$Resolved_date)) # get unique number of days (for output dimensions)
agent_n <- length(unique(input$Resolved_by)) # get unique number of agents (for output dimensions)

# Parameters used in the output table:
unique_dates <- c(unique(input$Resolved_date))
unique_agents <- c(unique(input$Resolved_by))
unique_types<- c(unique(input$Type))

y = 1

# Loop to output results in the output table:
for (type in unique_types) {
  x = 1
  output <- data.frame(matrix(NA, nrow = agent_n, ncol = days_n), row.names = c(unique(input$Resolved_by)))
  colnames(output) <- c(unique(input$Resolved_date))
  for (agent in unique_agents) {
    for (date in unique_dates) {
      output[x,y] <- filter(input, Type == type) %>% filter(., Resolved_by == agent) %>% filter(., Resolved_date == date) %>% count(.)
      y = y+1
    }
    x = x+1
    y=1
  }
  write.table(output, file=paste("output-",type,".csv", sep=""), sep = ",", quote = FALSE, row.names = TRUE, col.names = NA)
}
}