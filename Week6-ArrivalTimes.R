
# Load required packages
library(knitr)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)

# Read in CSV file from GitHub
GitHubFile <- getURL("https://raw.githubusercontent.com/jillenergy/Week6/master/ArrivalTimes.csv")
flights_raw <- data.frame(read.csv(text = GitHubFile, header = T, check.names=FALSE))

# Check the raw table imported
flights_raw

# Replace period with space in the column headers
names(flights_raw) <- gsub("\\.", " ", names(flights_raw)) 
flights_raw

# Use the Gather function to transform the table into a more usable format
flights_tidy <- gather(flights_raw, "City", "TotalFlights", "Los Angeles":"Seattle") %>% 
  arrange(Airline)
flights_tidy

# Reshape the data looking at the number of flights that were on time or delayed to set-up for analysis
flights_shaped <- flights_tidy %>% 
  spread("Status", "TotalFlights",fill=NA)
colnames(flights_shaped) <- c("Airline", "City", "Delayed", "OnTime")
flights_shaped

# Add a column for Total Flights by Airline and City
flights_shaped_tot <- flights_shaped %>% 
  mutate(TotalFlights = Delayed + OnTime)
flights_shaped_tot

# Add a column for Percentage of On Time
flights_shaped_perc <- flights_shaped_tot %>% 
  mutate(PercentOnTime = OnTime / TotalFlights)
flights_shaped_perc

# Round to shorten the Percentage on Time calculation two two digits
flights_shaped_perc[,'PercentOnTime']=format(round(flights_shaped_perc[,'PercentOnTime'],2),nsmall=2)
datatable(flights_shaped_perc)

# Plot the percentage of flights on time for each city comparing airlines
flights_shaped_perc %>%  
  ggplot( aes(x=City, y=PercentOnTime, fill=Airline)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=PercentOnTime), vjust=.5, hjust=1,position= position_dodge(width=0.9)) +
  ggtitle("Percentage of Flights On Time by Airline by City") +
  xlab("City") + ylab("Percentage of Flights On Time") +
  coord_flip() 
