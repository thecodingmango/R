# Load libraries for data cleaning
library(dplyr)
library(tidyverse)
library(caret)

# Load the titanic data set
titanic_data = read.csv('data/train.csv', header = T, sep = ',')

# Quick summary of the data
summary(titanic_data)

# Drop passengerid and cabin column
titanic_data <- select(titanic_data, -c("PassengerId", "Cabin"))

# Function that only keeps the title of the person
keep_title <- function(data){
  
  return(sub('\\..*', '', sub('.*, \\.*', '', data)))
  
}

# Change the name to title of the person
titanic_data$Name <- keep_title(titanic_data$Name)
names(titanic_data)[names(titanic_data) == 'Name'] <- 'Title'

# Encode male as 0, female as 1
titanic_data %>% mutate(Sex = ifelse(Sex == 'female', 1, 0)) -> titanic_data

# Add NA to empty cells
titanic_data$Embarked <- ifelse(titanic_data$Embarked == '', NA, titanic_data$Embarked)

# Set Sex, Embarked, Title as factor
titanic_data$Sex <- as.factor(titanic_data$Sex)
titanic_data$Title <- as.factor(titanic_data$Title)
titanic_data$Embarked <- as.factor(titanic_data$Embarked)

# Create a new dummy variable
dummy <- dummyVars('~ Embarked + Title', data=titanic_data)

# Add the dummy variable into the titanic data set
titanic_data <- cbind(titanic_data, data.frame(predict(dummy, newdata=titanic_data)))

# Drop the Embarked, Title, Ticket, Fare column
titanic_data <- select(titanic_data, -c('Embarked', 'Title', 'Ticket', 'Fare'))

# Drop the Embarked.S and Title.the.Countess Columns
titanic_data <- select(titanic_data, -c('Embarked.S', 'Title.the.Countess'))

# Fill the missing value using the mean
titanic_data$Age <- ifelse(is.na(titanic_data$Age), 
                           mean(titanic_data$Age, na.rm = T),
                           titanic_data$Age)

write.csv(titanic_data, file = 'data/Processed_training_data.csv', row.names = F)
