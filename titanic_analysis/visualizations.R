# Load the required library for this analysis
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load the Titanic data set
titanic_data = read.csv('data/train.csv', header = T)


# Plots here

# Plot the number of survived vs not survived
titanic_data %>% ggplot(aes(Survived, fill=factor(Survived))) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks=c(0, 1)) +
  labs(x='Survived vs Not Survived', 
       y='Number of People in Each Class') +
  scale_fill_discrete(name='Survived vs Not Survived',
                      labels=c('Not Survived', 'Survived'))

# Histogram of different PClass
titanic_data %>% ggplot(aes(Pclass, fill=factor(Pclass))) + 
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks=seq(1, 3, 1)) +
  labs(x='Histogram of Different Pclass', 
       y='Number of Each Class on the Titanic') +
  scale_fill_discrete(name='Pclass',
                      labels=c('Upper Class', 
                               'Middle Class', 
                               'Lower Class'))

# Histogram of different Sex
titanic_data %>% ggplot(aes(factor(Sex), fill=factor(Sex))) + 
  geom_histogram(stat='count') +
  labs(x='Histogram of Different Sex', 
       y='Count of Each Sex on the Titanic') +
  scale_fill_discrete(name='Sex',
                      labels=c('Female', 'Male'))

# Histogram of different Sex
titanic_data %>% mutate(Survived = ifelse(Survived ==  1, 
                                          'Survived', 'Not Survived')) %>% 
  ggplot(aes(factor(Sex), fill=factor(Sex))) + 
  geom_histogram(stat='count') +
  labs(x='Histogram of Different Sex', 
       y='Count of Each Sex on the Titanic') +
  scale_fill_discrete(name='Sex',
                      labels=c('Female', 'Male')) +
  facet_wrap(~Survived)


# Bar plot by age range every 5 years
titanic_data %>% mutate(Age=cut(Age, breaks=seq(0, 90, 5))) %>% 
  group_by(Age) %>% 
  count(Age) %>%
  ggplot(aes(x=Age, y=n, fill=Age)) + 
  geom_bar(stat='identity') + 
  scale_y_continuous(breaks = seq(0, 600, 10)) +
  ylab('Number of People in Each Age Group')


# Plots separate view of Survived vs Not Survived groups
# Show the number of Sibsp in each group
titanic_data %>%
  group_by(SibSp, Survived, Pclass) %>% 
  mutate(Survived = ifelse(Survived ==  1, 'Survived', 'Not Survived')) %>%
  count(Parch) %>%
  ggplot(data=.) + 
  geom_bar(mapping = aes(x=SibSp, y=n, fill=Pclass), stat='identity') +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  ylab('Number of People in Each SibSp Group') +
  facet_wrap(~Survived)

# Plots separate view of Survived vs Not Survived groups
# Show the number of Parch in each group
titanic_data %>%
  group_by(Parch, Survived, Pclass) %>% 
  mutate(Survived = ifelse(Survived ==  1, 'Survived', 'Not Survived')) %>%
  count(Parch) %>%
  ggplot(data=.) + 
  geom_bar(mapping = aes(x=Parch, y=n, fill=Pclass), stat='identity') +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  ylab('Number of People in Each ParCh Group') +
  facet_wrap(~Survived)

# Plots separate view of Survived vs Not Survived groups
# Show the number of Pclass in each group
titanic_data %>%
  group_by(Pclass, Survived) %>% 
  mutate(Survived = ifelse(Survived ==  1, 'Survived', 'Not Survived')) %>%
  count(Parch) %>%
  ggplot(data=.) + 
  geom_bar(mapping = aes(x=Pclass, y=n, fill=Pclass), stat='identity') +
  scale_x_continuous(breaks = seq(0, 6, 1)) +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  ylab('Number of People in Each Pclass Group') +
  facet_wrap(~Survived)

# Plots separate view of Survived vs Not Survived groups
# Show the number of Embarked in each group
titanic_data %>%
  group_by(Embarked, Survived, Pclass) %>% 
  mutate(Survived = ifelse(Survived ==  1, 'Survived', 'Not Survived'),
         Embarked = ifelse(Embarked =='', NA, Embarked)) %>%
  count(Embarked) %>%
  ggplot(data=.) + 
  geom_bar(mapping = aes(x=Embarked, y=n, fill=Pclass), stat='identity') +
  scale_y_continuous(breaks = seq(0, 600, 50)) +
  ylab('Number of People in Each Embarked Group') +
  facet_wrap(~Survived)
