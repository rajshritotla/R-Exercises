# Part1) Strings (60 points)
# Use the stringr functions for the following:
#   Initialize the vector of words from Lincoln's Gettysburg address with the
# following code:
#   file <- "http://kalathur.com/cs544/data/lincoln.txt"
# words <- scan(file, what=character())

library(stringr)
file <- "http://kalathur.com/cs544/data/lincoln.txt"
words <- scan(file, what=character())
head(words)


# a) Detect and show all the words that have a punctuation symbol.
words[str_detect(words,"[:punct:]")]

# b) Replace all the punctuations in the corresponding words with an empty
# string. Make this the new words data.
#words <- str_remove_all(words, "[:punct:]")

words <- gsub(pattern = "[[:punct:]]", replacement = " ", words)
words    #NEW WORD DATA

# c) Show the frequencies of the word lengths in the above data. Plot the
# distribution of these frequencies.

show_freq <- as.data.frame(table(str_length(words)))
names(show_freq) <- c("Strings Length","Frequency")
show_freq

barplot(table(str_length(words)), col="red", xlab="length", ylab="frequency", main="Lengths Frequency Graph")

# d) What are the words with the longest length?
max_len <- max(str_length(words))
print("Maximum Length Words")
words[str_length(words)==max_len]

#   e) Show all the words that start with the letter p.
words[str_detect(words,"^p")]

# f) Show all the words that end with the letter r.
words[str_detect(words,"r$")]

# g) Show all the words that start with the letter p and end with the letter r.
pattern1 <- "^p[A-Za-z]*r$"
pattern2 <- "^p\\w*r$"

words[str_detect(words, pattern1)]
words[str_detect(words, pattern2)]

#___________________________________________________________________________________________________________________________________________
# 
# Download the following csv file,
# http://people.bu.edu/kalathur/usa_daily_avg_temps.csv
# locally first and use read.csv to load the data into a data frame.


install.packages("tidyverse")
library(tidyverse)

#install.packages("ggplot2")
#library(ggplot2)

data <- read.csv(file="E:/Practice Projects/R & Analytics Foundation/CS544_HW6_totla/usa_daily_avg_temps.csv", stringsAsFactors = FALSE)
head(data)
data_df = as.data.frame(data)
head(data_df)

# a) Convert the data frame into a tibble and assign it to the variable
# usaDailyTemps.
usaDailyTemps = as.tibble(data_df)
usaDailyTemps

# b) What are the maximum temperatures recorded for each year? Show the
# values and also the appropriate plot for the results.
max_temp_yearwise = usaDailyTemps %>% group_by(year) %>% summarise(max(avgtemp))
max_temp_yearwise
plot(max_temp_yearwise, col="darkblue", xlab="Year", ylab="Maximuum Average Temp", main="Max Temp Yearly")

# c) What are the maximum temperatures recorded for each state? Show the
# values and also the appropriate plot for the results.
max_temp_state = usaDailyTemps %>% group_by(state) %>% summarise(max(avgtemp))
max_temp_state
# barplot(table(max_temp_state$state,max_temp_state$`max(avgtemp)`), col="darkblue", xlab="State", ylab="Maximuum Average Temp", main="Max Temp acc to State")

library(ggplot2)
ggplot(max_temp_state, aes(state)) + geom_bar(fill = "#0073C2FF")
# in plot x lab names are overlapping

# d) Filter the Boston data and assign it to the variable bostonDailyTemps.
bostonDailyTemps = filter(usaDailyTemps, city=="Boston")
bostonDailyTemps

# e) What are the average monthly temperatures for Boston? Show the
# values and also the appropriate plot for the results.
avgtemp_boston = bostonDailyTemps %>% group_by(month) %>% summarise(mean(avgtemp))
avgtemp_boston
plot(avgtemp_boston, col="darkblue", xlab="Month", ylab="Average Temp", main="For Boston")

