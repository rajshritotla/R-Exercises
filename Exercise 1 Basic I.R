# The following sample data shows the scores of 10 students in an exam:
#   45, 80, 83, 78, 75, 77, 83, 83, 79, 100

scores <- c(45,80,83,78,75,77,83,83,79,100)

# a) Using indexing, show the expression for accessing the second item and the last but one item.
# The code should work for a vector of any size.
# Sample output for the above input data:
#   [1] 80 79
a.second.lastsecond<-scores[c(2,length(scores)-1)]
a.second.lastsecond

# b) Using comparison operators, write the expression for scores less than the median of the
# data, computed as median(scores).
# Sample output:
#   [1] TRUE FALSE FALSE TRUE TRUE TRUE FALSE FALSE TRUE FALSE
scores.median<-median(scores)
b.comparision.scores<- scores<scores.median
b.comparision.scores

# c) Using logical indexing and the expression from b), return all the scores that are less than the
# median value of the data.
# Sample output:
#   [1] 45 78 75 77 79
c.scores.less.than.median<- scores[c(b.comparision.scores)]
c.scores.less.than.median

# d) Using rep function, create a sequence of alternating TRUE, FALSE values. Using this
# sequence, return the odd indexed values from the scores. The code should work for any size
# input data. You can assume that there are even number of values in scores.
# Sample output:
#   [1] 45 83 75 83 79
dGetOddIndexNo<- function(any.vector){      #used a function to get odd index no for any vector
  length.vector<- length(any.vector)
  true.false.vector<- rep(c("TRUE","FALSE"),length.vector/2)
}


d.true.false.vector<-dGetOddIndexNo(scores)
d.odd.index.value<- scores[c(TRUE,FALSE)]
d.odd.index.value

dd.odd.index.value<- scores(d.true.false.vector)
dd.odd.index.value


# e) Using the paste function with LETTERS, show the code for the following output.
# The code should not hardcode the value 10 for the number of scores.
# Sample output:
#   [1] "A=45" "B=80" "C=83" "D=78" "E=75" "F=77" "G=83" "H=83" "I=79" "J=100"
e.letters.scores<- paste(LETTERS[1:10],scores,sep="=")
e.letters.scores

# f) Create a matrix of size 2 x 5 using the scores data. The first five values belong to the first row
# of the matrix. Assign the result to the variable, scores.matrix, and display the result.
# Sample output:
#   [,1] [,2] [,3] [,4] [,5]
# [1,] 45 80 83 78 75
# [2,] 77 83 83 79 100
f.scores.matrix<- matrix(scores,nrow=2,ncol=5,byrow = TRUE)
f.scores.matrix

# g) Show the code for displaying the first and last columns of the matrix. The code should work
# for any size matrix.
# Sample output:
#   [,1] [,2]
# [1,] 45 75
# [2,] 77 100
g.any.matrix.name=f.scores.matrix
g.matrix.of.first.and.last.col=f.scores.matrix[,c(1,ncol(f.scores.matrix))] #used ncol to get size
g.matrix.of.first.and.last.col

# h) Assign row names for the scores.matrix as Student_1, Student_2,. and column names as
# Quiz_1, Quiz_2 .. The code should work for any size matrix, i.e., for any number of rows and
# any number of columns in the matrix.
# Sample output:
#   Quiz_1 Quiz_2 Quiz_3 Quiz_4 Quiz_5
# Student_1 45 80 83 78 75
# Student_2 77 83 83 79 100
h.named.scores.matrix=f.scores.matrix
dimnames(h.named.scores.matrix)<-(list(c("Student_1","Student_2"),c("Quiz_1","Quiz_2","Quiz_3","Quiz_4","Quiz_5")))
h.named.scores.matrix

#..........................................................................................................................
# Create a data frame, say weather.info, using the column names: Month, Monthly_Average,
# Daily_Max_Average, Daily_Min_Average, Record_High, and Record_Low.

# a) Show the code for creating the above data frame and display the resulting data frame.
month<- c("January","February","March","April","May","June","July","August","September","October","November","December")
monthly_avg<- c(4.7,6.1,12.8,23.9,35.5,45.0,49.1,48.1,41.6,30.2,20.7,10.1)
daily_max_avg<- c(13.6,14.7,20.7,30.4,41.3,50.4,54.1,53.3,47.1,36.4,28.1,18.4)
record_high<- c(48,43,54,60,66,72,71,72,69,62,52,47)
record_low<- c(-47,-46,-38,-20,-2,8,24,20,9,-5,-20,-46)
weather.info<- data.frame(month,monthly_avg,daily_max_avg,record_high,record_low)

a.weather.info<-weather.info
a.weather.info

# b) Show the summary for Monthly_Avg, DailyMax_Avg, DailyMin_Avg, Record_High, and
# Record_Low.
b.summary.monthly_avg=summary(monthly_avg)
b.summary.monthly_avg

b.summary.daily_max_avg=summary(daily_max_avg)
b.summary.daily_max_avg

b.summary.record_high=summary(record_high)
b.summary.record_high

b.summary.record_low=summary(record_low)
b.summary.record_low

# c) Show the data frame sliced using the columns Month, Record_High, and Record_Low.
c.sliced.dataframe=weather.info[c("month","record_high","record_low")]
c.sliced.dataframe

# d) Show the data frame sliced using the first and last row. Do not hard code 12 in the
# expression, i.e., the code should work for a data frame of any size.
d.total.no.of.column=ncol(weather.info)
d.first.and.last.column=weather.info[c(1,d.total.no.of.column),]
d.first.and.last.column

#e) Show all rows of the data frame whose DailyMax_Avg is greater than 50.

#.......tried two approaches
e1.daily_max_avg.greater.than.50=weather.info[weather.info$daily_max_avg>50,]
e1.daily_max_avg.greater.than.50

e2.daily_max_avg.greater.than.50=subset(weather.info,daily_max_avg>50)
e2.daily_max_avg.greater.than.50

# f) Modify the data by adding a new column, Record_Deviation, showing the difference between
# the Record_High and Record_Low. Display the new resulting data frame.
new.weather.info<- weather.info
new.weather.info$record_deviation <- weather.info$record_high-weather.info$record_low
new.weather.info 

