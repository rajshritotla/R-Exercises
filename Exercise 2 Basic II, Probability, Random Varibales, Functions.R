#1........................
# Probability 
# a) From the Bayes' rule example given in Section 3.10, compute the probabilities that a
# randomly selected non-smoker i) has lung disease and ii) does not have lung disease. Show the
# calculations without using R. Then, verify with the bayes function provided in the code samples.
# b) Suppose that in a particular state, among the registered voters, 40% are democrats, 50 %
#   are republicans, and the rest are independents. Suppose that a ballot question is whether to
# impose sales tax on internet purchases or not. Suppose that 70% of democrats, 40% of
# republicans, and 20% of independents favor the sales tax. If a person is chosen at random that
# favors the sales tax, what is the probability that the person is i) a democrat? ii) a republican, iii)
# an independent. Show the solutions with the calculations without using R. Then, verify with the
# bayes function provided in the code samples.


bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators))
}
prior <- c(0.07,0.93)
like <- c(0.1, 0.75)
a1<-bayes(prior, like)
a1

prior <- c(0.4, 0.5, 0.1)
like <- c(0.7, 0.4, 0.2)
a2<-bayes(prior, like)
a2


#2..............................
# Part2) Random Variables 
# a) Consider the experiment of rolling a pair of dice. Using R, show how would you define a
# random variable for the absolute value of the difference of the two rolls, using a user-defined
# function.

diespace<-rolldie(2,makespace = TRUE)
diespace

createRandomVariable <- function(x){
  return (diff(x))
}
X<-addrv(diespace,FUN=createRandomVariable,invars=c("X1","X2"),name="requiredDiff")
X

absoluteDifference<-abs(X$requiredDiff)
a2<-absoluteDifference
a2

# b) Using the above result, what is the probability that the two rolls differ by exactly 2? What is
# the probability that the two rolls differ by at most 2? What is the probability that the two rolls
# differ by at least 3? Use the Prob function as shown in the code samples.
probDifferByExactly2 <- Prob(X, absoluteDifference == 2)
b2i<-probDifferByExactly2
b2i

probDifferAtMost2 <- Prob(X, absoluteDifference <= 2)
b2ii<-probDifferAtMost2
b2ii

probDifferAtLeast3 <- Prob(X, absoluteDifference >= 3)
b2iii<-probDifferAtLeast3
b2iii


# c) Show the marginal distribution of the above random variable (using R).
result_marginalDist <- marginal(X, vars="requiredDiff")
c2<-result_marginalDist
c2

checkEvenOdd <- function(x){
  
  if((sum(x) %% 2==0)){
    return (TRUE)
  }
  else{
    return (FALSE)
  } 
  
}


# d) Using R, add another random variable to the above probability space using a user defined
# function. The random variable is TRUE if the sum of the two rolls is even, and FALSE otherwise.
# What is the probability that the sum of the two rolls is even? Show also the marginal distribution
# for this random variable.
X<-addrv(X,FUN=checkEvenOdd,invars=c("X1","X2"),name="evenOdd")
X

sumOfTwoRollsIsEven<-Prob(X,evenOdd=="TRUE")
d2i<-sumOfTwoRollsIsEven
d2i

result_marginalDist <- marginal(X, vars="evenOdd")
d2ii<-result_marginalDist
d2ii

#3...........................................
# Part3) Functions 
# Using a for loop, write your own R function, evensum(data), that returns the sum of all the even
# values in the given numeric data vector.
# Now, without using any loop, write your own R function, evensum2(data), that returns the sum
# of all the even values in the given numeric data vector.
# Test both functions with sample data.

totalofeven=0;
evensum <- function(inputData){
  for(num in inputData){
    if(num %% 2 == 0){
      totalofeven=totalofeven+num
    }
  }
  print(totalofeven)
}

evensum2<- function(inputData){
  i=inputData%%2==0
  print( sum(inputData[i]))
}

evensum(c(15,20,25,30,35))
evensum(1:10)
evensum(seq(1,15,by = 2))

evensum2(c(15,20,25,30,35))
evensum2(1:10)
evensum2(seq(1,15,by = 2))


# 4...................................................
# Initialize the Dow Jones Industrials daily closing data as shown below:
#   dow <- read.csv('http://kalathur.com/dow.csv', stringsAsFactors = FALSE)
# Provide the simplest R code and output for all of the following. The code should work for any
# given data.
# a) Use the diff function to calculate the differences between consecutive values.
# Insert the value 0 at the beginning of these differences. Add this result as the DIFFS column of
# the data frame.
# b) How many days did the Dow close higher than its previous day value? How many days did
# the Dow close lower than its previous day value?
# c) Show the subset of the data where there was a gain of at least 400 points from its previous
# day value.
# d) Provide the solution to compute the longest gaining streak of at least 100 points in the data.
# Show the data for that longest gaining streak. Hint: Use the rle function provided by R.


dow <- read.csv('http://kalathur.com/dow.csv', stringsAsFactors = FALSE)

consecutiveDiffs = diff(dow$VALUE)
newColumn <- c(0,consecutiveDiffs)
dow$DIFFS <- newColumn
a4<-dow
a4

higher_values <- dow[dow['DIFFS']>0,]
lower_values <- dow[dow['DIFFS']<0,]
days_higher_values <- nrow(higher_values)
days_lower_values <-nrow(lower_values)
nrow(dow)
b4i<- days_higher_values
b4ii<- days_lower_values
b4i
b4ii

c4<-subset(dow,DIFFS >= 400)
c4

dow$STREAK <- "N"
count = 0 
max = 0

for (i in 1:length(dow$DIFFS)) {
  #when gain of less than 100 points don't count
  if (dow$DIFFS[i] < 100) { 
    count = 0
  }
  
  #when gain of atleast 100 points increment the count
  if (dow$DIFFS[i] >= 100) {
    count = count + 1
    if (count > max) {
      max = count
      i_dx = i
    }
  }
}

for (i in (i_dx - max + 1):i_dx) {
  dow$STREAK[i] <- "Y"
}

d4<-dow[(i_dx-max+1):i_dx,] #longest gaining streak
d4

