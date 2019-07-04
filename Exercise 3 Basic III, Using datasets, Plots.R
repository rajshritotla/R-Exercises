library(UsingR)

# part1.................................................................
# Use the primes (UsingR) dataset. Use the diff function to compute the differences
# between successive primes. Show the frequencies of these differences. Show the
# barplot of these frequencies.


head(primes,n=2)

# using diff func
cosecutive_difference <- diff(primes)
freq_of_diff <- table(cosecutive_difference)
freq_of_diff     #freq

# barplot of freq
barplot(freq_of_diff, ylim=c(0,80), col="cyan", xlab="Consecutive Differences",ylab="Frequency")



# part2.................................................................
# Use the coins (UsingR) dataset. Do not use explicit loops for any calculations. Do not
# hard code the denominations in the solution. The solution should work for any
# denominations.

attach(coins)
head(coins,n=2)

# a) How many coins are there of each denomination?
colSums(table(coins))

#or

x <- as.data.frame(table(value))
names(x) <- c("denomination","count")
no_of_coin_of_each_den <- x
no_of_coin_of_each_den

# b) What is the total value of the coins for each denomination?
# to count each demoniation value
a <- as.vector(x$denomination)
b <- as.vector(x$count)

a <- as.double(a)
b <- as.double(b)
x$total_eachDen <- a*b
x

# c) What is the total value of all the coins?
cat("Total value is ",sum(x$total_eachDen))

# d) Show the barplot for the number of coins by year.
x <- table(coins)

barplot(t(x),beside=TRUE,col=rainbow(4), legend.text = TRUE, args.legend = list(x="top"),main="Coins",xlab = "denomination",ylab="count")

#or
#par(mar = rep(2, 4))
# for (i in 1:nrow(x)){       #to get by year
#   barplot(x[i,],col=c("green","red","blue","yellow"),xlab = "denomination",ylab="count",legend.text =TRUE)
# }

detach()  



# part3.................................................................
# Use the south (UsingR) dataset. 

head(south)

# a) Show the stem plot of the data. What do you interpret from this plot?
stem(south)
cat("It could be infered that maximum data lie between 10-14, max value in data is 33, only 3 values are above 20")

# b) Show the five number summary of the data. Calculate the lower and upper ends of
# the outlier ranges. What are the outliers in the data?
f<-fivenum(south)

upper_outlier <- f[2]+1.5*(f[4]-f[2])
lower_outlier <- f[2]-1.5*(f[4]-f[2])

f   #fivenum
upper_outlier
lower_outlier

# c) Show the horizontal boxplot of the data along with the appropriate labels on the plot.
boxplot(south, horizontal = TRUE, xaxt='n', xlab="south",col="cyan")
axis(side=1, at=fivenum(south),labels=TRUE)
text(f, rep(1.2,5),srt=90, adj=0,labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))



#part4.................................................................
# Use the pi2000 (UsingR) dataset.

head(pi2000)

# a) How many times each of the digits 0 to 9 occur in this dataset?
pi=table(pi2000)
pi #second row shows no of occurences

# b) Show the percentages of their frequencies.
options(digits = 2)
print("Percentages are")
prop.table(pi)*100

# c) Show a barplot of the above frequencies.
barplot(pi,ylim=c(0,220),col="cyan",xlab="Number(south)",ylab="Frequency")

# d) Show the histogram of the pi2000 data. Why is the first bar different from the rest?
hist(pi2000,ylim=c(0,400),col="cyan",xlab="Number(south)",ylab="Frequency")

cat("Histogram show the data for intervals while barplot show data of category. In 
    our question for the interval 0-1 there is lot of data as compared to other intervals
    that's why in histogram bar first bar or interval 0-1 is different from others")

# Using the breaks option, make this histogram similar to the barplot in c)

# using breaks
hist(pi2000, breaks=18, ylim=c(0,220),col="cyan",xlab="Number(south)",ylab="Frequency")



# part5.................................................................
# Suppose that a football (NFL), basketball (NBA), and hockey (NHL) games are being
# shown at the same time. Consider the two-way summarized data shown below showing
# the preferences of men and women what sport they wish to watch.

# sport<- rbind(c(25,10,15),c(20,40,30))

# a) Using cbind, create the matrix for the above data.
sport<- cbind(c(25,20),c(10,40),c(15,30))

# b) Set the row names for the data.
rownames(sport) <- c("Men","Women")

# c) Set the column names for the data.
colnames(sport) <- c("NFA","NBL","NHL")

# d) Now, add the dimension variables Gender and Sport to the data.
tmp1 <- c("NFA","NBL","NHL")
tmp2 <- c("Men","women")
dimnames(sport) <- list(Gender=tmp2,Sport=tmp1)

sport

# e) Show the marginal distributions for the Gender and the Sport.
margin.table(sport,1) #for Gender
margin.table(sport,2) #for Sport 

# f) Show the result of adding margins to the data.
addmargins(sport)

# g) Show the proportional data separately for Gender and Sport. Interpret the results.
options(digits = 2)

#for Gender
prop.table(sport,1)
print("By results we can see that 50% of Men likes NFA while it is least among women 22%")
print("Maximum women 44% likes NBL while NBL is least among Men 20%")

#for Sport
prop.table(sport,2)
print("By results we can interpret NFA will be most people like to watch with Men chances higher than Women")
print("NBL got least chances to watch with greater chances of being watched by women")

# h) Using appropriate colors, show the mosaic plot for the data. Also show the barplot for
# Gender and Sport separately with the bars side by side. Add legend to the plots.
mosaicplot(sport,col=rainbow(3))

#gender
barplot(t(sport),beside = TRUE,col = c("purple","orange","green"),xlab="Men & Women",ylab="Freuency", legend.text = TRUE,main="M W plot to watch Sport")


#sport
barplot(sport,beside = TRUE,col = c("blue","pink"),xlab="Sport",ylab="Freuency", legend.text = TRUE,main="Sport plot for watched by M W")




#part6.................................................................
# Use the midsize (UsingR) dataset.
# a) Show the pair wise plots for all the variables.
# b) Provide at least 4 interpretations of the results.

head(midsize,n=3)

plot(midsize, main="Overview")

attach(midsize)

#INFERENCE SHOWN IN GRAPHS
#1st Infer
plot(Year,Accord, main="Year-Accord\nGraph could be a st.line except in 1994-98\n Sales of accord increased proportionally every year")
#2nd Infer
plot(Year,Camry, main="Year-Camry\nGraph looks like e^x\nCampry sales increased exponentially within years")
#3rd Infer
plot(Year,Taurus, main="Year-Taurus\nGraph looks like a parbola\nTaurus sales increased slowly with year with a sudden gain in 2002-04")
#4th Infer
plot(Taurus,Accord, main="Taurus-Accord\nOverall Sales(Taurus)>Sales(Accord)")
#5th Infer
plot(Taurus,Camry, main="Taurus-Camry\nOverall Sales(Taurus)>Sales(Camry)")
#6th Infer
plot(Camry,Accord, main="Camry-Accord\nOverall Sales(Camry)>Sales(Accord)")
#7th Infer
print("Could be infered")
print("Sales(Taurus>Camry>Accord)")

detach()


#part7.................................................................
# Use the MLBattend (UsingR) dataset.

attach(MLBattend)
head(MLBattend,n=2)

# a) Extract the wins for the teams BAL, BOS, DET, LA, PHI into the respective vectors.
BAL_win <- subset(wins,franchise=="BAL")
BAL_win

BOS_win <- subset(wins,franchise=="BOS")
BOS_win

DET_win <- subset(wins,franchise=="DET")
DET_win

LA_win <- subset(wins,franchise=="LA")
LA_win

PHI_win <- subset(wins,franchise=="PHI")
PHI_win

# b) Create a data frame of five columns using these vectors. Use the team names for the
# columns
win <- data.frame(BAL=BAL_win,BOS=BOS_win,DET=DET_win,LA=LA_win,PHI=PHI_win)
head(win,n=3)

# c) Show the boxplot of the data frame.
boxplot(win, horizontal = TRUE, xlab="No of WINS", col=rainbow(5))

# for(i in 1:ncol(win)){
#   boxplot(win[,i], horizontal = TRUE, xaxt='n', xlab="WIN")
#   axis(side=1, at=fivenum(win[,i]),labels=TRUE)
#   text(fivenum(win[,i]), rep(1.2,5),srt=90, adj=0,labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))
# }

# d) Provide at least 5 interpretations of the results.
print("INFERENCES are: \n")
print("1. Except PHI rest of the teams average win lies in range 80-90 \n")
print("2. This means except PHI all teams have won more than 82 matches \n")
print("3. BOS & LA have lower outliers, their no of wins in that year were exceptionally lower than normal \n")
print("4. BOS team has least variations(range) in no of wins among all [means they were consinstent] could be infered by box size \n")
print("5. BAL team has maximum  variations(range) in no of wins among all, could be infered by box size \n")




#part 8.................................................................
# Initialize the House and Senate data as shown below:
#   house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
# senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)
# Provide the simplest R code for the following:

house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)

head(house,n=2)
head(senate,n=2)

#   a) Show how many senators and house members are there by party lines?
print("for house")
table(house$Party)

print("for senator")
table(senate$Party)

#   b) Show the top 10 states in decreasing order by the number of house members in that
# state?
b= table(house$State)
head(b[order(b,decreasing = TRUE)], n=10)

#   c) Use a box plot on the number of house members per state and determine which
# states are outliers?
boxplot(as.vector(b), horizontal = TRUE, xaxt="n", xlab="No of House Members in state",col="pink")
axis(side=1, at=fivenum(b),labels=TRUE)
text(fivenum(b), rep(1.2,5),srt=90, adj=0,labels=c("Min","Lower Hinge","Median","Upper Hinge","Max"))

state_which_are_outliers = boxplot(b, plot=FALSE)$out
state_which_are_outliers

#   d) What is the average number of years served by party line in the house and senate
# respectively?
d <- table(house$Party)
average_for_house_party_line <- mean(d)

average_for_house_republican<- mean(subset(house$Years_in_office, house$Party=="Republican"))
average_for_house_democratic<- mean(subset(house$Years_in_office, house$Party=="Democratic"))

d <- table(senate$Party)
average_for_senate_party_line <- mean(d)

average_for_senate_republican <- mean(subset(senate$Years_in_office, senate$Party=="Republican"))
average_for_senate_democratic <- mean(subset(senate$Years_in_office, senate$Party=="Democratic"))
average_for_senate_independent <- mean(subset(senate$Years_in_office, senate$Party=="Independent"))

print("Averages for each party line are (read variable name)")
average_for_house_republican
average_for_house_democratic
average_for_senate_republican
average_for_senate_democratic
average_for_senate_independent

print("Averages for overall party line are (read variable name)")
average_for_house_party_line
average_for_senate_party_line

