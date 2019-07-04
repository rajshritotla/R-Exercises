# Part1) Central Limit Theorem (20 points)
# The input data consists of the sequence from 1 to 20 (1:20). Show the following three
# plots in a single row.

data <- seq(1:20)
mean <- mean(data)
sd <- sd(data)

# a) Show the histogram of the densities of this distribution.

hist(data, prob=TRUE, col = "yellow")

# b) Using all samples of this data of size 2, show the histogram of the densities of the
# sample means.

length <- length(data)
sample.size <- 2
xbar2 <- numeric(length)
for(i in 1:length){
  xbar2[i] <- mean(rnorm(sample.size,mean=mean,sd=sd))
}

hist(xbar2, prob=TRUE, xlab = "X",col="red",breaks=3)

# or
# 
# samples <- combn(x,2)
# xbar <- apply(samples, 2, FUN = mean)
# hist(xbar, prob = TRUE, xlim = c(0, 25))


# c) Using all samples of this data of size 5, show the histogram of the densities of the
# sample means.

length <- length(data)
sample.size <- 5
xbar5 <- numeric(length)
for(i in 1:length){
  xbar5[i] <- mean(rnorm(sample.size,mean=mean,sd=sd))
}
hist(xbar5, prob=TRUE, xlab = "X",col="red",breaks=3)


# d) Compare of means and standard deviations of the above three distributions.
print("Orginal Data")
print("Mean:")
mean
print("Std. Dev.")
sd

print("For sample size 2")
print("Mean:")
mean(xbar2)
print("Std. Dev.")
sd(xbar2)

print("For sample size 5")
print("Mean:")
mean(xbar5)
print("Std. Dev.")
sd(xbar5)

plot(c(mean(xbar2), mean, mean(xbar5)),c(2,0,5), main="Mean")
plot(c(sd(xbar2), sd, sd(xbar5)),c(2,0,5), main="Std Dev")


#____________________________________________________________________________________________________

# The data in the file queries.csv contains the number of queries Google has had each day for a one
# year period (365 days). The data file is also available at
# http://kalathur.com/cs544/data/queries.csv. Use this link to read the data using read.csv function
# when submitting the homework.

data <- read.csv("http://kalathur.com/cs544/data/queries.csv")
head(data, n=2)
data = unlist(data$queries)
head(data, n=2)
rescaledata = as.integer(as.numeric(data/100000)) 
head(rescaledata, n=2)

# a) Show the histogram of the distribution of the number of queries. Compute the mean and
# standard deviation of the number of queries Google has had per day.

hist(rescaledata,col="cyan")

print("Mean")
mean <- mean(rescaledata)
mean

print("Std. Dev.")
sd <- sd(rescaledata)
sd

# b) Draw 1000 samples of this data of size 10, show the histogram of the densities of the sample
# means. Compute the mean of the sample means and the standard deviation of the sample means.

sample <- 1000
sample.size <- 10
xbar <- numeric(sample)
for(i in 1:sample){
  xbar[i] <- mean(sample(rescaledata, size= sample.size, replace = TRUE))
  
}
hist(xbar,col="blue",xlim = c(2300,2700),breaks=15)

print("Mean of sample means")
mean10 <- mean(xbar)
mean10

print("Std.Dev. of sample means")
sd10 <- sd(xbar)
sd10

# c) Draw 1000 samples of this data of size 30, show the histogram of the densities of the sample
# means. Compute the mean of the sample means and the standard deviation of the sample means.

sample <- 1000
sample.size <- 30
xbar <- numeric(sample)
for(i in 1:sample){
  xbar[i] <- mean(sample(rescaledata, size= sample.size, replace = TRUE))
}
hist(xbar,col="blue",xlim = c(2300,2700),breaks=15)

print("Mean of sample means")
mean30 <- mean(xbar)
mean30

print("Std.Dev. of sample means")
sd30 <- sd(xbar)
sd30

# d) Compare of means and standard deviations of the above three distributions.
print("Orginal Data")
print("Mean:")
mean
print("Std. Dev.")
sd

print("For sample size 10")
print("Mean:")
mean10
print("Std. Dev.")
sd10

print("For sample size 30")
print("Mean:")
mean30
print("Std. Dev.")
sd30

plot(c(10,0,30),c(mean10,mean,mean30), main="Mean")
plot(c(10,0,30),c(sd10,sd,sd30), main="Std Dev")

#________________________________________________________________________________________________________

# Part3) Central Limit Theorem - Negative Binomial distribution (20 points)

# Suppose the input data follows the negative binomial distribution with the
# parameters size = 5 and prob = 0.5.

size<-5
probality<-0.5

# a) Generate 1000 random numbers from this distribution. Show the barplot
# with the proportions of the distinct values of this distribution.

nums <- rnbinom(1000, size = size, prob =probality)
print("1000 nos are")
nums
# barplot(table(nums),col="green",ylab="frequency",xlab="Numbers")
barplot(prop.table(table(nums)),col="green",ylab="proportion",xlab="Numbers")


# b) With samples sizes of 10, 20, 30, and 40, generate the data for 5000
# samples using the same distribution. Show the histograms of the densities
# of the sample means. Use a 2 x 2 layout.

samples <- 5000
x10 <- numeric(samples)
x20 <- numeric(samples)
x30 <- numeric(samples)
x40 <- numeric(samples)

for(i in 1:samples){
  x10[i]<- mean(rnbinom(10,size = size, prob =probality))
  x20[i]<- mean(rnbinom(20,size = size, prob =probality))
  x30[i]<- mean(rnbinom(30,size = size, prob =probality))
  x40[i]<- mean(rnbinom(40,size = size, prob =probality))
}

#histogram for sample size 10
hist(x10, prob=TRUE, col="red", main="histogram for sample size 10",breaks = 15, xlim=c(0,10))

#histogram for sample size 20
hist(x20, prob=TRUE, col="red", main="histogram for sample size 20",breaks = 15, xlim=c(0,10))

#histogram for sample size 30
hist(x30, prob=TRUE, col="red", main="histogram for sample size 30",breaks = 15, xlim=c(0,10))

#histogram for sample size 40
hist(x40, prob=TRUE, col="red", main="histogram for sample size 40",breaks = 15, xlim=c(0,10))

# c) Compare of means and standard deviations of the data from a) with the
# four sequences generated in b).

#data in a
mean <- mean(nums)
sd <- sd(nums)

#data in b
mean10 <- mean(x10)
sd10 <- sd(x10)
mean20 <- mean(x20)
sd20 <- sd(x20)
mean30 <- mean(x30)
sd30 <- sd(x30)
mean40 <- mean(x40)
sd40 <- sd(x40)

compare.mean<-c(mean, mean10,mean20,mean30,mean40)
as.data.frame(compare.mean)

# mean plot
plot(c(0,10,20,30,40),c(mean, mean10,mean20,mean30,mean40), xlab="Original, Sample size 10,20,30,40") #"Orginal","10","20","30","40"


compare.sd=c(sd, sd10,sd20,sd30,sd40)
as.data.frame(compare.sd)

# sd plot
plot(c(0,10,20,30,40),c(sd, sd10,sd20,sd30,sd40), xlab="Original, Sample size 10,20,30,40") #"Orginal","10","20","30","40"

#_______________________________________________________________________________________________________________________________________________

# Part4) Sampling (40 points)
# Use the MU284 dataset from the sampling package. Use a sample size of
# 20 for each of the following.

library(sampling)
data("MU284")
head(MU284,n=2)
sample.size <- 20
data <- MU284

# a) Show the sample drawn using simple random sampling without
# replacement. Show the frequencies for each region (REG). Show the
# percentages of these with respect to the entire dataset.

s <- srswor(sample.size, nrow(data))
res <- data[s!=0,]
res

a = mean(res$RMT85)

reg = as.data.frame(table(res$REG))
colnames(reg) = c("REG","Frequency")
print("Frequency of each region")
reg 

percent <- as.data.frame(prop.table(table(res$REG)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent

# b) Show the sample drawn using systematic sampling. Show the
# frequencies for each region (REG). Show the percentages of these with
# respect to the entire dataset.

N <- nrow(data)
n <- sample.size
k <- ceiling(N/n)
r <- sample(k,1)
s <- seq(r,by=k,length=n)

res <- data[s!=0,]
#res <- data[s,]
res

b = mean(res$RMT85)

reg <- as.data.frame(table(res$REG))
colnames(reg) <- c("REG","Frequency")
print("Frequency of each region")
reg 

percent <- as.data.frame(prop.table(table(res$REG)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent


# c) Calculate the inclusion probabilities using the S82 variable. Using these
# values, show the sample drawn using systematic sampling. Show the
# frequencies for each region (REG). Show the percentages of these with
# respect to the entire dataset.

pik <- inclusionprobabilities(data$S82,sample.size)
s <- UPsystematic(pik)
res <- data[s!=0,]
res

c <- mean(res$RMT85)

reg <- as.data.frame(table(res$REG))
colnames(reg) <- c("REG","Frequency")
print("Frequency of each region")
reg 

percent <- as.data.frame(prop.table(table(res$REG)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent

# d) Order the data using the REG variable. Draw a stratified sample using
# proportional sizes based on the REG variable. Show the frequencies for
# each region (REG). Show the percentages of these with respect to the
# entire dataset.
x<-order(data$REG)
order.data= data[x,]
#order.data <- data[order(data$REG),]
print("Ordered data")
order.data

freq <- table(order.data$REG)
size <- sample.size * freq / sum(freq)
#size <- as.vector(size)
#size <- size[size != 0]
res <- strata(order.data, stratanames = c("REG"), size = size, method = "srswor", description = TRUE)
res

reg <- as.data.frame(table(res$REG))
colnames(reg) <- c("REG","Frequency")
print("Frequency of each region")
reg 

percent <- as.data.frame(prop.table(table(res$REG)))
percent$Percentage <- percent$Freq*100
print("Percent of each region")
percent

d <- data[res$ID_unit,]
d <- mean(d$RMT85,na.rm=TRUE)

# e) Compare the means of RMT85 variable for these four samples with the
# entire data.
print("Mean for a,b,c,d part")
compare.mean <- c(a,b,c,d)
as.data.frame(compare.mean)

