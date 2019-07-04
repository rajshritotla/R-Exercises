library(stats)

# Part1) Binomial distribution (20 points)
# Suppose a pitcher in Baseball has 50% chance of getting a strike-out when
# throwing to a batter. Using the binomial distribution,

# a) Compute and plot the probability distribution for striking out the next 6
# batters.

p <- 0.5
total_players <- 6
choose_players <- 0:6

req_prob <- dbinom(choose_players, size=total_players, prob=p)
req_prob

req_prob_5 <- req_prob


plot(choose_players, req_prob, type = 'h', ylab = "PMF", xlab = "Player No", col='blue')
points(choose_players, req_prob, pch=16, col='blue')
abline(h=0, col='blue')

# b) Plot the CDF for the above

req_cdf <- pbinom(choose_players, size=total_players, prob=p)
req_cdf <- c(0,req_cdf)
req_cdf

plot(choose_players, req_cdf, type = 'h', ylab = "CDF", xlab = "Player No", col='blue')
points(choose_players, req_cdf, pch=16, col='blue')
abline(h=0, col='blue')

#or cdf plot
cdfplot <- stepfun(choose_players,req_cdf)
plot(cdfplot, verticals = FALSE, pch = 16,
     main = "", xlab = "x", ylab = "CDF")

# c) Repeat a) and b) if the pitcher has 70% chance of getting a strike-out.

p <- 0.7

#a
req_prob <- dbinom(choose_players, size=total_players, prob=p)
req_prob

req_prob_7 <- req_prob

plot(choose_players, req_prob, type = 'h', ylab = "PMF", xlab = "Player No", col='blue')
points(choose_players, req_prob, pch=16, col='blue')
abline(h=0, col='blue')

#b
req_cdf <- pbinom(choose_players, size=total_players, prob=p)
req_cdf

plot(choose_players, req_cdf, type = 'h', ylab = "CMF", xlab = "Player No", col='blue')
points(choose_players, req_cdf, pch=16, col='blue')
abline(h=0, col='blue')

# d) Repeat a) and b) if the pitcher has 30% chance of getting a strike-out.

p <- 0.3

#a
req_prob <- dbinom(choose_players, size=total_players, prob=p)
req_prob

req_prob_3 <- req_prob

plot(choose_players, req_prob, type = 'h', ylab = "PMF", xlab = "Player No", col='green')
points(choose_players, req_prob, pch=16, col='green')
abline(h=0, col='green')

#b
req_cdf <- pbinom(choose_players, size=total_players, prob=p)
req_cdf

plot(choose_players, req_cdf, type = 'h', ylab = "CMF", xlab = "Player No", col='green')
points(choose_players, req_cdf, pch=16, col='green')
abline(h=0, col='green')


# e) Infer from the shape of the distributions.

plot(choose_players, req_prob_3, type = 'h', col='green', ylab = "Probability", xlab = "Player No (when p=0.3)")
points(choose_players,  col='green', req_prob_3, pch=16)
abline(h=0, col='green')
print("Probability is 30% then plot is right skewed, probability that baller will strikeout(success) is less than not striking out(failure) [success<failure]")

plot(choose_players, req_prob_5, type = 'h', col='green', ylab = "Probability", xlab = "Player No (when p=0.5)")
points(choose_players,  col='green',req_prob_5, pch=16)
abline(h=0, col='green')
print("Probability is 50% then plot is symmetric, probability that baller will strikeout(success) is equal to not striking out(failure) [success=failure]")

plot(choose_players, req_prob_7, type = 'h', col='green', ylab = "Probability", xlab = "Player No (when p=0.7)")
points(choose_players, col='green', req_prob_7,pch=16)
abline(h=0, col='green')
print("Probability is 70% then plot is left skewed, probability that baller will strikeout(success) is more than not striking out(failure) [success>failure]")



#______________________________________________________________________________________________________________________________
# Part 2
# Suppose that 80% of the flights arrive on time. Using the binomial
# distribution,

p <- 0.8

# a) What is the probability that four flights will arrive on time in the next 10
# flights?

total_flights <- 10
choose_flights <- 4
req_prob <- dbinom(choose_flights, size=total_flights, prob=p)
req_prob

#   b) What is the probability that four or fewer flights will arrive on time in the
# next 10 flights?
cmf_req_prob <- pbinom(4, size=total_flights, prob=p)
cmf_req_prob

#or
choose_flights <- 0:4     #because possible no flights on time so start with 0
req_prob_4orless <- dbinom(choose_flights, size=total_flights, prob=p)
req_prob_4orless
req_prob <- sum(req_prob_4orless)
req_prob


# c) Compute the probability distribution for flight arriving in time for the next
# 10 flights.
choose_flights <- 0:10    
req_prob <- dbinom(choose_flights, size=total_flights, prob=p)
req_prob

# d) Show the PMF and the CDF for the next 10 flights.

choose_flights <- 0:10    
req_pmf <- dbinom(choose_flights, size=total_flights, prob=p)
req_pmf
plot(choose_flights, req_pmf, type = 'h', ylab = "PMF", xlab = "Flights", col="darkblue")
points(choose_flights, req_pmf, pch=16, col="darkblue")
abline(h=0, col="darkblue")

choose_flights <- 0:10    
req_cmf <- pbinom(choose_flights, size=total_flights, prob=p)
req_cmf
plot(choose_flights, req_cmf, type = 'h', ylab = "CMF", xlab = "Flights", col="darkblue")
points(choose_flights, req_cmf, pch=16, col="darkblue")
abline(h=0, col="darkblue")


#______________________________________________________________________________________________________________________________
# 
# Part3) Poisson distribution (15 points)
# Suppose that on average 10 cars drive up to the teller window at your bank
# between 3 PM and 4 PM and the random variable has a Poisson
# distribution. During this time period,

lambda <- 10

#   a) What is the probability of serving exactly 3 cars?
dpois(3, lambda = lambda)

#   b) What is the probability of serving at least 3 cars?
ppois(2, lambda = lambda, lower.tail = FALSE)

#   c) What is the probability of serving between 2 and 5 cars (inclusive)?
ppois(5, lambda = lambda)-ppois(1, lambda = lambda)
#or diff(ppois(c(1, 5), lambda = lambda))

#   d) Calculate and plot the PMF for the first 20 cars.
pmf <- dpois(0:20, lambda = lambda)

plot(0:20,pmf, type = 'h',col='red', xlab = 'Cars',ylab = 'PMF')
points(0:20,pmf,pch=16,col='red')
abline(h=0,col='red')


#______________________________________________________________________________________________________________________________
# 
# Part4) Uniform distribution (15 points)
# Suppose that your exams are graded using a uniform distribution between
# 60 and 100 (both inclusive).

min = 60
max = 100

# a) What is the probability of scoring 

#i) 60?
dunif(60, min = min, max = max)

#ii) 80? 
dunif(80, min = min, max = max)

#iii) 100?
dunif(100, min = min, max = max)

#   b) What is the mean and standard deviation of this distribution?
mean= (min+max)/2
mean

variance= (max-min)*(max-min)/12
variance

#   c) What is the probability of getting a score of at most 70?
punif(70, min = min, max = max)

#   d) What is the probability of getting a score greater than 80 (use the
#                                                                  lower.tail option)?
punif(80, min = min, max = max, lower.tail = FALSE)

#   e) What is the probability of getting a score between 90 and 100 (both
#                                                                     inclusive)?

cmf_100 <- punif(100, min = min, max = max)

cmf_90 <- punif(90, min = min, max = max)

req_prob <- diff(punif(c(90, 100), min = min, max = max))
req_prob

#__________________________________________________________________________________________
# Part5) Normal distribution (20 points)
#  
# Suppose that visitors at a theme park spend an average of $100 on
# souvenirs. Assume that the money spent is normally distributed with a
# standard deviation of $10.

meann <- 100
std_dev <- 10

# a) Show the PDF plot of this distribution covering the three standard
# deviations on either side of the mean.

less_sd <- meann - 3*std_dev
more_sd <- meann + 3*std_dev

x<-dnorm(less_sd:more_sd, mean = meann, sd = std_dev)  

plot(less_sd:more_sd,x, type = 'h',xlab = "Standard Deviation", ylab = "Mean in 3 SD", col = "red", main="PDF in 3 Std. Dev.")
points(less_sd:more_sd, x,pch=16, col = "red")
abline(h=0, col='red')

# b) What is the probability that a randomly selected visitor will spend more
# than $120?
morethan120<- pnorm(120, mean = meann, sd = std_dev, lower.tail = FALSE)  
morethan120

# c) What is the probability that a randomly selected visitor will spend
# between $80 and $90 (inclusive)?

for80 <- pnorm(80, mean = meann, sd = std_dev)
for90 <- pnorm(90, mean = meann, sd = std_dev)

req_prob <- for90-for80
req_prob

# d) What are the probabilities of spending within one standard deviation, two
# standard deviations, and three standard deviations, respectively?

less_1sd <- meann - std_dev
more_1sd <- meann + std_dev
within1sd <- pnorm(more_1sd, mean = meann, sd = std_dev)-pnorm(less_1sd, mean = meann, sd = std_dev)
within1sd

less_2sd <- meann - 2*std_dev
more_2sd <- meann + 2*std_dev
within2sd <- pnorm(more_2sd, mean = meann, sd = std_dev)-pnorm(less_2sd, mean = meann, sd = std_dev)    
within2sd

less_3sd <- meann - 3*std_dev
more_3sd <- meann + 3*std_dev
within3sd <- pnorm(more_3sd, mean = meann, sd = std_dev)-pnorm(less_3sd, mean = meann, sd = std_dev)    
within3sd

# e) Between what two values will the middle 90% of the money spent will
# fall?

value1<- qnorm(0.05, mean = meann, sd = std_dev)
value2<- qnorm(0.95, mean = meann, sd = std_dev)

#value1 <- meann - 1.645*std_dev
#value2 <- meann + 1.645*std_dev
print("Required values")
value1
value2
print("proof")
proof <- pnorm(value2, mean = meann, sd = std_dev)-pnorm(value1, mean = meann, sd = std_dev)    
proof

# f) Show a plot for 10,000 visitors using the above distribution.

y<- rnorm(10000, mean=meann, sd= std_dev)
y<- round(y)
y<- table(y)
y
plot(y, type = 'h', xlab = "For 10,000 visitors", ylab="Frequency",col="cyan",main="Plot for 10,000 visitors")
# points(y, pch=16,col="cyan")
abline(h=0,col="cyan")



#__________________________________________________________________________________________
# Part6) Exponential distribution (15 points)
# Suppose your cell phone provider's customer support receives calls at the
# rate of 18 per hour.

rate =18

# a) What is the probability that the next call will arrive within 2 minutes?

pexp(2/60,rate=rate)

# b) What is the probability that the next call will arrive within 5 minutes?

pexp(5/60,rate=rate)

# c) What is the probability that the next call will arrive between 2 minutes
# and 5 minutes (both inclusive)?

pexp(5/60,rate=rate)-pexp(2/60,rate=rate)

# d) Show the CDF of this distribution.

seq_d <- seq(0 , 1, by = 1/60)
cdf <- pexp(seq_d, rate = rate)
plot(seq_d, cdf , type = 'l',col="orange", xlab = 'Time', ylab = 'CDF', main = "CDF of the distribution")

