library(rafalib)
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
View(dat)
##Getting Started Exercises #1
colnames(dat[2])
##Getting Started Exercises #2
dat[12,2]
##Getting Started Exercises #3
a <- dat$Bodyweight
a[11]
##Getting Started Exercises #4
length(a)
##Getting Started Exercises #5
hf.bwt <- filter(dat , Diet == "hf" ) %>% select(Bodyweight) %>% unlist
mean(hf.bwt)
##Getting Started Exercises #6
set.seed(1)
sample(hf.bwt , 1)
############################################################################
dat <- read.csv("femaleMiceWeights.csv")
controls <- filter(dat , Diet == "chow") %>% select(Bodyweight) %>% unlist

##############################################################################
##dplyr Exercises #1
dat1 <- read.csv("msleep_ggplot2.csv")
View(dat1)
class(dat1)
##dplyr Exercises #2
primates <- filter(dat1 , order == "Primates") 
nrow(primates)
##dplyr Exercises #3
class(primates)
##dplyr Exercises #4
primates <- filter(dat1 , order == "Primates") %>% select(sleep_total)
class(primates)
##dplyr Exercises #5
primates <- filter(dat1 , order == "Primates") %>% select(sleep_total) %>% unlist
mean(primates)
##dplyr Exercises #6(I failed!!)
filter(dat1 , order=="Primates") %>% summarize( mean( sleep_total) )

##################################################################################
## Exploratory data analysis
library(UsingR)
View(father.son)
x <- father.son$fheight  ##we are trying to figure out how the data look like
##-----------------------------------------
sample(x,20)
rount(sample(x,20),1) ## still we have no clue
##--------------------------------------------------------------------------
hist(x , main = "Histogram of Father's Height" , xlab = "Height in inches") ## Now we can say that the data look's like Normal Distribution
##--------------------------------------------------------------------------
##ECDF(better than histogram)
xs <- seq(floor(min(x)) , ceiling(max(x)) , 0.1)
ecdf(x)
ecdf(xs)
ecdf(x)(xs)
plot(xs , ecdf(x)(xs) , type = "l" , xlab = "Height on inches" , ylab = "F(x)", main = "ECDF")
##Here we can see 80-85% of the people are less than 70 inches of height

##Normalilty check(manually)
mean(x)
sd(x)

mean(x > 70) ##Exact approximation of father that are more than 70 inches of height
1 - pnorm(70 , mean(x) , sd(x)) ##Normal approximation of father that are above 70 inches of height 

mean(x <= 70)
pnorm(70 , mean(x) , sd(x)) ##If this two aproximation gives us almost same result 
  # every time than we suspect Normal distribution is very good approximation for  
  # this data
mean(x <= 69)
pnorm(69 , mean(x) , sd(x))

##Normality check(QQ plot)
ps <- seq(0.01 , 0.99 ,  0.01)  ##1% to 99%
qs <- quantile(x , ps)  ##I'm going to compute from my data what those percentiles are.
normalqs <- qnorm(ps , mean(x) , sd(x))  ##We can compute the same percentiles for the normal distribution.
plot(normalqs , qs ,xlab = "Normal percentiles" , ylab = "Height percentiles" ) ##Now we can compare them and see how similar they are.
                      # So we're going to just plot them against each other.
                      ## This is our QQ plot
abline(0,1) ## identity line(intercept = 0 , slope = 1)

##QQplot(using function)
qqnorm(x)
qqline(x)
###################################################################################
load("skew.RData")
dim(dat)
View(dat)
## QQ-plot Exercises #1
mypar(3,3)
for(i in 1:9){
  qqnorm(dat[,i])
}
hist(dat[,4])
## QQ-plot Exercises #2
hist(dat[,9])

##BOX-PLOT
# When data is not normally distributed, than the mean and the standard deviation 
# aren't necessarily good summaries.Then we can use boxplot.
library(UsingR)
View(exec.pay)
mypar(1,2)
hist(exec.pay)
qqnorm(exec.pay)
qqline(exec.pay) ## So This data is not normally distributed
##----------------------------------------------------------------------
boxplot(exec.pay)
mean(exec.pay)
median(exec.pay)

## Boxplot exercises #1
View(InsectSprays)
mypar(1,1)
boxplot(split(InsectSprays$count , InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)
# Which spray seems the most effective (has the lowest median count)?
# Answer : C

## Boxplot exercises #2
library(dplyr)
library(rafalib)
library(UsingR)
View(nym.2002)
mypar(2,2)
boxplot(nym.2002$time ~ nym.2002$gender)
males <- filter(nym.2002, gender == "Male") %>% dplyr::select(time) %>% unlist
females <- filter(nym.2002, gender == "Female") %>% dplyr::select(time) %>% unlist
hist(males)
hist(females)
#########################################################################
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
treatment <- filter(dat , Diet == "hf") %>% dplyr::select(Bodyweight) %>% unlist
mean(control)
mean(treatment)
obs <- mean(treatment) - mean(control)

population <- read.csv("femaleControlsPopulation.csv")
population  ##data.frame
population <- unlist(population)  ##numeric values
sample(population , 12)
mean(sample(population , 12)) ##every time we will get different result

##----------------------------------------------------------------------
##Random Variables Exercises #1
mean(population)
##Random Variables Exercises #2
set.seed(1)
x <- sample(population , 5)
abs(mean(population) - mean(x))
##Random Variables Exercises #3
set.seed(5)
x <- sample(population , 5)
abs(mean(population) - mean(x))

##---------------------------------------------------------------------
mypar(1,1)
n <- 10000
nulls <- c()
for(i in 1:n){
     control <- sample(population , 12)
    treatment <- sample(population , 12)
    nulls[i] <- mean(treatment) - mean(control)
}
hist(nulls)
max(nulls)
min(nulls)
sum(nulls > obs)
mean(nulls > obs) ##The proportion that nulls is bigger than obs.
mean(abs(nulls) > obs) # What is the probability that an outcome from 
# the null distribution is bigger than what we observed when the null
# hypothesis is true.(This is the p-value)
##---------------------------------------------------------------------
#Null Distributions Exercises #1
#What proportion of these 1,000 averages are more than 1 gram away from the average of population ?
set.seed(1)
n <- 1000
averages5 <- c()
for(i in 1:n){
  X <- sample(population,5)
  averages5[i] <- mean(X)
}
hist(averages5)    ##take a look
mean( abs( averages5 - mean(population) ) > 1)

#Null Distributions Exercises #2
#What proportion of these 10,000 averages are more than 1 gram away from the average of population?
set.seed(1)
n <- 10000
averages5 <- c()
for(i in 1:n){
  X <- sample(population,5)
  averages5[i] <- mean(X)
}
hist(averages5) ##take a look
mean( abs( averages5 - mean(population) ) > 1)

#Null Distributions Exercises #3
set.seed(1)
n <- 1000
averages50 <- c()
for(i in 1:n){
  X <- sample(population,50)
  averages50[i] <- mean(X)
}
hist(averages50)    ##take a look
mean( abs( averages50 - mean(population) ) > 1)

##----------------------------------------------------------------------
#Probability Distributions Exercises #1
library(gapminder)
data(gapminder)
head(gapminder)
data.1952 <- filter(gapminder , year == "1952") %>% dplyr::select(lifeExp) %>% unlist
mean(data.1952 <= 40)

#Probability Distributions Exercises #2
mean(data.1952 <= 60) - mean(data.1952 <= 40)

#Probability Distributions Exercises #3
prop = function(q) {
  mean(data.1952 <= q)
}
prop(40)
qs = seq(from = min(data.1952) , to = max(data.1952) , length = 20) ; qs
props <- sapply(qs , prop) ; props
plot(qs , props) ## Homemade function

plot(ecdf(data.1952)) ## Built in ecdf function

##----------------------------------------------------------------------
#Normal Distribution Exercises #1
#make average5
mypar(1,2)
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(population,5)
  averages5[i] <- mean(X)
}
hist(averages5)
# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(population,50)
  averages50[i] <- mean(X)
}
hist(averages50)

#Normal Distribution Exercises #2
# From a sample size of 50, what proportion are between 23 and 25?
mean(averages50 <= 25) - mean(averages50 <= 23)

#Normal Distribution Exercises #3
# What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 and standard deviation 0.43?
p1 <- pnorm(q = 23 , mean = 23.9 , sd = 0.43)
p2 <- pnorm(q = 25 , mean = 23.9 , sd = 0.43)
p2 - p1
##---------------------------------------------------------------------
##Population, Samples, and Estimates Exercises #1
dat <- read.csv("mice_pheno.csv")
View(dat)
dat <- na.omit(dat)
x <- filter(dat , Sex =="M" , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
mean(x)
#Population, Samples, and Estimates Exercises #2
popsd(x)
#Population, Samples, and Estimates Exercises #3
set.seed(1)
X <- sample(x , 25)
Xmean <- mean(X)
#Population, Samples, and Estimates Exercises #4
y <- filter(dat , Sex =="M" , Diet == "hf") %>% dplyr::select(Bodyweight) %>% unlist
mean(y)
#Population, Samples, and Estimates Exercises #5
popsd(y)
#Population, Samples, and Estimates Exercises #6
set.seed(1)
Y <- sample(y,25)
Ymean <- mean(Y)
#Population, Samples, and Estimates Exercises #7
popdiff <- mean(y) - mean(x)
samdiff <- Ymean - Xmean
abs(popdiff - samdiff)
#Population, Samples, and Estimates Exercises #8
x <- filter(dat , Sex =="F" , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
y <- filter(dat , Sex =="F" , Diet == "hf") %>% dplyr::select(Bodyweight) %>% unlist
mean(x)
mean(y)
popdiff <- mean(y) - mean(x)
set.seed(2)
X <- sample(x , 25)
mean(X)
set.seed(2)
Y <- sample(y , 25)
mean(Y)
samdiff <- mean(Y) - mean(X)
abs(popdiff - samdiff)

##---------------------------------------------------------------------  
#Central Limit Theorem Exercises #1
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat)
p1 <- pnorm(1 , mean = 0 , sd = 1) ; p1
p2 <- pnorm(-1 , mean = 0 , sd = 1) ; p2
p1 - p2 ##68.27% of data are in one standard deviation away from the average
#Central Limit Theorem Exercises #2
p1 <- pnorm(2 , mean = 0 , sd = 1) ; p1
p2 <- pnorm(-2 , mean = 0 , sd = 1) ; p2
p1 - p2 ##95.45% of data are in two standard deviation away from the average
#Central Limit Theorem Exercises #3
p1 <- pnorm(3 , mean = 0 , sd = 1) ; p1
p2 <- pnorm(-3 , mean = 0 , sd = 1) ; p2
p1 - p2 ##99.73% of data are in three standard deviation away from the average
#Central Limit Theorem Exercises #4
y <- filter(dat , Sex =="M" , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
mean(y)
popsd(y)
z <- ( y - mean(y) ) / popsd(y)
mean( abs(z) <= 1 )
#Central Limit Theorem Exercises #5
mean( abs(z) <= 2 )
#Central Limit Theorem Exercises #6
mean( abs(z) <= 3 )
#Central Limit Theorem Exercises #7
qqnorm(z)
abline(0,1)
#Central Limit Theorem Exercises #8
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% dplyr::select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
#Central Limit Theorem Exercises #9
y <- filter(dat, Sex=="M" & Diet=="chow") %>% dplyr::select(Bodyweight) %>% unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
mean(avgs)
#Central Limit Theorem Exercises #10
popsd(avgs)
##---------------------------------------------------------------------
library(rafalib)
mypar(1,2)
n <- 10000
nulls <- c()
for(i in 1:n){
  control <- sample(population , 12)
  treatment <- sample(population , 12)
  nulls[i] <- mean(treatment) - mean(control)
}
qqnorm(nulls)
qqline(nulls)
nulls <- c()
for(i in 1:n){
  control <- sample(population , 3)
  treatment <- sample(population , 3)
  nulls[i] <- mean(treatment) - mean(control)
}
qqnorm(nulls)
qqline(nulls)
#######################################################################
##T tests in practice
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
treatment <- filter(dat , Diet == "hf") %>% dplyr::select(Bodyweight) %>% unlist
obs <- mean(treatment) - mean(control)
N <- length(treatment)
se <- sqrt(var(control) / N + var(treatment) / N)
tstat <- obs / se
pnorm(tstat)
pvalue <- 2*(1-pnorm(tstat)) ; pvalue
population <- read.csv("femaleControlsPopulation.csv")
population <- unlist(population)  ##numeric values
n <- 10000
nulls.tstat <- c()
for(i in 1:n){
  control <- sample(population , N)
  treatment <- sample(population , N)
  se <- sqrt(var(control) / N + var(treatment) / N)
  nulls.tstat[i] <- (mean(treatment) - mean(control)) / se   #this will give us the actual null distribution of that t-statistic
}                                                            #we're assuming, using central limit theorem that it is, in fact, normally distributed.
mypar(1,1)
qqnorm(nulls.tstat)
qqline(nulls.tstat)  ##So the p-value we obtained is a good approximation of the actual p-value.

##
n <- 10000
nulls.tstat <- c()
for(i in 1:n){
  control <- sample(population , 3)
  treatment <- sample(population , 3)
  se <- sqrt(var(control) / 3 + var(treatment) / 3)
  nulls.tstat[i] <- (mean(treatment) - mean(control)) / se  
}                                                            
mypar(1,1)
qqnorm(nulls.tstat)
qqline(nulls.tstat) ##sample size = 3 does not make a good approximation
                    ## In this case we would instead use the t approximation

##-----------------------------------------------------------------------
##T test in practice 2
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat , Diet == "chow") %>% dplyr::select(Bodyweight) %>% unlist
treatment <- filter(dat , Diet == "hf") %>% dplyr::select(Bodyweight) %>% unlist
ttest <- t.test(treatment , control) ; ttest
mypar(1,2)
qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)
#CLT and t-distribution in Practice Exercises #1
n = 100
x = sample(1:6, n, replace = TRUE)
mean(x == 6)
p = 1/6    ## mean
p*(1-p)/n  ## variance
z = (mean(x == 6) - p) / sqrt(p*(1-p)/n) ##So according to the CLT z should be normal with mean 0 sd 1

set.seed(1)
z = replicate(10000 ,(mean(sample(1:6, n, replace = TRUE) == 6) - p) / sqrt(p*(1-p)/n))
mean(abs(z) > 2)

#CLT and t-distribution in Practice Exercises #2
ps <- c(0.5 , 0.5 , 0.01 , 0.01)
ns <- c(5 , 30 , 30 , 100)
library(rafalib)
mypar(4 , 2)
for(i in 1:4){
  p <- ps[i]
  sides <- 1/p
  n <- ns[i]
  zs <- replicate(10000,{
    x <- sample(1 : sides , n , replace = TRUE)
    (mean(x == 1) - p) / sqrt(p*(1-p)/n)
  }) 
  hist(zs , nclass = 7)
  qqnorm(zs)
  abline(0,1)
}
#CLT and t-distribution in Practice Exercises #3
library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
#CLT and t-distribution in Practice Exercises #4
# Answer : Xbar follows a normal distribution with mean μX and standard deviation σX/sqrt(12) where σX is the population standard deviation.
#CLT and t-distribution in Practice Exercises #5
## Answer : 0
#CLT and t-distribution in Practice Exercises #6
sd(X)
#CLT and t-distribution in Practice Exercises #7
#Z = (X¯−μ)/(σX/sqrt(n))
z <- (2/sd(X))*sqrt(12)  #Use the CLT to approximate the probability that our estimate  X¯  is off by more than 2 grams from  μX
#xbar - mu = 2
pnorm(z)
(1 - pnorm(z))
2*(1 - pnorm(z))
#CLT and t-distribution in Practice Exercises #8
se <- sqrt(var(X) /length(X) + var(Y) / length(Y))
se
#CLT and t-distribution in Practice Exercises #9
obsdiff <- mean(Y) - mean(X) ; obsdiff
tstat <- obsdiff / se ; tstat
#CLT and t-distribution in Practice Exercises #10
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)
#CLT and t-distribution in Practice Exercises #11
2*(1 - pnorm(2.0552))
# or
Z <- ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
2*( 1-pnorm(Z))
#CLT and t-distribution in Practice Exercises #11
t.test(Y , X)
######################################################################
# Introduction to Inference
#
babies <- read.table("babies.txt", header=TRUE)
View(babies)
bwt.nonsmoke <- filter(babies , smoke == 0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies , smoke == 1) %>% select(bwt) %>% unlist
library(rafalib)
popdiff <- mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#T-test Exercises #1
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , 25)
dat.s <- sample(bwt.smoke , 25)
mean1 <- mean(dat.ns)
mean2 <- mean(dat.s)
obsdiff <- mean1 - mean2
se <- sqrt(var(dat.ns) / length(dat.ns) + var(dat.s) / length(dat.s))
tval <- obsdiff / se ; tval
#T-test Exercises #2
p1 <- pnorm(abs(tval))
p2 <- pnorm(-abs(tval))
p1 - p2
pval <- 1 - (p1 - p2) ; pval
#T-test Exercises #3
2*pnorm(-abs(tval))
######################################################################
dat <- read.csv("mice_pheno.csv")
View(dat)
chowPopulation <- dat[dat$Sex == "F" & dat$Diet == "chow" , 3] ; chowPopulation
mu_chow <- mean(chowPopulation) ; mu_chow
set.seed(1)
N <- 30
chow <- sample(chowPopulation , N)
chow_mean <- mean(chow) ; chow_mean
se <- sd(chow) / sqrt(N) ; se
Z <- qnorm(1 - (0.05 / 2)) ; Z
# -Z < (chow_mean - mu_chow) / se < Z
interval <- c(chow_mean - (Z*se) , chow_mean + (Z*se)) ; interval
interval[1] < mu_chow & interval[2] > mu_chow
#(When N = 30 and we use Normal distribution approximation)
library(rafalib)
mypar(1,2)
L <- 200
N <- 30
Z <- qnorm(1 - (0.05 / 2))
plot(mu_chow + c(-8 , 8) , c(1,1) , xlab = "Weights" , ylab = "Interval" , ylim = c(1,L) , type = "n")
abline(v = mu_chow)
for(i in 1:L){
  chow <- sample(chowPopulation , N)
  se <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - (Z*se) , mean(chow) + (Z*se))
  tf.test <- interval[1] < mu_chow & interval[2] > mu_chow
  color <- ifelse(tf.test , 1 , 2)
  lines(interval , c(i , i) , col = color)
}
#(When N = 5 and we use Normal distribution approximation)
L <- 200
N <- 5
Z <- qnorm(1 - (0.05 / 2))
plot(mu_chow + c(-8 , 8) , c(1,1) , xlab = "Weights" , ylab = "Interval" , ylim = c(1,L) , type = "n")
abline(v = mu_chow)
for(i in 1:L){
  chow <- sample(chowPopulation , N)
  se <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - (Z*se) , mean(chow) + (Z*se))
  tf.test <- interval[1] < mu_chow & interval[2] > mu_chow
  color <- ifelse(tf.test , 1 , 2)
  lines(interval , c(i , i) , col = color)
}
#(When N = 5 and we use Student's t distribution approximation)
L <- 200
N <- 5
t <- qt(1 - (0.05 / 2) , df = N-1)
plot(mu_chow + c(-10 , 10) , c(1,1) , xlab = "Weights" , ylab = "Interval" , ylim = c(1,L) , type = "n")
abline(v = mu_chow)
for(i in 1:L){
  chow <- sample(chowPopulation , N)
  se <- sd(chow) / sqrt(N)
  interval <- c(mean(chow) - (t*se) , mean(chow) + (t*se))
  tf.test <- interval[1] < mu_chow & interval[2] > mu_chow
  color <- ifelse(tf.test , 1 , 2)
  lines(interval , c(i , i) , col = color)
}
##############################################################################
#Confidence Intervals Exercises #1
library(dplyr)
library(rafalib)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies , smoke == 0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies , smoke == 1) %>% select(bwt) %>% unlist
popdiff <- mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)
mean1 <- mean(dat.ns)
mean2 <- mean(dat.s)
obsdiff <- mean1 - mean2 ; obsdiff
se <- sqrt(var(dat.ns) / length(dat.ns) + var(dat.s) / length(dat.s))
t <- qt(0.99 , df = 2*N - 2) ; t ## 99% confidence level
t*se
#Confidence Intervals Exercises #3
set.seed(1)
N <- 5
dat.ns <- sample(bwt.nonsmoke , N)
dat.s <- sample(bwt.smoke , N)
t.test(dat.ns , dat.s)
#########################################################################
#Power Calculations
library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat , Sex == "F" & Diet == "chow") %>% select(Bodyweight) %>% unlist
hfPopulation <- filter(dat , Sex == "F" & Diet == "hf") %>% select(Bodyweight) %>% unlist
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
mu_hf - mu_control
((mu_hf - mu_control) / mu_control) * 100 ##Percent of increase
set.seed(1)
N <- 5
hf <- sample(hfPopulation , N)
control <- sample(controlPopulation , N)
t.test(hf , control)$p.value
##
N <- 12
alpha <- 0.05
reject <- function(N){
  hf <- sample(hfPopulation , N)
  control <- sample(controlPopulation , N)
  pval <- t.test(hf,control)$p.value
  pval < alpha
}
reject(12)
L <- 2000
rejections <- replicate(L , reject(N)) ; rejections
power <- mean(rejections) ; power ##Power of the test when sample size = 12
# Let's see how power improves with N. We will use the function sapply,
# which applies a function to each of the elements of a vector. We want to
# repeat the above for the following sample size
Ns <- seq(5 , 50 , 5)
power <- sapply(Ns , function(N)
    {
  rejections <- replicate(L , reject(N))
  mean(rejections)
    }
)
power
mypar(1,1)
plot(Ns , power , type = "b")
##---------------------------------------------------------------------------
library(dplyr)
library(rafalib)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies , smoke == 0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies , smoke == 1) %>% select(bwt) %>% unlist
popdiff <- mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
#Power Calculations Exercises #2
set.seed(1)
N <- 5
alpha <- 0.05
rejects <- replicate(10000 ,{
     dat.ns <- sample(bwt.nonsmoke , N)
     dat.s <- sample(bwt.smoke , N)
     pval <- t.test(dat.ns , dat.s)$p.value
     pval < alpha
})
mean(rejects)
#Power Calculations Exercises #3
N <- 5
alpha <- 0.05
reject <- function(N){
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  pval <- t.test(dat.ns , dat.s)$p.value
  pval < alpha
}
reject(N)  ## Made a function to check the reject or fail to reject when N = 5
L <-10000
rejections <- replicate(L , reject(N))   ## Replicate the reject function 10000 times so we get different sample of size 5 every time
power <- mean(rejections) ; power   ## Find the power of the test when N = 5
Ns <- c(30 , 60 , 90 , 120)
power <- sapply(Ns , function(N){
  rejections <- replicate(L , reject(N))
  mean(rejections)
})
power
# Power Calculations Exercises #4
# same ase previous(just alpha = 0.01)
##############################################################################
# Monte-Carlo Simulations
set.seed(1)
library(rafalib)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- read.csv("femaleControlsPopulation.csv")
controlPopulation <- unlist(controlPopulation)

tstatgenerator <- function(n){
  cases <- sample(controlPopulation , n)
  controls <- sample(controlPopulation , n)
  tstat <- (mean(cases) - mean(controls)) / sqrt(var(cases) / n + var(controls) / n)
  return(tstat)
}
tstatgenerator(10)
tstats10 <- replicate(1000 , tstatgenerator(10) ) ##Monte Carlo Simulation
hist(tstats10)
qqnorm(tstats10)
qqline(tstats10)
#
tstatgenerator(3)
tstats03 <- replicate(1000 , tstatgenerator(3) ) ##Monte Carlo Simulation
hist(tstats03)
qqnorm(tstats03)
qqline(tstats03)

## 
N <- 3     ##sample size = 3
ps <- seq(from = 0 , to = 1 ,length = 1000)
qs <- qt(ps , df = 2*(N - 1)) ; qs
qqplot(qs , tstats03)
qqline(qs , tstats03)
#
qqnorm(controlPopulation)
qqline(controlPopulation)
#
mean(controlPopulation)
sd(controlPopulation)
# Now assume we don't have the access to the population data and we 
# have to create the population data with assumed mean = 24 and 
# sd = 3.5 , this is called parametric simulation 
controls <- rnorm(5000 , mean = 24 , sd = 3.5)
tstatgenerator <- function(n , mean = 24 , sd = 3.5){
  cases <- rnorm(n , mean , sd) # instead of samples we are taking randorm numbers from normal distribution
  controls <- rnorm(n , mean , sd)
  tstat <- (mean(cases) - mean(controls)) / sqrt(var(cases) / n + var(controls) / n)
  return(tstat)
}
tstatgenerator(10)

tstats <- replicate(10000 ,tstatgenerator(3))
qqnorm(tstats)
qqline(tstats)
##--------------------------------------------------------------------
#Monte Carlo Exercises #1
set.seed(1)
n <- 5
x <- rnorm(n)
tstat <- (sqrt(n)*mean(x)) / sd(x) ; tstat
#Monte Carlo Exercises #2
B <- 1000
set.seed(1)
tstatgenerator <- function(n){
  sam <- rnorm(n)
  tstat <- (sqrt(n)*mean(sam)) / sd(sam)
  return(tstat)
}
tstatgenerator(5)
tstats <- replicate(B , tstatgenerator(5))
mean(tstats > 2)

#Monte Carlo Exercises #3
library(rafalib)
mypar(3,2)
Ns <- seq(5,30,5)
B <- 1000
mypar(3,2)
for(N in Ns){
  ts <- replicate(B,{
     X <- rnorm(N)
     sqrt(N)*mean(X)/sd(X)
  })
  ps <- seq(1/(B+1) , 1-1/(B+1),len = B)
  qs <- qt(ps , df = N-1)
  qqplot(qs , ts , main = N , xlab = "Theoretical" , ylab = "Observed")
  abline(0,1)
} 
#Monte Carlo Exercises #4
Ns <- seq(5,30,5)
B <- 1000
mypar(3,2)

for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x , y , var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1) , 1-1/(B+1) , len = B)
  qs <- qt(ps , df = 2*N - 2)
  qqplot(qs , ts , main = N , xlab = "Theoretical" , ylab = "Observed")
  abline(0,1)
} 
#Monte Carlo Exercises #5
set.seed(1)
N <- 15
B <- 10000
tstats <- replicate(B,{
  X <- sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps=seq(1/(B+1), 1-1/(B+1), len=B) 
qqplot(qt(ps,N-1), tstats, xlim=range(tstats))
abline(0,1)
#The population data is not normal thus the theory does not apply.
#We check with a Monte Carlo simulation. The qqplot shows a large tail. 
#Note that there is a small but positive chance that all the X are the same.
##In this case the denominator is 0 and the t-statistics is not defined
#Monte Carlo Exercises #6
set.seed(1)
N <- 1000
B <- 10000
tstats <- replicate(B,{
  X <-  sample(c(-1,1), N, replace=TRUE)
  sqrt(N)*mean(X)/sd(X)
})
qqnorm(tstats)
abline(0,1)
#With N=1000, CLT kicks in and the t-statistic is approximated with normal 0,1
##Furthermore, t-distribution with df=999 and normal are practically the same.
#Monte Carlo Exercises #7
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)
##########################################################################
# Permutation test
dat=read.csv("femaleMiceWeights.csv")

library(dplyr)

control <- filter(dat,Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat,Diet=="hf") %>% select(Bodyweight) %>% unlist
obsdiff <- mean(treatment)-mean(control)
N <- 12
avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v=obsdiff, col="red", lwd=2)
#the proportion of permutations with larger difference
(sum(abs(avgdiff) > abs(obsdiff)) + 1) / (length(avgdiff) + 1)
mean(abs(avgdiff) > obsdiff)
##
N <- 5
control <- sample(control,N)
treatment <- sample(treatment,N)
obsdiff <- mean(treatment)- mean(control)
avgdiff <- replicate(1000, {
  all <- sample(c(control,treatment))
  newcontrols <- all[1:N]
  newtreatments <- all[(N+1):(2*N)]
  return(mean(newtreatments) - mean(newcontrols))
})
hist(avgdiff)
abline(v = obsdiff, col = "red", lwd = 2)
mean(abs(avgdiff) > obsdiff)
##------------------------------------------------------------------------
#Permutations Exercises #1
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke == 0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke == 1) %>% select(bwt) %>% unlist

N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff <- mean(smokers) - mean(nonsmokers)

avgdiffs <- replicate(1000 ,{ 
 dat <- c(smokers,nonsmokers)
 shuffle <- sample( dat )
 smokersstar <- shuffle[1:N]
 nonsmokersstar <- shuffle[(N+1):(2*N)]
 avgdiff <- mean(smokersstar)-mean(nonsmokersstar)
 return(avgdiff)
})
hist(avgdiffs)
abline(v = obsdiff , col = 2 , lwd = 3)
pvalue <- mean(abs(avgdiffs) > abs(obsdiff)) ; pvalue

#Permutations Exercises #2
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obsdiff <- median(smokers) - median(nonsmokers)

avgdiffs <- replicate(1000 ,{ 
  dat <- c(smokers,nonsmokers)
  shuffle <- sample( dat )
  smokersstar <- shuffle[1:N]
  nonsmokersstar <- shuffle[(N+1):(2*N)]
  avgdiff <- median(smokersstar) - median(nonsmokersstar)
  return(avgdiff)
})
hist(avgdiffs)
abline(v = obsdiff , col = 2 , lwd = 3)
pvalue <- mean(abs(avgdiffs) > abs(obsdiff)) ; pvalue ## Theorytical formula
pvalue <-  (sum(abs(avgdiffs) >= abs(obsdiff)) + 1 ) / (length(avgdiffs) + 1) ; pvalue ## Real formula

##########################################################################
#Association Tests 
#Fisher test
tab <- matrix(c(3,1,1,3),2,2) ; tab
rownames(tab)<-c("Poured Before","Poured After")
colnames(tab)<-c("Guessed before","Guessed after")
tab
fisher.test(tab , alternative = "greater")

##Chi-square test

disease <- factor(c(rep(0,180) , rep(1,20) , rep(0,40) , rep(1,10)) , labels = c("control","cases")) ; disease
genotype <- factor(c(rep("AA/Aa",200) , rep("aa",50)) , labels = c("AA/Aa" , "aa")) ; genotype
dat <- data.frame(disease , genotype) ; dat
shuffle <- dat[sample(nrow(dat)) , ] ; shuffle   ##Shuffle them up
table(genotype)
table(disease)
tab <- table(genotype , disease) ; tab
OR <- (tab[2,2]/tab[2,1]) / (tab[1,2]/tab[1,1]) ; OR ## Odds ratio
p <- mean(disease == "cases") ; p  ##Probability of disease (30/250)
c(1-p,p) * sum(genotype == "AA/Aa")
c(1-p,p) * sum(genotype == "aa")
expected <- rbind(c(1-p,p) * sum(genotype == "AA/Aa") , c(1-p,p) * sum(genotype == "aa")) ; expected
dimnames(expected) <- dimnames(tab)
expected
chisq.test(tab)
tab <- tab*10
chisq.test(tab)

fit <- glm(disease~genotype , family = "binomial" , data = dat) ; fit  ##generalised linear model
coeftab <- summary(fit)$coef ;coeftab
coeftab[2,2]
ci <- coeftab[2,1] + c(-1.96,1.96) * coeftab[2,2] ; ci
exp(ci)

#Association Tests Exercises #1
d = read.csv("assoctest.csv")
tab <- table(d$allele , d$case)
chisq.test(tab)
#Association Tests Exercises #2
fisher.test(tab)
###########################################################################
##ScatterPlot
library(UsingR)
View(father.son)
x <- father.son$fheight
y <- father.son$sheight
plot(x , y)

##Stratification
split(y , round(x)) ##divided the sons height based on the group of fathers height
boxplot(split(y , round(x)))

# For the particular question we asked of 72 inch height
# father , what's the prediction for that son,
# we can just take the average of that group.

mean(y[round(x) == 72]) ##the avg sons hieght when the father's height is 72
## Standardized
X <- (x - mean(x)) / sd(x)
Y <- (y - mean(y)) / sd(y)
means <- tapply(Y , round(X*4) / 4 , mean) ;means
names(means)
fatherheights <- as.numeric(names(means)) ; fatherheights
plot(fatherheights , means)
abline(0 , cor(X,Y))

##
set.seed(1)
a <- rnorm(100) ; a[1] = 25
b <- rnorm(100) ; b[1] = 26
plot(a,b)

##Scatterplot Exercises #1
library(dplyr)
library(UsingR)
dat <- nym.2002
View(dat)
males <- filter(dat , gender == "Male") %>% dplyr::select(age , time) ; males
females <- filter(dat , gender == "Female") %>% dplyr::select(age , time)
male.age <- males$age
male.time <- males$time
cor(male.age , male.time)

##Scatterplot Exercises #2
female.age <- females$age
female.time <- females$time
cor(female.age , female.time)

##Scatterplot Exercises #3
library(rafalib)
mypar(2,2)
plot(female.age , female.time)
plot(male.age , male.time)
group.female <- floor(female.age / 5)*5 ; group.female
boxplot(female.time ~ group.female)
group.male <- floor(male.age / 5)*5 ;group.male
boxplot(male.time ~ group.male)

##Symmetry of Log Ratios Exercises #1
time = sort(nym.2002$time) ; time
median(time)
time[1] #fastest time
min(time) #fastest time
min(time) / median(time)

##Symmetry of Log Ratios Exercises #2
max(time) / median(time)

##Symmetry of Log Ratios Exercises #3
mypar(2,1)
plot(time/median(time), ylim=c(1/4,4))  ##Without using log 
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2)) ## Using log
abline(h=-1:1)
#######################################################
#Median, MAD, and Spearman Correlation Exercises #1
library(UsingR)
data <- ChickWeight
View(data)
plot(data$Time , data$weight , col = data$Diet)
chick = reshape(data,idvar = c("Chick","Diet") , timevar ="Time",direction = "wide") ; chick
chick <- na.omit(chick)
weight_4 <- chick$weight.4 ; weight_4
mean(weight_4)
weight_3000 <- c(weight_4 , 3000) ; weight_3000
mean(weight_3000)
proportion <- mean(weight_3000) / mean(weight_4) ; proportion

#Median, MAD, and Spearman Correlation Exercises #2
proportion <- median(weight_3000) / median(weight_4) ; proportion

#Median, MAD, and Spearman Correlation Exercises #3
proportion <- sd(weight_3000) / sd(weight_4) ; proportion

#Median, MAD, and Spearman Correlation Exercises #4
proportion <- mad(weight_3000) / mad(weight_4) ; proportion

#Median, MAD, and Spearman Correlation Exercises #4
correlation <- cor(chick$weight.4 , chick$weight.21) ; correlation  ##without outlier
correlation.3000 <- cor(c(chick$weight.4 , 3000) , c(chick$weight.21 , 3000)) ; correlation.3000  ## with outlier
proportion <- correlation.3000 / correlation ; proportion
