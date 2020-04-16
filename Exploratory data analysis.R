library(UsingR)
View(father.son)
x <- father.son$fheight  ##we are trying to figure out how the data look like
##-----------------------------------------
sample(x,20)
rount(sample(x,20),1) ## still we have no clue
##--------------------------------------------------------------------------
hist(x , main = "Histogram of Father's Height" , xlab = "Height in inches") ## Now we can say that the data look's like Normal Distribution
##--------------------------------------------------------------------------
xs <- seq(floor(min(x)) , ceiling(max(x)) , 0.1)
ecdf(x)
ecdf(xs)
ecdf(x)(xs)
plot(xs , ecdf(x)(xs) , type = "l" , xlab = "Height on inches" , ylab = "F(x)", main = "ECDF")
  ##Here we can see 80-85% of the people are less than 70 inches of height

##Histogram Exercises #1