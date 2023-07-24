#Lab10
library(tidyverse)
colNames <- c("ID", "VCap", "Worth", "Shares", "Buyout") #Cartegorical responce, quantitative explanatory
IPO <- read.table("http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/kutnerData/Appendix%20c%20Data%20Sets/APPENC11.txt", col.names = colNames)
summary(IPO)
hist(IPO$Worth)
IPO <- IPO %>% mutate(LogWorth = log10(Worth))
hist(IPO$LogWorth)

#Logistic Regretion with glm()
modelLogReg <- glm(VCap~LogWorth, data = IPO, family = "binomial")
modelLogReg
summary(modelLogReg) #intersept -7.6722  slop = 1.0225
anova(modelLogReg)
#probability (P hat) = e^intersept+slop /

#built the function
RegPlot <- function(B0, B1, L, U, BY) {
  x <- seq(from=L, to=U, by=BY)
  p_hat <- (exp(B0+B1*x))/(1+exp(B0+B1*x))
  plot(x, p_hat)
}

Find.P <- function(B0, B1, x){
  p_hat <- (exp(B0+B1*x))/(1+exp(B0+B1*x))
  p_hat
}

#use function to create a plot for IPO
RegPlot(-7.672, 1.023, 0, 15, .1) #y = P_hat/probability of success
Find.P(-7.672, 1.023, 10)

######################################################################IRIS
ggplot(data = iris) + geom_point(mapping = aes(x=Sepal.Length, y=Sepal.Width, color=Species))
ggplot(data = iris) + geom_point(mapping = aes(x=Petal.Length, y=Petal.Width, color=Species))

#Petal models
modelFull <- lm(iris$Petal.Width~iris$Petal.Length*iris$Species) #lm(Y~X*Categorical)
summary(modelFull)
modelReduce <- lm(iris$Petal.Width~iris$Petal.Length) #lm(Y~X)
summary(modelReduce)
anova(modelReduce, modelFull)

#Sepal models
modelFull <- lm(iris$Sepal.Width~iris$Sepal.Length*iris$Species) #lm(Y~X*Categorical)
summary(modelFull)
modelReduce <- lm(iris$Sepal.Width~iris$Sepal.Length) #lm(Y~X)
summary(modelReduce)
anova(modelReduce, modelFull)
