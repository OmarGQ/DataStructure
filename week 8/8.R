attach(iris)
View(iris)

plot(Petal.Length, Petal.Width)
cor(Petal.Length, Petal.Width)


model <- lm(Petal.Width~Petal.Length)
model
summary(model)
anova(model)
plot(x,y)
abline(model)
