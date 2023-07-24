install.packages("carData")
library(carData)
library(tidyverse)
library(broom)
theme_set(theme_classic())
?Salaries

summary(Salaries)

#Introduction -Why you care?
#Q = Quantitative, C = categorical
#response Variable(DV) -> Salary Q
#explanatory Variable(IV) -> sex C (2levels), discipline C (2levels), rank C (3 levels), yrs.since.PhD Q, yrs.service Q

Salaries %>% group_by(sex) %>% summarise(Number = n(), Mean = mean(salary), Median = median(salary), SD = sd(salary), IQR = IQR(salary))
boxplot(Salaries$salary~Salaries$sex)
boxplot(Salaries$salary~Salaries$discipline)
plot(Salaries$yrs.service, Salaries$salary)
cor(Salaries$yrs.service, Salaries$salary)

plot(Salaries$yrs.since.phd, Salaries$salary)
cor(Salaries$yrs.since.phd, Salaries$salary) #bigger correlation

#Q variables talk abou center(m,x bar), spread(sd, IQR), shape(symmetry, Skewed, modality), unusual(outlines)

#Linear model
model <- lm(Salaries$yrs.service~Salaries$salary)
summary(model)
anova(model)
plot(Salaries$yrs.service, Salaries$salary)
abline(model)

ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.service, y = salary, color = sex)) #salary vs sex
ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.service, y = salary, color = rank)) #salary vs rank
ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.service, y = salary, color = rank, shape = sex))

ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.service, y = salary)) +
  facet_grid(rank ~ sex)

ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.service, y = salary)) +
  facet_grid(discipline ~ rank)


Salaries %>% filter(yrs.service == 0) #new professors

#Regression model
model <- lm(salary ~ yrs.service, data = Salaries)

model.diag.metrics <- augment(model)

ggplot(model.diag.metrics, aes(yrs.service, salary)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = yrs.service, yend = .fitted), color = "red", size = 0.3)



attach(Salaries)
model1 <- lm(salary~yrs.service)
summary(model1)
model2 <- lm(salary~rank)
summary(model2)

boxplot(Salaries$salary ~ Salaries$rank)
Salaries %>% group_by(rank) %>% summarise(mean = mean(salary), median = median(salary), sd = sd(salary), IQR = IQR(salary), min = min(salary), max = max(salary))
ggplot(data = Salaries) +
  geom_point(mapping = aes(x = yrs.since.phd, y = salary, color = rank))
boxplot(Salaries$salary~Salaries$sex)
mytable <- table(Salaries$rank, Salaries$discipline, Salaries$sex)
ftable(mytable)
prop.table(mytable)
