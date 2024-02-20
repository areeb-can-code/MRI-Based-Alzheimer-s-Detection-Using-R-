### Week 5 continued 
## Week 6 May 5

###########################################
###########################################
library(openintro)
library(dplyr)
library(ggplot2)
library(tidyverse)

###########################################
###########################################

data(ncbirths)
?ncbirths

str(ncbirths)


ggplot(ncbirths, aes(x=weeks, y=weight)) + geom_point()
ggplot(ncbirths, aes(x=weeks, y=weight)) + geom_jitter()

ggplot(ncbirths, aes(x=weeks, y=weight)) + geom_jitter(alpha=0.3)

ggplot(ncbirths, aes(x=factor(weeks), y=weight)) + geom_boxplot()


cut(ncbirths$weeks, breaks=4)
cut(ncbirths$weeks, breaks=c(25,30,35,40,45))



ggplot(ncbirths, aes(x=cut(weeks, breaks=10), y=weight)) + geom_boxplot()
 ae
ggplot(bdims, aes(x= hgt, y=wgt, color=factor(sex))) + geom_jitter() +
         stat_smooth(method = 'lm')

names(bdims)

###########################################
###########################################
## Week 6 notes inlcluding hiss ##

###########################################
###########################################
library(tidyverse)
library(openintro)

x = c(3,4,5,6,7)
y = c(10, 9, 12, 15, 13)
cor(x,y)

mean_x = mean(x)
mean_x

mean_y = mean(y)
mean_y

(x[1] - mean_x) * (y[1] - mean_y) + 
  (x[2] - mean_x) * (y[2] - mean_y) + 
  (x[3] - mean_x) * (y[3] - mean_y) + 
  (x[4] - mean_x) * (y[4] - mean_y) + 
  (x[5] - mean_x) * (y[5] - mean_y)

(x[1] - mean_x)^2 + (x[2] - mean_x)^2  + ...

seq(5)

numerator = 0
for (i in 1:5) {
  numerator <- numerator + (x[i] - mean_x) * (y[i] - mean_y)
}
numerator

x_sum = 0 
y_sum = 0
for (i in 1:5) {
  x_sum = x_sum + (x[i] - mean_x)^2
  y_sum = y_sum + (y[i] - mean_y)^2
}
x_sum
y_sum

denominator = sqrt(x_sum * y_sum)
denominator

numerator/denominator

cor(x,y)

plot(x,y)


# Outliers

data(bdims)

ggplot(bdims, aes(x=hgt, y=wgt, color=factor(sex))) + geom_point()


lm(formula = wgt ~ hgt, data = bdims)

ggplot(bdims, aes(x=hgt, y=wgt)) + geom_point() + stat_smooth(method='lm')

set.seed(123)
# Create outlier data

bdims2 = bdims %>% 
  mutate(hgt_outlier = ifelse(hgt > 195, hgt*2, hgt))

summary(bdims2)


lm(formula = wgt ~ hgt_outlier, data = bdims2)


ggplot(bdims2, aes(x=hgt_outlier, y=wgt)) + geom_point() + stat_smooth(method='lm')


ggplot(bdims2, aes(x=hgt_outlier)) + geom_boxplot()

quantile(bdims2$hgt_outlier)

iqr = IQR(bdims2$hgt_outlier)

iqr

bdims3 = filter(bdims2, hgt_outlier < 200 + 1.5*iqr )

ggplot(bdims3, aes(x=hgt_outlier, y=wgt)) + geom_point()

# Capping
x = bdims2$hgt_outlier
qnt = quantile(x, probs=c(.25, .75), na.rm=T)
caps = quantile(x, probs=c(.05, .95), na.rm=T)


qnt
caps

H = 1.5 * IQR(x, na.rm=T)
H


x[x < (qnt[1] - H)] = caps[1]
x[x > (qnt[2] + H)] = caps[2]

bdims2$hgt_outlier = x

ggplot(bdims2, aes(x=hgt_outlier, y=wgt)) + geom_point()

# Scaling
# min-max normalization

# (x - min_x) / (max_x - min_x)

x = c(3,4,5,6,7)
y = c(10, 9, 12, 15, 13)

min_x = min(x)
max_x = max(x)
min_x
max_x

(x - min_x) / (max_x - min_x)

min_max = function(x) {
  min_x = min(x)
  max_x = max(x)
  x_new = (x - min_x) / (max_x - min_x)
  return(x_new)
}

x = c(3,4,5,6,7)
min_max(x)

y = c(10, 9, 12, 15, 13)
min_max(y)

# Standardization

# (x - mean_x) / sd_x

(x - mean(x))/ sd(x)
(y - mean(y))/ sd(y)

scale(x)[,1]
scale(y)[,1]


###########################################
###########################################
## Day 2 Weeek 6 ###

###########################################
###########################################
library(tidyverse)


wine = read.csv('wine.csv')
str(wine)

# AGST: Average Growing Season Temperature
head(wine)

summary(wine)

anyNA(wine)

ggplot(wine, aes(x=Year, y=Price)) + geom_point()

ggplot(wine, aes(x=FrancePop, y=Price)) + geom_point()

ggplot(wine, aes(x=HarvestRain, y=Price)) + geom_point()

ggplot(wine, aes(x=WinterRain, y=Price)) + geom_point()

ggplot(wine, aes(x=AGST, y=Price)) + geom_point()


 #install.packages('ggcorrplot')
library(ggcorrplot)

cormat = cor(wine)
ggcorrplot(cormat)

ggcorrplot(cormat, type='lower')


model1 = lm(formula= Price ~ WinterRain, data=wine)

summary(model1)


model2 = lm(formula= Price ~ HarvestRain, data=wine)

summary(model2)


model3 = lm(formula= Price ~ AGST, data=wine)
summary(model3)

model3$coefficients
coefs = coef(model3)
coefs[1]
coefs[2]
b = coefs[1]
m = coefs[2]

b
m

# Predict the price of wine for the first 3 cases
# y = m*x + b
x = wine$AGST[1:3]
x

m*x + b

# Predictions
yhat = coefs[2] * x + coefs[1]

y = wine$Price[1:3]

# residual
y - yhat

model3$residuals

# Base model calcualte Sum of Squared Errors (SSE)
mean_price = wine$Price %>% mean()

error = wine$Price - mean_price
error

SSE_mean = sum(error ^2)
SSE_mean


model3 = lm(formula= Price ~ AGST, data=wine)
summary(model3)

model3$residuals

SSE_fit = sum(model3$residuals ^2)
SSE_fit

# SSE_fit = sum((wine$Price - model3$fitted.values) ^ 2)

SSE_mean
SSE_fit

# R^2
(SSE_mean - SSE_fit) / SSE_mean

# Multiple Linear Regression
model4 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model4)

sum(model4$residuals ^ 2)

(10.15 - 2.500209)/10.15

model5 = lm(Price ~ ., data=wine)
summary(model5)

head(wine)

model6 = lm(Price ~ .-Age, data=wine)
summary(model6)

model7 = lm(Price ~ .-Year, data=wine)
summary(model7)


# Multiple Linear Regression
model4 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model4)

coefs = model4$coefficients
coefs
coefs['HarvestRain']
model4$coefficients[3]

# Prediction of wine price for input:
# AGST = 17.1167, HarvestRain = 160, WinterRain = 600
pred = coefs[['(Intercept)']] + 
  coefs[['AGST']] * 17.1167 + 
  coefs[['HarvestRain']] * 160 + 
  coefs[['WinterRain']] * 600
pred

head(wine)

# Prediction for the whole dataset
pred = predict(model4, newdata=wine)

# Residuals
wine$Price - pred

# Root mean squared error
sqrt(mean(model4$residuals^2))

# plot
df = data.frame(Price = wine$Price, Prediction = pred)
head(df)

ggplot(df, aes(x=Price, y=Prediction)) + geom_point()

# Predict using the test data
wine_test = read.csv('wine_test.csv')
wine_test

predict(model4, newdata=wine_test)
?predict()







