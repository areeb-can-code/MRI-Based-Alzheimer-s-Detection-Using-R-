##Week 7 and Onwards ##
print("Finishing touches")
library(tidyverse)
library(ggcorrplot)
library(nycflights13)
names(wine)
wine %>%
  ggplot(aes(x=Price, y=AGST, color=Price)) + geom_point()+ geom_smooth(model='lm')
kR <- cor(wine$Price, wine$AGST)
sqrt(kR)

#############################
###wine test##
###########################

library(readr)
wineTest <- readr::read_csv(file.choose())
str(wineTest)
summary(wineTest)
giniEx<- readr::read_csv(file.choose())
summary(giniEx)
head(giniEx)
# install.packages("rpart")
# install.packages('rattle')
library(rpart)
library(rattle)
tree <- rpart(formula=Class~ . ,method='class', 
              data=giniEx, minbucket=1, cp=-1)
tree
fancyRpartPlot(tree)
mpgClean <- data.frame(readr::read_csv(file.choose()))
mpgClean <- mpgClean%>%
  mutate(model_year = factor(model_year),
                             origin= factor(origin))
summary(mpgClean)
mpgClean <- mpgClean%>% 
  select(-car_name)
mpgClean
library(caret)
set.seed(2020)
## Linear Regression ##
sample <- createDataPartition(y=mpgClean$mpg, groups=5, 
                              p=0.7, list=F) ## p is a factor or % of 
    #how much data should be used for training purposes

## splits data for us 
train <- mpgClean[sample, ]
test <- mpgClean[-sample, ]

## drawing histograms of both
train %>%
  ggplot(aes(x=mpg, )) + geom_histogram()
test %>%
  ggplot(aes(x=qmpg)) + geom_histogram()

#drawing a a linear regression # ## Train a regression model 
linearM <- lm(mpg~weight, data=train)
summary(linearM)

## Test on testing data 
prediction <- predict(linearM, newdata = test)
prediction
library(ModelMetrics)
 

# now we want to see how similar it is with Root Mean Squared
## Takes actual and then predicted 
rootMeanSq <- rmse(actual=test$mpg, predicted = prediction)
summary(rootMeanSq)
 
## Maybe you want continuous vars to be categorized to high and low
##  brain vols high and low

## Classificaition
mpgClean$mpg_discrete <- cut(mpgClean$mpg, breaks=2, labels=c('Low', 'High'))
mpgClean
head(mpgClean)

library(rpart)
library(rattle)


# Creating a partition and testing those categorical vals
## THIS IS WRONG DON'T DO THIS -DONT KEEP THE MPG VAR ###
set.seed(2020)
sample2 <- createDataPartition(y=mpgClean$mpg_discrete, 
                               p=0.7, list=F)
## data partiion of hte mpg (randomly gen)
train2 <- mpgClean[sample2, ]
test2 <- mpgClean[-sample2,]
decision1 <- rpart(formula=mpg_discrete~. , ## the actual col is in 
                   ## teh formula
                   data = train2)
fancyRpartPlot(decision1)


## Droppping the MPG Var- THIS IS CORRECT ###
mpgClean <- select(mpgClean, -mpg)
  set.seed(2020)
sample3 <- createDataPartition(y=mpgClean$mpg_discrete, 
                               p=0.7, list=F)
## data partiion of hte mpg (randomly gen)
train3 <- mpgClean[sample3, ]
test3 <- mpgClean[-sample3,]
decision2 <- rpart(formula=mpg_discrete~. , ## the actual col is in 
                   ## teh formula
                   data = train2=3)
fancyRpartPlot(decision1)
  
####### WEEK 7 His notes #####
library(tidyverse)

df = read.csv('datasets/data_course_slides_gini_index_example.csv')
df


# Parent Gini

# 10 C0, 10 C1

P = 1 - (10/20)^2 - (10/20)^2
P

# Split on Gender Type:
# M: 6 C0, 4 C1
# F: 4 C0, 6 C1

gini_M = 1 - (6/10)^2 - (4/10)^2
gini_M
gini_F = 1 - (4/10)^2 - (6/10)^2

# Weighted Average of Children nodes based on Gender:

M_gender = 10/20 * gini_M +   10/20 * gini_F
M_gender

# Information Gain based on Gender split:
P - M_gender


# Split on Car type
# Family: 1 C0, 3 C1
# Luxury: 1 C0, 7 C1
# Sports: 8 C0, 0 C1

gini_Fam = 1 - (1/4)^2 - (3/4)^2
gini_Lux = 1 - (1/8)^2 - (7/8)^2
gini_Sport = 1 - (8/8)^2 - (0/8)^2

M_vehicle = 4/20 * gini_Fam + 8/20 * gini_Lux + 8/20 * gini_Sport
M_vehicle

P - M_vehicle


# Shirt size
# Small: 3 C0, 2 C1
# Medium: 3 C0, 4 C1
# Large: 2 C0, 2 C1
# XL: 2 C0, 2 C1


# install.packages('rpart')
library(rpart)
# install.packages('rattle')
library(rattle)


df = select(df, -Customer.ID)
head(df)

tree = rpart(formula = Class ~ . , method='class', data=df)
tree
fancyRpartPlot(tree)

tree = rpart(formula = Class ~ . , method='class', 
             data=df, minbucket=1, cp=-1)
fancyRpartPlot(tree)











## Week 8 Notes
library(tidyselect)
df <- data.frame(readr::
      read_csv(file.choose()))
summary(df)
df <- df %>%
  select(-car_name) %>%
  mutate(model_year <- factor(model_year), 
         origin <- factor(model_year))
df
## Create Binary Class Based on MPG ####
    # now we want to transform mpg
      # into categorical values (low,high)

cut(x=df$mpg, breaks=2)
df$mpg_discrete1 <- cut(x=df$mpg, breaks=2, labels=c("Low", "High"))
df$mpg_discrete2 <- cut(x=df$mpg, breaks=c(0,25,100), labels=c('Lo', 'Hi'))

df <- df%>%
  mutate(mpg_discrete3=ifelse(mpg>25, 'H', 'L'))
df <- df%>%
  select( 
         -c(mpg, mpg_discrete2, mpg_discrete1))
df <- rename(df, 'mpg_discrete' = 'mpg_discrete3')
df
### Train and Test split ####
library(caret)
sample <- createDataPartition(df$mpg_discrete, p = 0.7, list=F)

train <- df[sample, ]
test <- df[-sample, ]
## Checking if the values of hte target vars are preserved after split, 
##Stratified Sampling
table(test$mpg_discrete)
table(train$mpg_discrete)
prop.table(table(train$mpg_discrete))
prop.table(table(test$mpg_discrete))

## Now making a decision Tree ####
library(rpart)
library(rattle)
 
dec1 <-  rpart(mpg_discrete ~ ., data = train) ## do not use the OG Dataset here
## now we gotta visualise what we got 
fancyRpartPlot(dec1)

pred <- predict(dec1, newdata = test, type = 'prob')


# Making a confusino Matricx
df_pred <-  data.frame(pred, mpg_discrete <- test$mpg_discrete)
###install.packages('yardstick')
library(yardstick)

caret::confusionMatrix(df_pred$pred, df_pred$mpg_discrete, positve="H") 

## Assignment 3 Hell but I'm Proud ####
giniInit <-1- (12/25)^2 - (13/25)^2
giniInit

giniWarm  <- 1-(4/8)^2-(4/8)^2
giniCold <- 1-(5/8)^2-(3/8)^2
giniHot <- 1-(3/9)^2-(6/9)^2
giniWarm
giniCold
giniHot
giniWeather <- 8/25*giniWarm + 8/25 * giniCold + 9/25 *giniHot
giniWeather
giniInit - giniWeather


  giniSunny <- 1- (9/16)^2 - (7/16)^2
  giniRainy <- 1- (3/9)^2 - (6/9)^2
  giniRainy
  giniWeather <-  16/25*giniSunny + 9/25 * giniRainy
  giniInit - giniWeather

entropyInit <- -(12/25)* log2(12/25) - (13/25) *log2(13/25)
entropyInit

entropySunny <- -(9/16)* log2(9/16) - (7/16) *log2(7/16)
entropySunny
entropyRainy <- -(3/9)* log2(3/9) - (6/9) *log2(6/9)
entropyRainy
entropyWeather <- entropyInit - entropySunny*(16/25)- entropyRainy* (9/25)
entropyWeather
