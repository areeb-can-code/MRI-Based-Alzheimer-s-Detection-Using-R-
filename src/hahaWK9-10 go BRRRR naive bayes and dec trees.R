## haha grade go Down






# his code but me testing it out as well you know...
## May 26 Day 1 2020 WK 9#####

library(tidyverse)
# install.packages('e1071')
library(e1071)
library(caret)

computer = read.csv('data_course_slides_buy_computer.csv')
computer
names(computer)
nb = naiveBayes(buys_computer ~ age + income + student + credit_rating, data = computer)
nb


# Prior prob of buying a computer#####
9/14

person = data.frame(age = '<=30', income = 'medium', student='yes', credit_rating='fair')
str(person)

# Predict the class#####
predict(nb, newdata=person, type='class')

# show the probabilities of each class given the attributes#####
predict(nb, newdata=person, type='raw')



pred = predict(nb, newdata=computer, type='class')
pred
computer$buys_computer


### Making a confusino Matrix with Computers #####
caret::confusionMatrix(pred, computer$buys_computer, positive = 'yes')


?naiveBayes
nb

# Laplace Correction #####
nb = naiveBayes(buys_computer ~ age + income + student + credit_rating, 
                data = computer, laplace=1)

nb

# If you check the conditional probabilities now for buys_computer = no#####
# Laplacian correction is done. This adds 1 to each numerator of each categorical#####
# value in the varaible, and adds a constant to the denominator. This constant is equal #####
# to the number of unique categorical values in that particualr variable.#####

# The goal of the Laplacian correction is to prevent zero probability ####
# of a categorical value from ruining all the other variables and rendering them####
# all to be zero. 




######NOTHING #### -------------------------------------------------------
##-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
###010101011010101010101010101010101010101010101010101011010
####><><><><><><><><><><><><<><><<><><><><><><><><><><><<>><><><><


print("May 28 KNN and Nearest Neighbor")


######## Euclidian distance calculation######
p1 = c(0, 2)
p2 = c(2, 0)
p3 = c(3, 1)
p4 = c(5 ,1)

sqrt(sum((p1 - p2)^2))

sqrt(sum((p1 - p3)^2))

sqrt(sum((p1 - p4)^2))


A = c(-1, 2)
C = c(3, -3)

sqrt(sum((A - C)^2))


########### KNN######
library(tidyverse)
library(caret)
library(caTools)

data(iris)
iris

ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) + 
  geom_point()

set.seed(1)
spl = sample.split(iris$Species, SplitRatio = 0.7)
spl
iris_train = subset(iris, spl==TRUE)
iris_test = subset(iris, spl==FALSE)


m_knn = knn3(Species ~ Petal.Length, data=iris_train, k=1)
pred = predict(m_knn, newdata=iris_test, type='class')

table(pred, iris_test$Species)

cm = caret::confusionMatrix(pred, iris_test$Species)
cm$overall[['Accuracy']]

###Try different k values to find the optimum based on accuracy####
accuracy = c()
for (k in seq(1, 10, by=1)) {
  m_knn = knn3(Species ~ ., data=iris_train, k = k)
  pred = predict(m_knn, newdata=iris_test, type='class')
  cm = caret::confusionMatrix(pred, iris_test$Species)
  accuracy = c(accuracy, cm$overall[['Accuracy']])
}
accuracy

k = seq(1, 10, by=1)
k
k[5]
# 5 nearest neighbors gives the best accuracy #####

m_knn = knn3(Species ~ ., data=iris_train, k = 5)
pred = predict(m_knn, newdata=iris_test, type='class')
cm = caret::confusionMatrix(pred, iris_test$Species)
cm

df = data.frame(k, accuracy)

ggplot(df, aes(x=factor(k), y=accuracy)) + geom_col()


ggplot(df, aes(x=k, y=accuracy)) + geom_line()


## Quiz 6 Stress and Staying up #####
library(tidyverse)
library(rpart)
library(rattle)
library(caret)
library(caTools)
library(e1071)
library(readr)
hotel <- read.csv('hotel_cancellations.csv')
str(hotel)
anyNA(hotel)
set.seed(1)
spl <- sample.split(Y=hotel$is_cancelled, SplitRatio = 0.70)
hotel_training <- hotel[spl==TRUE,] ## train set
hotel_test <- hotel[spl== FALSE, ] ### Test set
spl



str(hotel_training)
hotel_training$is_cancelled
str(hotel_test)
n_distinct(hotel_training)
#
hotel_training %>%
  group_by(is_cancelled) %>%
  count()
##
names(hotel)
hotel_training %>% 
  filter(is_cancelled == 'Cancelled') %>%
  group_by(deposit_type) %>%
  count()

## making a decision tree
decisionJuan <- rpart(is_cancelled ~ ., data = hotel_training)
decisionJuan
str(decisionJuan)
fancyRpartPlot(decisionJuan)

# finding the accuracy
bigPred <- predict(decisionJuan, newdata = hotel_test, type='class')
bigPred
mean(hotel_test$is_cancelled == bigPred )

## confusion matrix
print("Making a pred of the predictions we have")
hotel_pred <- data.frame(bigPred, is_cancelled=hotel_test$is_cancelled)
hotel_pred
bruh <- caret::confusionMatrix(hotel_pred$bigPred, 
                               hotel_pred$is_cancelled, positive='Cancelled')
bruh

### Making a Naive Bayes Model ####
##############nb = naiveBayes(buys_computer ~ age + income + student 
    ####         + credit_rating, data = computer)
#######################nb
nbHotel = naiveBayes(is_cancelled ~ ., data = hotel_training)
nbHotel
##### Making a Prediction on NB #####
nbPred <- predict(nbHotel, newdata = hotel_test, type='class')
nbPred
mean(hotel_test$is_cancelled == nbPred )
## Making a confusion Matrix ####
hotel_predNB <- data.frame(nbPred, is_cancelled=hotel_test$is_cancelled)
bruh2 <- caret::confusionMatrix(hotel_predNB$nbPred, hotel_predNB$is_cancelled, 
                                positive='Cancelled')
bruh2
