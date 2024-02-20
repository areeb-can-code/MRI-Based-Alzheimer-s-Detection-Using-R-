#MRI and Alzheimers:

# MMSE -> Mini Mental State Evaluation
# CDR -> Clinical Dementia Rating
# eTIV -> Estimated Total Intracranial Volume
# nWBV -> Normalize Whole Brain Volume
# ASF -> Atlas Scaling Factor



summary(mri2)

#install.packages("ggplot2")
library(caret)
library(tidyverse)
library(rattle)
library(e1071)
library(readr)
library(rpart.plot)
library(ggplot2)
library(caTools)

int.volume <- mri2$eTIV * mri2$nWBV

p <- ggplot(data = mri2, aes(x = Age, y = nWBV, 
                             color = Group, size = CDR))  
p + geom_jitter() + labs(x= "Age" ,
                         y= "Normalize Brain Volume",
                         size="Clinical Dementia Rating",
                         title= "Age vs Brain Volume")


p <- ggplot(data = mri2, aes(x = MMSE, y = CDR, 
                             color = Group))  
p + geom_jitter(size = 3)

plot(mri2)

group.model <- lm(nWBV ~ Group, data = mri2)
summary #0.09525 (p = 1.09e-06)

visit.model <- lm(nWBV ~ Visit, data = mri2)
summary(visit.model) #0.01435 (p = 0.0144)
plot(visit.model)



mr.delay.model <- lm(nWBV ~ MR.Delay, data = mri2)
summary(mr.delay.model)  #0.008483 (p  = 0.0415)
plot(mr.delay.model)

M.F.model <- lm(nWBV ~ M.F, data = mri2)
summary(M.F.model)  #0.05978 (p = 1.05e-06)

age.model <- lm(nWBV ~ Age, data = mri2)
summary(age.model)  #0.2667 (p = 2e-16)

edu.model <- lm(nWBV ~ EDUC, data = mri2)
summary(edu.model) #0.8143 (p = 0.814)

ses.model <- lm(nWBV ~ SES, data = mri2)
summary(ses.model) #0.09054 (p = 0.0905)

cdr.model <- lm(nWBV ~ CDR, data = mri2)
summary(cdr.model) #0.1165 (p = 7.47e-12)

etiv.model <- lm(nWBV ~ eTIV, data = mri2)
summary(etiv.model) #0.04157 (p = 4.31-05)

asf.model <- lm(nWBV ~ ASF, data = mri2)
summary(asf.model) #0.043 (p = 3.22e-05)

fs.model1 <- lm(nWBV ~ Age + CDR + M.F, data = mri2)
summary(fs.model1) 


#------------------------- Backward Elimination


be.model1 <- lm(nWBV ~ Group + Visit + MR.Delay + M.F + Age + EDUC + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model1)


be.model2 <- lm(nWBV ~ Group + MR.Delay + M.F + Age + EDUC + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model2)


be.model3 <- lm(nWBV ~ Group + MR.Delay + M.F + Age + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model3)


be.model4 <- lm(nWBV ~ Group + M.F + Age + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model4)

be.model5 <- lm(nWBV ~ MR.Delay + M.F + Age + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model5)

be.model6 <- lm(nWBV ~  M.F + Age + SES + MMSE +
                  CDR + eTIV + ASF, data = mri2)
summary(be.model6)

be.model7 <- lm(nWBV ~  M.F + Age + SES + MMSE +
                  CDR + eTIV, data = mri2)
summary(be.model7)

int.mmse.cdr <- mri2$MMSE * mri2$CDR

be.model8 <- lm(nWBV ~  M.F + Age + SES + MMSE + CDR
                , data = mri2)
summary(be.model8)


#----------------- Visualizations

#Scatterplot -> Age and Normalized Whole Brain Volume (Group and Clinical Dementia Rating)
u <- ggplot(data = mri2, aes(x = Age, y = nWBV,
                             color = Group, size = CDR))
u+ geom_point()

u + geom_point() + geom_smooth(fill = NA)

#Boxplot:
b <- ggplot(data =filteredD, aes(x = Age, y = nWBV,
                                 color = Group))

b + geom_boxplot()
b + geom_boxplot(size = 1.2) + 
  labs(title="Age vs Brain Volume",
       x = "Age",
       y="Normalized Brain Volume") +geom_bar()
brainVol <-  filteredD%>%
  ggplot(aes(x=nWBV, fill= Group)) +
  geom_histogram(bins=15) + labs(x="Normalized Brain Volume",
                                 title= "Histogram of Brain Vol")
brainVol


geom_boxplot(size = 1.2) + 
  geom_point()


b + geom_boxplot(size = 1.2) + geom_jitter()

b + geom_jitter() + geom_boxplot(size = 1.2, alpha = 0.5)

# Scatter/Line Plot -> ASF and Normalized Whole Brain Volume (Including M/F, Clinical Dementia Rating)
ggplot(data = mri2, 
       aes(x = ASF, y = nWBV, color = M.F, size = CDR)) +
  geom_point() +
  labs(title = "ASF vs nWBV", 
       x= "Atlas Scaling Factor",
       y= "Normalize Brain Volume",
       size= "Clinical Demented Rating")

#### JUSTUS CONTRIBU ####
#cross sectional file 
mri1 <- read.csv("oasis_cross-sectional.csv")
mri2 = read.csv("oasis_longitudinal.csv")

library(ggplot2)
library(tidyverse)

mri3 = mri2 %>% select(MMSE, CDR, EDUC, SES, nWBV)

summary(mri2)

ggplot(mri2, aes(x = ASF, y = as.factor(CDR))) + geom_boxplot()

ggplot(mri2, aes(x = SES, y = as.factor(CDR),color=Group)) + geom_jitter()

ggplot(mri2, 
       aes(x = nWBV, y = ASF, 
           color = as.factor(CDR))) + 
  geom_point() + geom_smooth()

ggplot(mri2, aes(x = nWBV, y = ASF, color = as.factor(MMSE))) + geom_point()

lm1 = lm(formula = CDR ~ ASF + eTIV + nWBV, data = mri2)
summary(lm1)
lm2 = lm(formula = MMSE ~ ASF + eTIV + nWBV, data = mri2)
summary(lm2)

lm3 = lm(formula = CDR ~ EDUC + SES, data = mri2)
summary(lm3)
lm4 = lm(formula = MMSE ~ EDUC + SES, data = mri2)
summary(lm4)

cormri = cor(mri3)
ggcorrplot::ggcorrplot(cormri, type = 'lower')


lm6 = lm(formula = CDR ~ nWBV + EDUC + M.F + Age, 
         data = filteredD)
summary(lm6)
samp <- sample.split(Y=filteredD$Group, SplitRatio = 0.70)

filteredD <- mri2%>% 
  filter(Group %in% c("Demented", "Nondemented"))


dem_train <-filteredD[samp==TRUE,] ## train set
dem_test <- filteredD[samp== FALSE, ]
decisionmri <- rpart(Group ~ EDUC+ nWBV+Age , data = dem_train)
names(mri2)


age <- filteredD%>%
  ggplot(aes(x=EDUC, y= nWBV, size= CDR, color= Group)) +
  geom_jitter() + labs(x="Education", 
                       y= "Normalized Brain Volume", 
                       title="Education vs Brain Volume")
age
p <- ggplot(data = filteredD, aes(x = Age, y = nWBV, 
                                  color = Group, size = CDR))  
p + geom_jitter() + labs(x= "Age" ,
                         y= "Normalize Brain Volume",
                         size="Clinical Dementia Rating",
                         title= "Age vs Brain Volume")



#### MATT ####
ocs <- read.csv(file = 'oasis_cross-sectional.csv')

# confusion matrix setup
library(caret)
# DataFrame Structure
str(ocs)
# view 10 columns
head(ocs[, 1:10])

# splitting data into training and test data

# For our split I will use the 70/30 split we have used in class
createDataPartition()
# the training data is created to learn of relationships between x and y

# the model is udes to predict our dependent variable

# train/test data split

set.seed(100)

# rnum of training data
trnum <- createDataPartition(ocs$nWBV, p = 0.7, list = FALSE)
# the createDataPartition uses the input Y 
# By setting the value of list to FALSE we can prevent the rownumbers as a list
data_table  = sort(sample(nrow(nocs), nrow(nocs)*.7)) # *.7 is the 70% training data
drop <- c("Hand")
df = ocs[, !(names(ocs)%in%drop)]
df
library(caret)
set.seed(200)
tr_index <- createDataPartition(df$nWBV, p = .7, list = FALSE, times = 1) # the number that gets assigned to times is the num in which is split

tr_data  <- df[ tr_index, ]
tst_data <- df[-tr_index, ]

l <- sapply(tr_data, function(x) is.factor(x))
m <- tr_data[, l]
m
ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")
# need to drop hand as a variable because the level of hand is offsetting the other factor types




# my dependent variable is nWBV
str(tr_data)
str(tst_data)
str(tr_index)



# let n and m be used as values for 
# creating a tree
treeFit <- train(tr_data$nWBV ~ ., data = tr_data, na.action = na.pass, method = "rpart", tuneLength=10)
treeFit
Tree_p  <- predict(treeFit, tst_data, na.action = na.pass)

# to measure the levels
levels(Tree_p)

confusionMatrix(Tree_p, tst_data$nWBV, mode = "everything")
