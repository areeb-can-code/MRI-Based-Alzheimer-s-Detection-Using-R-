## DEM projects ##)
library(tidyverse)
library(readr)
oasis_cross_sectional %>%
  ggplot(aes(y= MMSE, x=`M/F`, color=`M/F`)) + geom_jitter()


names(oasis_cross_sectional)

oasis_cross_sectional %>%
  ggplot(aes(x=nWBV, fill=`M/F` )) + geom_histogram()


names(oasis_longitudinal)
str(oasis_longitudinal$Group)
oasis_longitudinal %>%
  ggplot(aes(x=Group, y= nWBV, color= `M/F`)) + geom_jitter()

lmodel <- lm(formula = nWBV ~ Age , data = oasis_cross_sectional)
library(ggcorrplot)
ggcorrplot(lmodel)
lmodel
oasis_cross_sectional %>%
  ggplot(aes(x= factor(Age), y=nWBV, color = `M/F`)) +
  geom_boxplot()
install.packages('quantreg')

library(quantreg)
# library(dplyr)
oasis_cross_sectional%>%
  group_by(Age) %>%
  summarise_all(funs(n(), Sum=sum(.)), oasis_cross_sectional$nWBV)

oasis_longitudinal%>%
  ggplot(aes(y=Age, x=factor(Group), color= eTIV))+
  geom_jitter(na.rm = TRUE)


library(ggplot2)
