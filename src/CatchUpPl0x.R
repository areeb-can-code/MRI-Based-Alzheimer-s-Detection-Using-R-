#Week 6 Stress and Assignments
library('tidyverse')
install.packages('tidyverse')
library('nycflights13')
library('flights')
install.packages('flights')
## I can say this is not Week 6 lol
## Weeks to Catch ##

# Subsetting hopefully ##
library(dplyr)
library(nycflights13)

# Select() the only cols you need
select(flights, carrier, dep_delay)


#fil ter
# pick between median or mean with seletions

uhhS2 <- select(flights, carrier, dep_delay)
filthy <- filter(uhhS2, carrier=='UA')

# below is mean for departure delay with UA
mean(filthy$dep_delay,na.rm=TRUE)
## Avg delay for whole data set
mean(flights$dep_delay, na.rm = TRUE)
##This filter excludes UA but it it is the data set that 
##    contains no UA Or dep_delay

## ALSO don't include dep_delay while filtering or it will mess 
##up the mean
## calculation
notUA <- filter(flights, carrier!='UA')
mean(notUA$dep_delay, na.rm=TRUE)

#Filtering two things so UA and AA
twoFilter <- filter(filthy, carrier== c('UA', 'AA'))
## This is definetly the wrong answer since this will be missing data
mean(twoFilter$dep_delay, na.rm=TRUE)
# This is how you subset using the %in% operator to combine them
## correctly
twoFilter2 <- filter(flights, carrier %in% c('UA', 'AA'))
mean(twoFilter2$dep_delay, na.rm=TRUE)
## To check the uniqueness use the unique function
unique(twoFilter$carrier)
unique(twoFilter2$carrier)


# finding the carrier with the largest dep delay
max(flights$dep_delay, na.rm=TRUE)
rowIndex <- which.max(flights$dep_delay)
rowIndex
## Gives back the carrier if you know where the cols are located
flights[rowIndex, 10] ## or even write a col name
flights[rowIndex, 'carrier']


df2 <-flights%>% 
  select(carrier, dep_delay, arr_delay, air_time, distance) %>%
  mutate(total_delay = dep_delay + arr_delay, speed = (air_time / distance) *60)
df2

## Group by and finding means
df2 %>% group_by(carrier) %>% summarise(mean(total_delay, na.rm=TRUE))


## Apr 23## notes



# install.packages('ggplot2')
library(dplyr)
library(ggplot2)

# reading in the auto-mpg data

bruh = read.table('auto-mpg.data', header = FALSE)
head(bruh)
str(bruh)
names(bruh)

# We will work on this data on 4/28/2020

##########################################
library(nycflights13)
flights

head(flights)

# If your data contains 
##  more than ~10,000 rows it can be very slow
#     to draw scatter plots. It will also be useless 
##      since there will be
#       too much overlap.

# that's why we are sampling 2000 points randomly.
# The set.seed() will ensure that we all have
    ##the same random points selected.
set.seed(123)
flights_small = sample_n(flights, 2000)
?sample_n()
# scatter plots
ggplot(flights_small, aes(x=dep_time, y=arr_time) ) + geom_point()

ggplot(flights_small, aes(x=dep_delay, y=arr_delay) ) + geom_point()

ggplot(flights_small, aes(x=dep_delay, y=arr_delay, col=factor(carrier))
       ) + geom_point()
names(flights)
# histogram

ggplot(flights_small, aes(x=dep_delay)) + geom_histogram()
ggplot(flights_small, aes(x=dep_delay)) + geom_histogram(bins = 50)
#####Me Practicing ####
# Analyzing GapMinder Data

install.packages('gapminder')
library(gapminder)
library(tidyverse)
library(dplyr)# 

# how many obs and vars are there
str(gapminder)
print("1704 obs and 6 vars")

# 2
head(gapminder)

#3  ## remember to specify to see the last 10 rows
tail(gapminder, 100)

# 4
summary(gapminder)

# 5 $$ shows all the countries in the whole data set so you're not a dumbo
gapminder %>% group_by(country) %>% n_distinct()
unique(gapminder$country) ## the write answer


# 6 ## filtering US
gapminder %>% filter(country=='United States')
 # 7 filter out for the year 1990
gapminder %>% filter(year == '1990')
summary(gapminder)

n_distinct(gapminder$year)
# To check the distinct # of years or cols in a certain variable
unique(gapminder$year)

# Order data based off of Life Expectancy in ascending (low to high)
gapminder %>% select(lifeExp, country, year, pop, gdpPercap) %>%
  arrange(desc(lifeExp))
## did both ascending and descending on teh line above

## 8 sort data by lowest to hi year and hi gdp to low gdp
gapminder %>% arrange(year, desc(gdpPercap))


# 9 Filter year 2007, sort\
##    by gdpPercap in descending order, report only the first 10 countries.

biBoy <- gapminder%>% filter(year==2007) %>%
  arrange(desc(gdpPercap))%>%
  head(10)## and add head 10
biBoy
head(biBoy, 10)
## to find the country with the largest life expec
gapminder %>% arrange(desc(lifeExp)) ## this one is good but what if you find another way
gapminder %>% slice(which.max(lifeExp)) ## all in one row to get  info 2 get other 
## conclusions
# 10
# Which country has the smallest gdp per cap
gapminder %>%
  group_by(country) %>%
  arrange(gdpPercap)

## smallest life exp in 2002
gapminder %>%
  group_by(country)%>%
  arrange(lifeExp)%>%
  filter(year =='2002')
## pretty good but the real solution is much more efficient. FILTER FIRST
gapminder %>%filter(year=='2002') %>% slice(which.min(lifeExp))
 ## combine populiation and gdppercaptia

gapminder %>% mutate(gdp= gdpPercap * pop) %>% arrange(desc(gdp))
## sorting the data


# 13 

# That scientific notation does not 
##  look very nice. Since the number is very large R puts that sign.
# 
# If we were to divide the gdp by 1,000,000 the gdp 
##    would be in "millions". Let's try that.
# 
# Create gdp column in million dollars and show the data in descending gdp.

gapminder %>% mutate(gdp= (gdpPercap * pop)/1000000) %>% arrange(desc(gdp))
# 14
#
# 
# Question:
#   
#   Filter 2007 (filter)
# Create gdp column in million dollars (mutate)
# Sort in descending gdp (arrange)
# What is the name of the 10th country?

gapminder %>% filter(year=='2007') %>%
  mutate(gdp= (gdpPercap *pop)/1000000) %>%
  arrange(desc(gdp)) %>%
  head(10)
  
  
# 15
#
summarize()

#What is the mean life expectancy 
#in the whole dataset?
# this bad boy doesnt wanna work
gapminder%>% 
  mean(gapminder$lifeExp, na.rm = TRUE)

gapminder%>% summarise(max(lifeExp, na.rm = TRUE))
 ## 16
#What is the mean life expectancy in each continent?
gapminder %>% group_by(continent) %>%
  summarise(mean(lifeExp, na.rm=TRUE))
# 17
#What is the median gdp in 
#each continent? Sort by median gdp in descending order.
gapminder
gapminder2 <- gapminder %>% 
  mutate(gdp= pop * gdpPercap/1000000)

gapminder %>%
  group_by(continent) %>% 
  summarise(median(gdp, na.rm = TRUE)) %>%
  arrange(desc(gdp))

# the answer
gapminder%>%
  mutate(gdp = pop * gdpPercap/1000000) %>%
  group_by(continent) %>%
  summarize(median_gdp = median(gdp)) %>%
  arrange(desc(median_gdp))


## whatever do the work again next time

# 18  What is the median gdp of 
##US, Germany, and Japan in the years between 1990 and 2010?
gapminder %>%
  filter(year > 1990, country %in% c('United States', 'Germany', 'Japan')) %>%
  mutate(gdp = pop * gdpPercap/1000000) %>%
  group_by(country) %>% ## forgot to show the countries by grouping them
  summarize(median_gdp=median(gdp)) %>%
  arrange(desc(median_gdp))


 # I'm doing so much better now! thank you Allah
# 19 Find the country with largest gdp per capita
#in each year. Hint: Make use of group_by(), slice(), and which.max()

gapminder %>%
  group_by(year) %>%
  slice(which.max(gdpPercap))

# the answer
gapminder %>% 
  group_by(year) %>% 
  slice(which.max(gdpPercap))

# I rewrote this shit
gapminder %>%
  group_by(year) %>%
  slice(which.max(gdpPercap))
## get better at slicing 

# 20 Find the country with largest gdp in
## each year. Hint: Make use of mutate(),
##group_by(), slice(), and which.max()
gapminder %>%
  mutate(gdp=gdpPercap*pop /1000000) %>%
  group_by(year) %>%
  slice(which.max(gdp))%>%
  arrange(desc(gdp), year)

# the answer
gapminder %>% 
  mutate(gdp = pop * gdpPercap/1000000) %>% 
  group_by(year) %>% 
  slice(which.max(gdp))

 # 21 and the last question

#Find the country with largest life expectancy in each year. 
# Hint: Make use of group_by(), slice(), and which.max()

gapminder %>%
  group_by(year) %>%
  slice(which.max(lifeExp))


## I did it just a few hiccupps

# week 5 apr 28
auto = read.table('auto-mpg.data', header = FALSE)
head(bruh)
str(bruh)
names(bruh)
names <- c('mpg', 'cylinders', 'displacement', 
           'horsepower', 'weight','acceleration',
           'model_year', 'origin', 'car_name')
colnames(auto)= names
names(bruh)
library(dplyr)
library(ggplot2)
library(tidyverse)
# install.packages('tidyverse')

# Here make usre the dataset and code file are in the same folder.
# Set the working directory by: 
# Session -> Set working directory -> To source file location
# Check the data
str(auto)
summary(auto)

df = mutate(auto, horsepower2 = ifelse(horsepower == '?', NA, 
                                       as.character(horsepower)))

auto$horsepower = as.numeric(df$horsepower2)

str(auto)
auto  = auto %>% 
  mutate(cylinders = as.factor(cylinders), 
                        origin = as.factor(origin))

str(auto)

# Impute the missing values
df = drop_na(auto)

 #install.packages('mice')
library(mice)

md.pattern(auto)
miceMod = mice(data=auto)
miceOutput = complete(miceMod)


anyNA(miceOutput)
anyNA(auto)


auto = miceOutput
##############################

# Scatter plot
ggplot(auto, aes(x=horsepower, y=mpg) ) +
  geom_point()


ggplot(auto, aes(x=cylinders, y=horsepower)) + 
  geom_point()


ggplot(auto, aes(x=cylinders, y=horsepower)) + 
  geom_jitter()


ggplot(auto, aes(x=horsepower, y=mpg, color=cylinders) ) + 
  geom_point()

?geom_jitter()
ggplot(auto, aes(x=horsepower, y=mpg, color=model_year) ) + 
  geom_point()


# Create a scatter plot of mpg vs weight, and color by origin
# x = weight, y = mpg

ggplot(auto, aes(x=weight, y=mpg, color=displacement)) + geom_point()
names(auto)

# Line plot
# mean mpg of each year
# group_by, summarize
auto %>% 
  group_by(model_year) %>%
  summarise(mean_mpg=mean(mpg))



auto_year= auto %>% group_by(model_year) %>% 
  summarize(mean_mpg = mean(mpg))

ggplot(auto_year, aes(x=model_year, y=mean_mpg)) + geom_line(size=4)



# mean mpg of each year and each origin
# group_by, summarize
auto_year_orig = auto %>% group_by(model_year, origin) %>% 
  summarize(mean_mpg = mean(mpg))
auto_year_orig


ggplot(auto_year_orig, aes(x=model_year, y=mean_mpg, color=origin)) + 
  geom_line(size=2)


# Bar plot
auto_year= auto %>% group_by(model_year) %>% 
  summarize(mean_mpg = mean(mpg), 
            mean_weight=mean(weight), 
            mean_hp=mean(horsepower))

auto_year

ggplot(auto_year, aes(x=model_year, y=mean_mpg)) + geom_col()
ggplot(auto_year, aes(x=model_year, y=mean_weight)) + geom_col()


ggplot(auto_year, aes(x=model_year, y=mean_mpg, fill=model_year)) + 
  geom_col()

ggplot(auto_year, 
       aes(x=as.factor(model_year), y=mean_mpg, fill=as.factor(model_year))) + 
  geom_col()


## Count plot
ggplot(auto, aes(x=origin)) + geom_bar()

ggplot(auto, aes(x=origin, fill=origin)) + geom_bar()


ggplot(auto, aes(x=model_year, fill=origin)) + geom_bar()

ggplot(auto, aes(x=model_year, fill=origin)) + geom_bar(position='dodge')


# histogram
ggplot(auto, aes(x=mpg)) + geom_histogram()

ggplot(auto, aes(x=mpg)) + geom_histogram(binwidth=2)

ggplot(auto, aes(x=mpg, fill=origin)) + geom_histogram(binwidth=2)

# Boxplot
ggplot(auto, aes(y=weight)) + geom_boxplot() + scale_x_discrete('')

ggplot(auto, aes(x=origin, y=weight)) + geom_boxplot()

# create a boxplot of mpg for each cylinder type
ggplot(auto, aes(x=cylinder, y=mpg, color=origin)) + geom_boxplot() 


names(auto)
ggplot(auto, aes(x=cylinders, y=mpg, fill=cylinders)) + geom_boxplot()




#######Apr 30- and More Practice ####
library(openintro)
ncbirths
str <- names(ncbirths)
str
ggplot(ncbirths, aes(x=factor(weeks), y=weight, color=gender)) + geom_boxplot()
