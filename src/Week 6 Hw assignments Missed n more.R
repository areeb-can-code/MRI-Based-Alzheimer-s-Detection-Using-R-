## Week 6 Attempt 2 with Quizzes and Assignments
library(flights)
library(nycflights13)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(dbplyr)


## Me doing hw up here instad of practice###
attach(flights)
flights
glimpse(flights)
attach(flights)
attach(fl_new)
fl_new <- flights%>%
  mutate(total_delay= dep_delay+arr_delay) %>%
  select(month,carrier,origin, dest, distance, total_delay, arr_delay)%>%
  mutate(total_delayMean = mean(total_delay, na.rm=TRUE))
#Which of the following carriers has the smallest mean total delay
##in the fl_new data? 
fl_new$total_delayMean


bruh <- fl_new %>%
  arrange(total_delay)
bruh
min(mean(bruh$total_delay, na.rm= TRUE))


## question 2 
library(airports)

library(airports)
?airports
??airports
names(airports)
flights
airports
fl_airport <-  inner_join(fl_new, airports, by = c('dest' = 'faa'))
head(fl_airport)
names(airports)
names(flights)
names(airports)
airportsName <- sort((airports$name))
airportsName
fl_airport%>% subset(month == '6') %>% filter(dest =='TPA')
fl_airport%>% filter(total_delay > total_delayMean)
maxDelay<- max(fl_airport$total_delay, na.rm = TRUE)

## Example #
flights %>% 
  filter(month==5) %>%
  group_by(carrier) %>%
  count() %>%
  arrange(desc(n))
attach(flights)
fl_new%>%
  group_by(carrier)%>%
  summarise(total_delay22= mean(arr_delay + dep_delay, na.rm = TRUE))%>%
  arrange(total_delay22)

airports<- airports
airports
?airports

## Another question
fl_airportMeanTD<- fl_airport %>%
  mutate(mean_toe_delay = mean(total_delay,na.rm=TRUE))
fl_airportMeanTD %>%
  select(mean_toe_delay)

  ### Continue working on fl_airport data. What is the average arr_delay in 
  ##flights to "Tampa Intl" airport in September? *
fl_airport %>%
  mutate(avg_arr_delay = mean(arr_delay)) 
fl_airport %>%
  filter(dest == 'TPA', month == 9)%>%
  summarise(mean(arr_delay, na.rm = TRUE))

### Next Question ###
##Continue working on fl_airport data. Which one of the 
##following is the airport with largest average arr_delay? *
names(fl_airport)
str(fl_airport$total_delayMean)

fl_airport %>%
  group_by(name) %>%
  summarise(arr_delay_mean2=mean(arr_delay,na.rm=TRUE)) %>%
  arrange(desc(arr_delay_mean2))
fl_airport45 <-  fl_airport%>%
  mutate(arr_delay_mean2= mean(arr_delay, na.rm=TRUE)) %>%
  select(arr_delay_mean2)
## Next question with 2 steps###
#----###----#### 
#Which one of the following is the airport that has the largest
#average arr_delay that had at least 10,000 arrivals?

  #Hint: First get the airports with at least 10,000 arrivals. 
  #Then check the mean arrival delay among those airports. *
names(fl_airport)
fl_airport%>%
  group_by(name) %>%
  
  
  



## da right answer 
flights %>% 
  filter(month==5) %>%
  group_by(carrier) %>%
  count() %>%
  arrange(desc(n))

flights %>%
  group_by(carrier) %>%
  count() %>%
  filter(month==5) %>%
  arrange(desc(n))



##### END OF HW####
plot(cars)
myData<- cars
?flights
library("tidyverse")
#importing data from World Bank
climate<- read.csv("https://gitlab.com/stragu/DSH/raw/master/R/tidyverse_next_steps/data_wb_climate.csv",
                   na = "..")
libary("tidyverse")
library(tidyr)
library(tidyverse)
# use tidyr to lengthn th e data
climate_long <- pivot_longer(climate, 
                             `X1990`: `X2011`,
                             names_to = "year", 
                             values_to = "value")
mutate(year=as.integer(year))
#use tidyr to widen teh data
codes <- unique(climate_long[,c("Series code", "Series name")])



#use purr for iterating with functional programming
mtcars
class(mtcars)
?mtcars
carOutput<- vector ("double", ncol(mtcars))
for (i in seq_along(mtcars)){
  carOutput[[i]] <- median(mtcars[[i]])}
i
carOutput

#the map family in purr
car_medians <- map_dbl(mtcars, median)
car_medians

typeof(car_medians)
starwars
map_lgl(starwars, is_character) ## returns with dataset and shows wich ones are char vals
                          # and that's mapped as vectors with logical values
map_dbl(starwars, is_double)
# cahgne the default bahievior of function applied
map_dbl(mtcars, mean, trim = 0.2)
map_dbl(mtcars, ~round(mean(.x)))
# now trying to do a logical variant of the one above
map_lgl(mtcars , ~max(.x) > 3 *min(.x))

#find out the number of unique values in each var in STARWARS
map_int(starwars, ~length(unique(.x)))
## this gives back this 

name     height       mass hair_color skin_color 
87         46         39         13         31 
eye_color birth_year     gender  homeworld    species 
15         37          5         49         38 
films   vehicles  starships 
24         11         17 



#splitting data 
#split function
unique(mtcars$cyl)
mtcars%>%
  split(.$cyl)  %>% 
  map(summary)


# splitting empty cars // map () always returns a list

mtcars %>%  ## makes sccatter plot
  split(.$cyl) %>%
  map(~ggplot(.,aes(mpg, wt)) + geom_point() +geom_smooth())
        
        
#predicates
str(iris)
?str   
iris %>%
  discard(is.factor) %>%
  map_dbl(mean) %>% round(1)

## this gives back unique value with characte values as well
starwars %>%
  keep(is.character) %>%
  map_int( ~ length(unique(.x)))



# sometimes we want to keep eevyeritn gbut apply function on some
iris %>%
  map_if(is.numeric, round) %>% ## only appliues to numeric vals
  str()
 # cumulative  yearly change in CO2 emissions
climate_cum <- climate_
libary(plotly)
install.packages("plotly")
library(plotly)




#################NEW VID#################
require(tidyr)
data("starwars")
starwars %>% ## and then
  select(gender, mass, height, species) %>%
  filter(species=="Human") %>%
  na.omit() %>%
  mutate(height = height/100) %>% ## this line changes it from centimeters
                              ## to meters and puts that change into the dataset as well.
                      # only for htat data set you using
mutate(BMI=mass/height^2)%>%
  group_by(gender) %>%
  summarize(Average_BMI = mean(BMI))


# is there a didffer with gender bmi
print("you did it")
library(tidyr)
library(tidyverse)

##### Data Wrangling part 1######
# so this spots vars and obs w/in data
  # quickly derive new vars and obs to explore
  # to reshpae data in2 da layout that works best for R
# to join multiple data sets together
# and to use 'group-wise' summaries to explore
    #hidden levels of information w/in ur data
# goes with tidyr and dplyr 
## done in all notes lol


#####Practice with GGPlot now ####

glimpse(cars)
attach(starwars)
names(starwars)
# I'm learning guys
select(starwars, mass, name) %>% filter(mass>10) %>% arrange (desc(mass))
 
detach(starwars)
ggplot(diamonds, aes(carat, price)) + 
  facet_wrap(~color)

## Learning with a guy####
print("Filter is subsetting rows by vars")
print("Arrange is sort rows by vars")
print("Select is columns (vars)")
print("Mutate is change values")
print("Count is ---")
print("summarize is ==")
#### Hey I'm learning w/ Titenic####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)


titanic <- read.csv(file = "titanic.csv", stringsAsFactors = FALSE)
view(titanic)
## Setting up factors##
titanic$Pclass<- as.factor(titanic$Pclass)
titanic$Survived<- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)

ggplot(titanic, aes(x=Survived)) +
  geom_bar()

## Add some customization
ggplot(titanic, aes(x=Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y="Passenger Count", title = "Titanic Survival Rates")
## Add some hypothesis and survival rate for gender?
ggplot(titanic, aes(x=Sex, fill=Survived)) +
  theme_bw() + 
  geom_bar() +
  labs(y= "Passenger Count", title= "Titanic Survival Rates by Sex")


## Check if class had a motivation for survival rate
ggplot(titanic, aes(x=Pclass, fill=Survived)) +
  theme_bw() +
  geom_bar()+
  labs(y="Passenger Count", title="Titanic Surival Rates by P class")
## facet Wrap
ggplot(titanic, aes(x=Sex, fill= Survived)) +
  theme_bw() + 
  facet_wrap(~Pclass) +
  geom_bar() +
  labs (y = "Pass Count", title="Titanic Surv R8s Pclass & Sex")
### Making distribution of ages###
ggplot(titanic, aes(x=Age)) + theme_bw() +
  geom_histogram(binwidth =1) +
  labs(y="pass Count", x = "Age (binwidth 2)" , title="Titanic Age Distro")
##Dpylr Vids or More ##
library(dplyr)
flights%>%
  group_by()



#### Hw 2 Stuff####
c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)-> a
a
mean(a)
median(a)
mode(a)
aFactor <- as.factor(a)
a
x <- seq(-5,5, 0.1)
x
y <- (x^2)
y
cor(x,y)
print(cor(x,y))
print.warnings(cor(x,y))
?plot
plot(x,y)
plot(y,x)
ggplot(y, (aes(x= "Lmao")+ geom_point(y)))

cor(y,x)

print(25.00 24.01 23.04 22.09 21.16 20.25 19.36 18.49 17.64 16.81 16.00 15.21 14.44 13.69 12.96 12.25 11.56 10.89 10.24  9.61  9.00  8.41  7.84  7.29  6.76  6.25  5.76  5.29  4.84  4.41  4.00  3.61  3.24  2.89  2.56  2.25  1.96  1.69  1.44  1.21  1.00  0.81 0.64  0.49  0.36  0.25  0.16  0.09  0.04  0.01  0.00  0.01  0.04  0.09  0.16  0.25  0.36  0.49  0.64  0.81  1.00  1.21  1.44 1.69  1.96  2.25  2.56  2.89  3.24  3.61  4.00  4.41  4.84  5.29  5.76  6.25  6.76  7.29  7.84  8.41  9.00  9.61 10.24 10.89 11.56 12.25 12.96 13.69 14.44 15.21 16.00 16.81 17.64 18.49 19.36 20.25 21.16 22.09 23.04 24.01 25.00
      )
-5.0 -4.9 -4.8 -4.7 -4.6 -4.5 -4.4 -4.3 -4.2 -4.1 -4.0 -3.9 -3.8 -3.7 -3.6 -3.5 -3.4 -3.3 -3.2 -3.1 -3.0 -2.9 -2.8 -2.7 -2.6
-2.5 -2.4 -2.3 -2.2 -2.1 -2.0 -1.9 -1.8 -1.7 -1.6 -1.5 -1.4 -1.3 -1.2 -1.1 -1.0 -0.9 -0.8 -0.7 -0.6 -0.5 -0.4 -0.3 -0.2 -0.1 0.0  0.1  0.2  0.3  0.4  0.5  0.6  0.7  0.8  0.9  1.0  1.1  1.2  1.3  1.4  1.5  1.6  1.7  1.8  1.9  2.0  2.1  2.2  2.3  2.4  2.5  2.6  2.7  2.8  2.9  3.0  3.1  3.2  3.3  3.4  3.5  3.6  3.7  3.8  3.9  4.0  4.1  4.2  4.3  4.4  4.5  4.6  4.7  4.8  4.9
5.0
       