library(openintro)
library(dplyr)
library(ggplot2)

data(ncbirths)
ggplot(ncbirths,aes(x=weeks, y = weight))+ geom_point()
ggplot(ncbirths,aes(x=weeks, y = weight))+ geom_jitter()

print("I am done with this")
color= c('r','b','r','r','g','g','b')
class(color)
fac_color= factor(color)
summary(color)
table(color)
("no order in categorical")
v=1:20
m=matrix(v,nrow=6,byrow=TRUE)


#Slicing in vectors

m[1,3]
m[,3]
## With Seq you can increment or count by so every other
m[,seq(2,6,2)]





library(nycflights13)
install.packages('nycflights13')
library(nycflights13)

#select()
df<-select(flights,carrier,dep_delay)

#filter
?nycflights13
filter(df, carrier=='UA')
df<-filter(df,carrier=='UA')
mean(df$dep_delay, na.rm=TRUE)
#looking at the rest of the data in one thing
mean(flights$dep_delay, na.rm=TRUE)
notUA<-filter(flights, carrier!='UA')
#Excluding a certain varibalbe
mean(notUA$dep_delay, na.rm=TRUE)


#Filter UA and AA
data_ua_aa =filter(df,carrier %in% c('UA', 'AA'))
df
names(flights)
unique(data_ua_aa)
unique


# Filtering w/ Python
# %>% to add functions together
flights %>% select(carrier, dep_delay) ## Same as the one below

select(flights, carrier, dep_delay)

flights %>% select(carrier, dep_delay) %>% filter(carrier == 'UA')

## Slicing things with just selcting rows
slice(flights , c(1,3,6)) ## gets the rows you want
flights[c(1,3,6),]


#Find the carrier with the largest dep_delay
max(flights$dep_delay, na.rm=TRUE)
#finds the row where the max is
idx_row<-which.max(flights$dep_delay)
# now you need to find out who that belogns to

flights[idx_row, ]
head(flights[idx_row, ])
## you get a row so you need to subset afterwards
flights[which.max(flights$dep_delay),c('carrier', 'dep_delay')]


flights%>% slice(which.max(dep_delay))
#mutate create a new col or var
total_Delay<-flights%>%select(carrier,dep_delay, arr_delay)%>% mutate(total_delay=dep_delay + arr_delay)
total_Delay
flights%>%select(carrier,dep_delay, arr_delay, distance,air_time) %>%
  mutate(total_Delay=dep_delay+ arr_delay, speed=distance/air_time*60)-> a
a## cuts off some values so maybe we can see it
a
a %>% arrange(desc(speed))
a %>% arrange((speed))
a %>% arrange(desc(total_Delay), desc(speed))

# summarize()
mean(a$total_Delay, na.rm=TRUE)
summarize(a, mean_td=mean(total_Delay, na.rm=TRUE))
a%>% group_by('carrier')%>% summarize(mean(total_Delay, na.rm=TRUE))
# created 16 groups above
# shows avg amt above now
a %>% arrange(desc(total_Delay), desc(speed))



## now doing summarize again
## summarize gives a data frame while mean just gives back a value (Numeric)

## groupby
a%>%group_by('carrier')
a%>%group_by(carrier)%>%
  summarize(mean(total_Delay, na.rm = TRUE))
# avg of delays above
a%>%summarize(mean(total_Delay, na.rm = TRUE))

# going over gwens's bs with count
a%>%group_by(carrier) %>% count()
a%>% group_by(carrier) %>% summarize(mean_td = mean(total_Delay, na.rm = TRUE),N = n())


#month = May fidn the largest carrier with arr_delay (In flights data)
names
names(flights)


## we had to use FILTER??
## and group by???
## and summarize???
## mean...??
flights%>%filter(month=='5') %>% #looking whatever values correspond for the month of may
  group_by(carrier) %>% # to grab that subset of data
  summarize(mean_arr_delay=mean(arr_delay,na.rm=TRUE))%>% # to put the mean in
  arrange(mean_arr_delay) #to order it

# create a data frame or whatever where you can seee
#     total average delay for all the carriers for the 12 month period
        ## and report the airliine with the smallest average total delay(arr_delay + dep_delay)
##            ...? Which month is it?
flights %>%
  mutate(total_delay=arr_delay+ dep_delay)%>%
  group_by(carrier,month)%>%
  summarize(mean_td=mean(total_delay, na.rm = TRUE )) %>%
  arrange(mean_td)

flights

names(flights)
attach(flights)
type(day)
names(flights)
dataFlights <- flights
dataFlights
typeof(tailnum)
str(dataFlights)


unique(dataFlights)
dataFlights%>%group_by(dest)
dataFlights2<-dataFlights%>%filter(dest == 'LGA')
dataFlights%>%filter()%>%group_by(carrier)%>%count()
dataFlights%>%filter(origin == 'LGA')
attach(nycflights13)
names(nycflights13)
names(flightsData)
airports<-airports
names(airports)
airports
f1_new <- flights%>%
  mutate(total_delay=dep_delay+arr_delay)%>%
  select(month,carrier,origin,dest,distance,total_delay,arr_delay)



data2<-read.csv(file.choose(),sep=',', header=TRUE)

names(data2)
data2
names(data2)
