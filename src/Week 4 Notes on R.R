print("Hey what's up guys")
getwd()
data2<- read.csv("Mroz.csv", TRUE, ",")
summary(data2)
devtools::install_github("tidyverse/dplyr")
install.packages("dplyr")
library(ggplot2)
library(tidyverse)
dataJew <- read.csv(file="Mlb1.csv", TRUE, ",")
dataJew
names(dataJew)
temp <- rowSums(dataJew[,c("frstbase", "scndbase", 
                           "shrtstop", "thrdbase", 
                            "outfield", "catcher")])
attach(dataJew)
names(dataJew)
temp
min(temp)
max(temp)
dataJew$position <-"First Base"
dataJew$position[dataJew$scndbase == 1] <- "Second Base"
max(dataJew$salary)



## Using the official P i p e  Command
## Bruh
#sum(select(filter(babynames,sex=="M",name=="Taylor"),n))
dataJew%>%group_by(position)%>% summarise(mean(salary, na.rm=TRUE))

####1###
#Var
print("Reserved in memory")
#Data Types
print("3 types that I know of Integer, Numeric
      Boolean and Character, but char doesn't have
      any functions or methods with it")
## 2 . ....##
print("5 types Vector
      Matrix
      Array
      List 
      Data Frame")
print("Vector is a sequence of data elemetns fo the same bsic type:
--5 Atomic Vectors which are classed as vectors(5)")
    "Logical-True or False"
    "Integer--15L (signified by L), 30L, 1699L"
    "Numeric ---5, 3.1415926535, 44545423"
    "Complex---- 4+3i, 8+7i"
    "Character-----'A', (''Hey''), 'TRUE'"

    vrt <- c(TRUE, FALSE)
    #class shows which class
    class(vrt)
    
    
    

    

print("Matrix are the R objects in which
the elements are arranged in a 2Dimensional
rectangular layou")
"Syntax --> matrix(data--is the input vector which becomes the datae elments of the matrix
,nrow - is the number of rows to be created,
ncol is the number of columns to be created
,byrow -- is a locial clue. If TRUE then teh input vector elemtns are arranged by row. --->  <- --->
dinames is the names assigned to the rows and columns)"
mtrx = matrix(c(5:29),5,5)
mtrx


print("Array are the R dataa objects which 
      can store data in more than > 2Dimensions")
"Synatx---> array (data, dim, dimnames)"
arr<- array(c(1:9) ,dim=c(3,3,4,2))
print("the 3 x 3 matrix will then
      be stored into a 4 x 2 matrix.")



print("List are the objects which contain elments
      of different types like - numbers, strings,
      vectors and ANOTHER LIST inside it")
"Synatx --> list(data)"
print("examples:
      vtr<- c(1:5)
      vtr1 <- c('hi', 'Hello', ''How are you'')
      vtr2 <- c(TRUE,TRUE,FALSE,FALSE)
      mylist<- list(vtr1,vtr2,vtr)")
vtr7<- c(5.68, 23,95,31)
vtr8<- c("Hey", "How are you", "Thank you!")
myList<- list(vtr8,vtr7)
print(myList) # all the properties 
#of the variables are kept 
#inside the list (nothing classes or casted into anything else)



## So some methods for Lists are merge(vtr) reg
## regardless of the types inside (but cannot be sorted)

print("Data Frame is a table or a 2Dimensional array-like structure in which
      each column contains values of one variable
      and each row contains one set of values from each column")
"Syntax --> data.frame(data)"
#data.frame(mtcars)
vtr<- c(1:5)
vtr2=c("Neel","Adam","Adi","Dork","nigger")
vtr3<- c(15,25,53,145,74)
data.frame(vtr,vtr2,vtr3)
data.frame(airquality)



print("Ah yes the factor variable:
      they are the data objects which are used to categorize
      data and store it as levels.
        They can store both strings and ints.
        And lasty they are useful in data anal for stat modeling")

"Syntax --> data=c('East', 'West', 'North', 'East', West'')"
          "factorData<- factor(data)"


#Operators
print("Operators are the constructs which can manipulate 
      the value of operands, Arithemtic Ops, Assignment,
      Logical Ops, Relational Ops")
"Arithemetic -- exponent = a^b
            Modulus = a %% b
            Floor Division = a % / % b -- basically rounds to the previous whole
                    number even if it's at 999"
"For Logical Operators...
it's just a & b,  
          a | b
          !a.... if you want to compare element to element
                  otherwise it compares the whoel fuckin thing"




#Conditional Statements
print("Cond Statements: 
      The uhhh Switch case systems")

"Syntax --> switch(Expression

case =  #Statement1
case =  #Statement2
case =  #Statement3

default Statement
)  "

switch(8, #have a comma 
       #######infront of your value
          #and comma after every 
       ###case except last
'1' =print("Monday"),
'2' = print("Tuesday"),
'3'= print("Wednesday"),
'4'=print("Thursday"),
'5'=print("FriYAY"),
'6'=print("Sat"),
'7'=print("Sunday"),
print("Invalid input Nigga")
)





#Loops
print("Repeat Loops:
      repeats a stement or group
      of statements while a given condition is TRUE.
      It tests the condition after exeucting hte loop body")

"Syntax --> repeat {
              commands
              if (condition) {
                  break
                    }
                  }"


varry = 5
repeat{
  print(varry)
  varry=varry+2
  if(varry >21){
    break
  }
}

#Strings
"Some Functions for dem string beans"
"paste(string) is just how you concantenate 
a string (string1,string2)"

"nchar(str) -- tells you how many characters are in the string"

"str--toupper/lower(str)"

"str -- substr(str,4,8)"
stringB = "Lmao Gay"
##########012345678
substr(stringB,2,4)
nchar(stringB)


#Functions
print("some factor practice")

direction =c("Up", "Left", "Right","Down", "Left","Up")
factorDire = factor(direction)
is.factor(factorDire)## Some more notes LOL##
factorDire
levels(x=factorDire)
dow=c("M","Tu","W","Th","Fr","Sat","Sum")
wDays=c("Tu","Th","M")
wdFact = factor(x=wDays,levels = dow, ordered = T)
wdFact
print("pretty much removes duplicates with each level")
print("EXIT ALL OF THESE")
i=0
lol = (1:5)






######### Week 5-6 Reflection be like ####
basefile.choose()

