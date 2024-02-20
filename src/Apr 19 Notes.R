# Categorical Vectors
color = c ('r', 'b', 'b', 'g', 'r', 'r'
   , 'g', 'b', 'b')
class(color)
fac_color = factor(color)
summary(color) ## tells you the length 
##  in each vvector
table(fac_color)

fac_color
head(?)


## Vars are small so we need more memory



shirts = c('S', 'S', 'XL', 
  'L', 'M','M',"L","L")
shirts
table(shirts)
fac_shirts <- factor(shirts, ordered=FALSE
       , levels = c('S', 'M','L', 'XL'))
fac_shirts
table(fac_shirts)
as.character(fac_shirts)

as.numeric(fac_shirts)
# Matrix
V=1:20
matrix(V, nrow=4)
matrix(V, nrow=4, byrow=TRUE)
m=matrix(V, ncol=6, byrow=TRUE)

# Single Value
m[1,3]
# Whole colmn or row
m[,3]
m[,2]

#Gives back 2nd and 3rd row
m[2:3,]
 #Only gives back the designated rows)
m[c(1,4),]

m2=m[,2:5]
m[,seq(2,6,2)]
m

m[2:3,3:4]

m[2:4, 1:2]
## 10 and 11 and 22 23
m[c(2,4), 4:5]
m[m<15]
m2 =m < 15
sort(m[m2])
m * 2
m - 5
m - m
m / m
m * m
m


#Naming Matricies
m

m4=matrix(seq(1,48,1), nrow = 6, byrow=T, dimnames =list(c(letters[1:6])))
m4
LETTERS
letters


#Dpylr package
library(dplyr)
install.packages("dplyr")
print("Hello")
#use str to see summary of data thus far
#using dyplr
filter(flights == 'DL')
head(married)
