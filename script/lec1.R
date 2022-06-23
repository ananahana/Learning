x<- rnorm(1000)
library(ggplot2)
ggplot(data=data.frame(x=x), aes(x))+
         geom_histogram()

summary(cars)

FAC <- gl(3, 10, labels = c("Rita Repulsa", "Lord Zedd", "Rito Revolto"), ordered = FALSE)
FAC[1] <- c("Shredder")
FAC[1] <- "Shredder"
mat <- matrix(1:12, 3, 4)
mat

mat<- matrix(1:50, 10, 5)
rownames(mat)<-paste0("nrow_", 1:nrow(mat))
colnames(mat)<-paste0("ncol_", 1:ncol(mat))
col_mean_mat<- colMeans(mat)
row_mean_mat<- rowMeans(mat)
mat<-rbind(mat, seq(60,100,10))
mat2 <- matrix(rnorm(length(mat), 3,1), 11, 5)
rownames(mat2)<-paste0("nR_", 1:nrow(mat2))
colnames(mat2)<-paste0("nC_", 1:ncol(mat2))

mat3 <- matrix(sample(100, length(mat), replace = TRUE), 11, 5)
subtractedMat <- mat - mat3
M <- cor(subtractedMat, method = "spearman")
corrplot::corrplot(M, method = "number")

sample(1:10)
m <- rbind(m, seq())
n<- matrix(sample(100, length(m), replace=TRUE), nrow(m), ncol(m))
n


X <- as.list(LETTERS[1:3])
data.frame(number = 1:4, letter = letters[1:4])


vec_1 <- 1:10
names(vec_1)<- letters[1:10]
names(vec_1) <- letters(vec_1)
names(vec_1)<- letters[1:10]


f1 <- factor(letters)
levels(f1) <- rev(levels(f1)) #each level is associated with a factor, here you are changing the order of the levels so also the associated factors are sorted accorting to order of levels
f2 <- rev(factor(letters))#here you are not changing the level order, but only telling it to create a factor where the letters are revered
f3 <- factor(letters, levels = rev(letters))# here you explicity say your factors should contain letters, but the levels should be in the reverse order. It first creates the letters then the level name.

myvec1 <- 1:10
myvec2 <- seq(from=1, to=11)
myvec3 <- seq(from=1, to= 24, by=2) #type double instead of integer like the previous two.
names(myvec1) <- letters[1:length(myvec1)]
names(myvec2) <- LETTERS[1:length(myvec2)]
names(myvec3) <- letters[myvec3]

typeof(myvec1)
typeof(myvec2)
typeof(myvec3)


mylist <- list(myvec1, myvec2, myvec3)
attributes(mylist)
as.data.frame(mylist)#fails because not of the same lengths


mylist[[1]] <- c(mylist[[1]], NA, NA)
mylist[[2]] <- c(mylist[[2]], NA)
df <- as.data.frame(mylist) #works when run again

names(df) <- LETTERS[1:3] #assign names to the df
df
aa <- seq(1:24)

aa <- seq(1,26)
bb <- seq(4, 104, by = 4)
cc <- rep(seq(1, 26, 2), each = 2) #create a list containinig each 26 values
df <- data.frame(V1 = aa, V2 = bb, V3 = letters[cc]) #combine them in a data frame assigning to each the names, 
#letters[cc] returnes the letter associated to each number in cc list, es 1 = a
df
row.names(df)<- letters[1:nrow(df)]
colnames(df)<- LETTERS[1:ncol(df)] 

#the previous excercise can also be done in the following way

attr(df, "names")  <- letters[1:3]
attr(df, "row.names") <- LETTERS[1:26]


#or

names(df) <- LETTERS[1:3]
row.names(df) <- letters[1:26]

#coerce the class to a list
attr(df, "class") <-"list"
attr(df, "class") <-"hello kitty"

head(df)
tail(df)

dd <- rev(rep(seq(1, 26, 2), each = 2))
ee <- seq(0,1.6,length.out = 26)
df2 <- data.frame(V4 = dd, V5 = ee)
df-df2<- cbind(df,df2)
#---------
  
M <- data.frame(matrix(1:12, 3, 4))
colnames(M) <- paste0("col_", 1:ncol(M))
#access row2, column 2 and 4:
M$X2[2]; M$X4[2] #this version extracts the values singularly
#or
M[2,c(2,4)]
colnames(M) <- paste0("col_", 1:ncol(M))
row.names(M) <- paste0("row_", 1:nrow(M))
M$col_1<- 0
M$col_4<- paste0(0)

#alternetively, to use one single function
M[,3:4] <- paste0(0)
M[,paste0("col",3:4)] <- 0 #this adds two additional columns called col3 and col4 which contains 0
M[,paste0("col",3:4)] <- 0 #this replaces the two existing columns, advantage that you don't need to know which index column number they actually have

M[,3:4]<-0 #this already replaces coloumn 3 and 4 with 0
M
#subsetting
x <- c("a"=1, "b"=2, "c"=3, "d"=4, "e"=5)
x[c(3,5)]
x[-c(1,2,4)]
x[c(FALSE,FALSE,TRUE,FALSE,TRUE)]
x[c("c", "e")]


vals <- outer(1:5, 1:5, FUN = "/")
select <- matrix(ncol = 2, byrow = TRUE, c(
  5, 1, #this selects the element locatd at row 5 of column 1
  4, 2 )) # this selects element row4 col 2

vals[select]

x <- matrix(1:20, ncol=2)
x[,1]
x[1, , drop = T] #data simplified to a vector
x[1, , drop = F]
y <- as.data.frame(x)
y
y[,1] #selects coloumn 1 of dataframe
y[[1]]
y[1] #selects col1 preserving data structure

x[[1]] #selects row1 col1 of matrix
x[1] #like above

Lookup <- data.frame(
  averageTemperature = c(5, 7, 10, 0, 3),
  desc = c("cloudy", "rainy", "sunny", "snowy", "windy"),
  goodForSki = c(T, F, T, F, F)
  )

WeeklyCast <- c("rainy", "rainy", "cloudy", "windy", "snowy", "cloudy", "sunny")

weeklytable <- data.frame(averageTemperature=c(7,7,5,3,0,5,10),
                          desc= c("rainy", "rainy", "cloudy", "windy", "snowy", "cloudy", "sunny"),
                          goodForSki =c(F,F,T,F,F,T,T))
weeklyTable #the outcoume you want to get using loopup and weekly cast

#create weekly table using rownames
rownames(Lookup) <- Lookup$desc #assign row names to lookup to have the same name as desc
Lookup[WeeklyCast,] #replace the rows by the name of the weeklycast

#use the match function (i don't know how to use match)
id <- match(weeklyCast, lookup$desc)
lookup[id,]

#order the rows of loopup table by column of desc
lookup$desc
lookup[order(lookup$desc),] #comma is needed to tell you want the whole table


bigDF <- as.data.frame(matrix(0, ncol=1500, nrow=1500))
colnames(bigDF) <- paste0("Column_",1:dim(bigDF)[2])

#select even columns of bigDF

SelCols <- paste0("Column_", seq(2, 1500, 2)) ###seq(2, 1500, 2) #start from 2 to 1500, by two
a <- bigDF[SelCols]
a

a <- c(1,23,4)
a[-2]#tolgo il secondo valore

#select all col but 76, for dataframes, use setdiff #what is different (also "union" #without repetition, "intersect" #what is in common,"setequal")
allbut76 <- bigDF[setdiff(names(bigDF), "Column_76")]

#select rows where columns Column_1 or Column_2 are 1 by using the subset() function
subset(bigDF, Column_1 ==1 | Column_2 ==1)

#assign 1 to 500 randomly selected diagonal indices
randSample <- sample(1:1500, 500)
bigDF[cbind(randSample,randSample)] <- 1

#retrieve the row and column indices of the elements which has been assigned 1
c<- which(bigDF == 1, arr.ind = T)

#compute number of women who survided titanic

tab <- read.csv("https://www.gagneurlab.in.tum.de/fileadmin/w00bxk/www/dataset/titanic.csv")

tab
#important columns are $survived (either 0 or 1) and $sex (female | male)
#select only rows that are female
onlySurvivedFemale <- subset(tab, sex == "female" & survived == 1)
sum(onlySurvivedFemale$survived)  #339

onlyFemale <- subset(tab, sex == "female")
sum(onlyFemale$survived)#339

onlymale <- subset(tab, sex == "male")
sum(onlymale$survived) #161


sum(tab$survived) #500

nrow(tab)#1309
nrow(onlymale)#843
nrow(onlyFemale)# total of females 466



