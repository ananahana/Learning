codes <- c(rep("a", 3), "c", rep("b", 3), "c", rep("a", 3))
cod<- data.frame( 
  +     code = c("a", "b", "c", "u"),
  +     weather = c("sun", "rain", "wind", NA))

for (i in codes) {(print(cod$weather))}

weather <- rep (NA, length(codes))
for (i in 1:length(codes)){
  if (codes[i] =="a"){ 
    weather[i] <- "sun" 
  } else if (codes[i] =="b"){
    weather[i] <-"rain"
  } else if (codes[i] =="c"){
    weather[i] <- "wind"
  } else if (codes[i] == "u"){
    weather[i] <- NA}
}

#x is the argument of the fuction; looks only at the first argument, so if you have a vector, it will only look at first argument of the vector
# return gives r, after it has done what is in the {} brackets. NB the return value cannot be c
#if you pass a code not known to the fuction it will return the last line (es in this case NULL (because x not "u"))

code2condition <- function(x){
  if (x =="a"){ 
    r <- "sun" 
  } else if (x =="b"){
    r <-"rain"
  } else if (x =="c"){
    r <- "wind"
  } else if (x == "u"){
    r <- NA}
  return(r) 
}
code2condition("a")

#you can pass a funtion as an argument to another function
#you can use an anonymous function (not defined)

sapply(codes, code2condition) #looks if types are always equal in length and then concatenates the answer in one line. moreover, it gives a name at each of them.
lapply(codes, code2condition) #conserves the original structure of the arguement. i.e lenght and order. More flexible to use for non homogenous data (or when you don't know them)
X <- matrix(1:100, ncol=5)
apply(X, c(1,2),sd) # this would try to calculate on the single value
apply(X, 1 ,sd)
apply(X, 2 ,sd)
apply(X, 1, mean)


string <- "This is a string"
paste(string, "NOT!", sep = "...")
dir_name <- "mydir"
base_name <- "myfile"
extension <- "txt"
paste0(dir_name, "/", base_name, ".", extension)
(wordVector <- strsplit(string, split = " "))
paste(wordVector[[1]], collapse = " ")
grep("is", wordVector[[1]], value = TRUE, perl=T)

#-----------------------------
  
#GET FILES

# target directory
DATADIR <- file.path('./extdata')
# url of source file, telling it where to store it. curl fetches data from web

titanic_url <- paste0('https://public.tableau.com/s/sites/default/files/media/',
                      'titanic%20passenger%20list.csv') # target file for downloaded
titanic_file <- file.path(DATADIR, 'titanic.csv')
download.file(titanic_url, destfile = titanic_file, method='curl') list.files(DATADIR, pattern="tita")

#reading it only, but not nicely structured. Puts all file in memory because put in a single line
titanic_vector <- readLines(titanic_file)


#can also read it line by line
cmd <- paste0("wc -l ", titanic_file, " | awk '{ print $1 }'") n <- system(command = cmd, intern=TRUE)
con <- file(description=titanic_file, open="r") # open connection for(i in 1:n) {
tmp <- readLines(con, n=1) # do something on a line of data }
close(con)

#since most data are already formatted, this is a better way. you can tell what is the separetor: header =T means that first row are the col names
# to change the decimal place annotation use "dec". e.i dec = "."

titanic_df <- read.table(titanic_file, sep=',', header=TRUE) class(titanic_df)

#usually R converts strings/characters as factors. Sometimes you just want the normal char.
#by using stringAsFactors = FALSE

#to make the reading faster use colClasses by specifying in advace which class each col is supposed to be

#for large files use the library (data.table) and read it with "fread"


tmp_tidy_table <- "1_colname,2_colname,3_colname
  3,4,5
a,b,c"
read.csv(text= tmp_tidy_table)
read.csv(text= tmp_tidy_table, check.names = FALSE)

tmp_messy_table <- "# This line is just useless info
  1_colname,2_colname,3_colname
3,4,5
a,b,c"

read.csv( text = tmp_messy_table,  check.names =F, comment.char= "#")

#if problem of read file, just skip lines by skip=1

T_df <-read.csv("./extdata/titanic.csv")
#NOTA Bene, Per essere in grado di aprire il file devi darli il path o mettere il file nella current working directory
#tit_df, survived ==1&age ==max()

poke_file <- file.path(DATA, 'pokemon.xlsx')

library(readxl)
?read_xlsx
(poke_file <- file.path("./extdata/lectures-WS1819_extdata_pokemon.xlsx"))

poke_df <- read_excel(poke_file, sheet=1, n_max =10, range= cell_cols(2:4))
poke_df
(oly_file <- file.path('extdata/lectures-WS1819_extdata_summer_olympic_medals.xlsx'))

oly <- read_excel(oly_file)
head(oly)

#covert to data table
oly_dt<-as.data.frame.table(oly)
head(oly_dt)

bronze <- oly_dt[Freq.Medal == "Bronze"]
unique(oly_dt$Freq.Gender)

#table function returns how often values is occurring

# XML also describes what the data are, HTML only display data

library(XML)
plant_path <- file.path('extdata/lectures-WS1819_extdata_plant_catalog.xml')
doc = xmlTreeParse(plant_path, useInternalNodes = TRUE)
(root = xmlRoot(doc))


botanical_name<- xpathApply(doc, "//*", xmlName) #means to get all elements, and you need the names that are unique
unique(unlist(botanical_name)) #unlists unpackes the list if u have many lists into each other to make only 

#get plants of zone 4

zone4Plant <- xpathApply(doc, "//PLANT[ZONE=4]", xmlToList)
zone4Plant

library(RSQLite)
#Connect to the extdata/Northwind.sl3 SQLite data base
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname="extdata/lectures-WS1819_extdata_Northwind.sl3")

#Inspect the data base tables using the ‘dbListTables’ and ‘dbListFields’ functions. 
dbListTables(con)
dbListFields(con, "Customers")

#Put together a SQL statement to retrieve a table that lists for 
#all customers 


#(name of the company, name of the contact person and city)
#all the products (name of the product) that they ordered
dbGetQuery(con, "SELECT companyname, contactname, city from customers limit 5")

tab1 <- dbGetQuery(con, "SELECT companyname, contactname, city from customers")
tab2 <- dbGetQuery(con, "SELECT orderID, productName from 'order details extended'")

dbWriteTable(con, "copy_of_customers", tail(tab), append=TRUE)
dbListFields(con, "Products")
dbListFields(con, "Order Details")
dbListFields(con, "Orders Qry")
dbListFields(con, "Orders")
dbListFields(con, "Order Details Extended")


tab <- dbGetQuery(con, "SELECT customers.companyname, customers.contactname 
                  from customers inner join 
                  orders on customers.customerid
                  orders.customerid inner join
                  'order details' on orders.orderid = 'order details'.orderid inner join products on 'order details'.productid = products.productid")

nrow(tab)
tab[1.5,]
