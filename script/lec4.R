library(data.table)
library(magrittr)
library(ggplot2)
library(tidyr)

##ON TIDY DATA
#If you organize the data by observation (each obs class is in a column), it's easier to plot/quantify
#depending on the analysis, what is the observation? organize table in order to have that observation on the rows
#col = VARIABLES
#row = OBSERVATION  
#each cell = VALUE
#don't have values (or other important operations) as a name of the column
#PIPE OPERATOR %>%, you need the packgage dplyr
#"erwr_qweqwe" %>% gsub("_", " ", .) #the dot tells the place where pipe should be placed.
#rbindlist(dt_list, ID )
# #
# data.tables has some operations included. such as melt and dcast
# melt brings together two tables into one
#dcast value.var = which col of the orginal table contains the value
# tidyr::separate
# quiz:
#   melt: puts columns into one
# unite: year month day
# cast: tmax and tmin in different col

path <- file.path(getwd())
path


filepath <- file.path("/Users/Anahana/Documents/MisgeldLab/Rwd/extdata/example_product_data.csv")
messydt <- fread(filepath)


tidydf <- (melt(messydt,id.vars = "name",
             #measure.vars = c("producta","productb"),
             variable.name = "product name",
             value.name = "count"))

#gather the variables producta and productb under the name 'product', add the value name "counts"
messydt %>% gather('producta', 'productb', key = 'product', value = 'counts')

#melt , gather - long
#dcast, spread - wide


weather<- fread("extdata/weather.txt") %>% as.data.table(.)
weather


# melt(weather_dt, id.vars = c("id", 'year', 'month', 'element'), variable.name = "day")
# (melt(weather_dt, id.vars = c("id", 'year', 'month', 'element'))) # works but new col does not have a name
## gather everything but what selected with "-"
weather_dt<- weather %>% gather(-c(id, year, month, element), key = "day", value = "temp") %>% as.data.table(.) ##NB "gather"coverts into data.frame
weather_dt
class(weather_dt)

## first sobstitute the day values without the "d"
weather_dt <- weather_dt[, day := as.integer(gsub("d", "", day))] 
weather_dt <- weather_dt %>% unite(col = "date", year, month,day, sep = ".", remove = TRUE)
weather_dt <- spread(weather_dt, element, temp) 
## some days have NA temp, remove them
weather_dt <- weather_dt[!(is.na(TMAX) & is.na(TMIN))]

weather_dt <- weather_dt[, day := as.integer(gsub("d", "", day))] %>% unite(col = "date", year, month,day, sep = ".", remove = TRUE) %>% spread(element, temp)

## some days have NA temp, remove them
weather_dt <- weather_dt[!(is.na(TMAX) & is.na(TMIN))]




####################################################### read recursively different files in a folder

path
files <- list.files("extdata/baby-names/", full.names = TRUE)
files
#dirname(files[1]) #returns director names
#class(read.csv(files[1])) #data frame

basename(files[1]) #returns filename as chr, you want to add this as new info to file you are opening
f1 <- fread(files[1])
f1[, filename := basename(files[1])]


#append all files into one table with a function

read_append <- function(file) {
  dt <- fread(file)
  dt <- dt[, filename := basename(file)]
  return(dt)
}
# see if it works for one file
read_append(files[1]) %>% head
 
#do it for all files

#lapply(to what, which function)
#creates list of lists

dt <- lapply(files, read_append)
dt <- rbindlist(dt)

#separate year and gender in two col

dt <- separate(dt, col = "filename", into = c("year","gender"), extra = "drop") # extra is used because you don't want the cvs to be another col

################################################################################ small study case


original <- fread("extdata/gene_expression.tds.txt")
dim(original)
head(original, n =2)
colnames(original)
original[, NAME][3]

more <- original %>% separate( col = "NAME", into = c("gene_name", "biological_process", "molecular_function", "systematic_name"), sep = "\\|\\|", extra = "drop")


#remove unwanted columns GID, YORF, GWEIGHT
cleaner <- more[, c("GID", "YORF", "GWEIGHT") := NULL]
cleaner[1]
melted <- melt(cleaner, id.vars = c("gene_name","biological_process","molecular_function","systematic_name"), variable.name = "somevar")

#need to take the rate out of somevar >>> need to separate
sep <- separate(melted, col = "somevar", into = c("nutrient", "rate"), sep = 1) #sep =1 separates after first character

#add meaningulnames to nutrient
nutrient_names <- c(G = "Glucose", N = "Ammonia", P ="Phosphate", S = "Sulfate", U = "Uracil")
nutrient_names["G"] #way to obtain the value of the list throught the name
#in the table, the col nutrient is a list with all the letters, so by doing "nutrient_names[nutrient]" you are accessing that col and giving the
#associated value for each letter

gene_expression <- sep[, nutrient := nutrient_names[nutrient]]





