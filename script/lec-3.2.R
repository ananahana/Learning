#install.packages("dtplyr") #when installed once, no need to install anymore. Call the library if you don't know wheather you have the package
library(data.table)
library(magrittr)
library(readxl)
library(dtplyr)
library(tidyr)
library(dplyr)


#############################################################################
colMax <- function(DT, colNames = NULL){
  if (is.null(colNames)) colNames = names(DT)
  DT[,sapply(.SD,max), .SDcols = colNames]
}

#which.min gives you the position of the first min . NOTICE, even if you have more min
# which gives you the location of all minumums
which_all_min <- function(x, na.rm =FALSE){
  which(x == min(x, na.rm = na.rm)) #which gives indexes of x
}


#this function gives all the indexes of where the max values are located (not the value)
which_all_max <- function(x, na.rm =FALSE){
  which(x == max(x, na.rm = na.rm)) 
}


best_attack = function(Poke1, Poke2){
  type1 <- poke_dt[Name == Poke1, Type] #identifying which pokemon type the selected pokemon is. Example ouf outcome:
  #> poke_dt[Name == "Bulbasaur", Type]
  #[1] GRASS  POISON
  type2 <- poke_dt[Name == Poke2, Type][1] # Consider only the 1st type. Outcome:  #[1] GRASS
  attacks = moves_dt[Type %in% type1 & Category %in% c("Special", "Physical")] #excluding "Status", 
  #as writing: moves_dt[Type %in% "GRASS" & Category %in% c("Special", "Physical")]
  #it will give all the rows having grass, special and physical
  mult = typeChart_dt[Attack %in% type1 & Defense == type2, ] #typeChart_dt[Attack %in% "GRASS" & Defense == "NORMAL", ]
  #output is all row that has that requirement
  setnames(mult, "Attack", "Type") #this one selects the attack type and the associated defense to that attack, this changes the name from "Attack" to "Type"
  attacks = left_join(attacks, mult[,.(Type, Multiplier)], by = "Type") %>% as.data.table() #leftjoin keeps all that is in the first entry (attack), 
  #and add the entries "Type" and "Multiplier" of the mult datatable. Since they are only 1 value, it's gona be recycled. It can be merged because "Type" is in commong 
  attacks[, power_mult := realPower * Multiplier] # creating an additional col power_multi
  attacks[which_all_max(power_mult)] #selecting all the rows that contain the max power_mult value
}

#Hard coded example
attacK <- moves_dt[Type %in% "GRASS" & Category %in% c("Special", "Physical")]
multi<- typeChart_dt[Attack %in% "GRASS" & Defense == "POISON", ]
setnames(multi, "Attack", "Type")
attacK = left_join(attacK, multi[,.(Type, Multiplier)], by = "Type") %>% as.data.table()
multi[,.(Type, Multiplier)]#is the col "Type" and "Multiplier" of datatable
multi
#once you created different data tables, you can call them in each other or in functions
##############################################################################


#opening files

(poke_file <- file.path("/Users/Anahana/Documents/MisgeldLab/Rwd/Lec3 table manipulation/pokemon.xlsx"))

#-------------------------------------------------------------------------------------


poke_dt <-read_excel(poke_file, sheet ="Pokemon")
poke_dt <- as.data.table(poke_dt)
colnames(poke_dt)[1] <- "Number" #renaming columns
colnames(poke_dt) <- gsub(" ", "_", colnames(poke_dt)) #sobstituting column char
poke_dt[, Number := gsub(intToUtf8(160),'', Number)] #gets rid of "intToUtf8(160)" in first col with nothing
poke_dt[, Number := as.integer(Number)] #transforms col N in integer class
poke_dt[, Type := as.factor(Type)]
grep("Mega", poke_dt$Name) #greps gives indeces of where what you are searching is
poke_dt <- poke_dt[ !(grep("Mega",Name))] #grep gives them the indexes, and with ! i choose all the rest
class(poke_dt)
head(poke_dt)
sapply(poke_dt, class) #tells you for each col called by name, what class they belong to
poke_dt[1,]
poke_dt[!1,] #how to use !
poke_dt[Number <= 150]
dim(poke_dt)
poke_dt[, uniqueN(Type)]
poke_dt[, unique(Type)]
poke_dt[, .N, by = Type] #this tells you how many types are there, devided by the type
poke_dt[Type == "ICE", ] # all ice pokemon, done in the row
stats <- c("HP", "Attack", "Defense", "Special_Attack", "Special_Defense", "Speed", "Total")
poke_dt[,.SD[which.max(HP)]] #gives first max value only for HP
sapply(poke_dt[,.SD, .SDcols = stats], max) #.SDcols
#poke_dt[,stats]#doesn't work because stats don't exists
sapply(poke_dt[, stats, with = FALSE], max) #with = F because stats are defiend outside
sapply(poke_dt[, ..stats],max) # the two dots replace the with = FALSE
poke_dt[,.SD[which.max(HP)], by = Name]
poke_dt[, .SD[1], by = Type] #give me the first row by type
colMax(poke_dt,stats)
colMax(poke_dt[, ..stats])
poke_dt[, .SD[which_all_max(Total)], by = Type]#strongest and weakest FOR EACH POKEMON TYPE, give index of the strogest, but considering the whole .SD
poke_dt[, .SD[which_all_max(Total)], by = Type] #difference compared to previous is that it gave also the ones that have the same value
poke_dt[, .SD[which_all_min(Total)]]
poke_dt[, .SD[which_all_min(Speed)], by =Type]
poke_dt[Type == "ICE"][order(-Speed)][1] #you can do several subsetting 
poke_dt[, mean(Total), by = Type][order(-V1)] #average of strongest type meaning compute the mean
#poke_dt[, .(meantot = mean(Total)), by = Type][order(-V1)]
poke_dt[,.N] #how many rows
poke_dt[.N] #last entire row




evolution_dt <-read_excel(poke_file, sheet ="Evolution")
evolution_dt <- as.data.table(evolution_dt)
colnames(evolution_dt)[1]<- "Name"
colnames(evolution_dt)[2]<- "Evolution"
colnames(evolution_dt)[5]<- "Evolution_Type"
setnames(evolution_dt, c("Evolving From", "Evolving to"), c("Name", "Evolution"))
colnames(poke_dt) <- gsub(" ", "_", colnames(poke_dt))
row.names(poke_dt)
evolution_dt[, .N] #390 pokemons
evolution_dt <- evolution_dt[1:150,]
head(evolution_dt)
dim(evolution_dt)
sapply(evolution_dt, class) #sapply does it recursively for each col
poke_dt[! Name %in% c(evolution_dt$Name, evolution_dt$Evolution), unique(Name)]
setdiff(poke_dt$Name, c(evolution_dt$Name, evolution_dt$Evolution)) #which of the first (x) are different from the second (y)

#MERGING
#you can't merge more than two data tables at the same time
#there are 4 different ways to merge the data table
#inner merge: discard those that are not in common
#full gives everythig
#left: give me everything from the data table 
#right: gives only in the second 
#NOTA BENE: it sortes the data so you might have different order form the first table (use JOIN instaed from dyplyir)
#merge leftjoin (poke, evolution).. it adds TO pokemone

join_dt <- dplyr::left_join(poke_dt, evolution_dt[, .(Name,Evolution, Level)], by = "Name") #:: terminology to tell R from which package to use command
join_dt <- as.data.table(join_dt)
class(join_dt)
evolution_dt[max(Level)] #pokemon that requires max level to eveolov
join_dt[,max(Level, na.rm =T)] 
join_dt[, .SD[which_all_max(Level, na.rm =TRUE)], by = Type]





moves_dt <- read_excel(poke, sheet = "Moves")
moves_dt <- as.data.table(moves_dt)
moves_dt[1,]
colnames(moves_dt)[3] <- "Category"
colnames(moves_dt)[5] <- "Accuracy"
colnames(moves_dt)[9] <- "Probability"
setnames(moves_dt, c("Cat.","Acc.", "Prob. (%)"), c("Category","Accuracy", "Probability"))
sapply(moves_dt, class) #tells you for each col called by name, what class they belong to
moves_dt[, Probability := as.numeric(Probability)]
moves_dt[, Accuracy := as.numeric(Accuracy)]
sapply(moves_dt, class)
moves_dt[,Accuracy]
moves_dt[,unique(Category)] # to see the type of categories
moves_dt[,uniqueN(Category)]
moves_dt[, .SD[Type== "GRASS"]]
Ty <-moves_dt[, .N, by = Type] #number of instances for each type
#grassmoves <- moves_dt[Type == "GRASS", .(GRASS = unique(Name))] #how many moves of the grass type. All grass attacks returns datatable
grassmoves <- moves_dt[Type == "GRASS", unique(Name)] #returns character list
poisonmoves <- moves_dt[Type == "POISON", unique(Name)]
flyingmoves <- moves_dt[Type == "FLYING", unique(Name)]
normalmoves <- moves_dt[Type == "NORMAL", unique(Name)]
psymoves <- moves_dt[Type == "PSYCHIC", unique(Name)]
moveTypes <-moves_dt[,unique(Type)] #as vector, list of movetypes
as.character(grassmoves)
paste0(grassmoves,collapse = " , ")
# lenGra <- grassmoves[,.N] #to count rows of the data table
x <- as.data.table(grassmoves, poisonmoves, flyingmoves, normalmoves, psymoves) #concatenetes all in a unique list
L <- length(x)
listM <- c( 
  length(grassmoves),
  length(poisonmoves),
  length(flyingmoves),
  length(normalmoves),
  length(psymoves))

n <- max(listM)
dt <- data.table(Grass = grassmoves[1:n], Poison = poisonmoves[1:n], Flying = flyingmoves[1:n], Normal = normalmoves[1:n], Psychic = psymoves[1:n])
list <- c(list(poisonmoves), list(flyingmoves)) #list of lists
lapply(list, length) #extracts length of each list in list "list"
#list <- c(.(poisonmoves), .(flyingmoves)) does not work
type_attack<- moves_dt[,.(Name), by = Type] #as data.table

#which are the attacks that bulbasaur can do?
bulbAttackTypes<- poke_dt[Name == "Bulbasaur", unique(Type)]
bulbamoves <- moves_dt[Type == "GRASS" | Type == "POISON", unique(Name)] #returns character list
bulbamoves <- moves_dt[Type %in% bulbAttackTypes, unique(Name)]
moves_dt[Type %in% poke_dt[Name == "Bulbasaur", Type]] %>% head


moves_dt[ , realPower := Power * Accuracy /100]
#moves_dt[, .SD[, max(realPower, na.rm = TRUE)], by = Name] #wrong because for each different name! it gives the max
moves_dt[, unique(realPower)]

moves_dt[,.SD[which_all_max(realPower, na.rm =TRUE)]] #gives the same results as the one below!
moves_dt[which_all_max(realPower, na.rm = TRUE)] #because "which" gives the index, so if you subseti it in the rows, it gives all rows
moves_dt[,.SD[which_all_max(realPower, na.rm =TRUE)], by = .(Category, Type)]


typeChart_dt <- as.data.table(read_excel(poke_file, sheet = "TypeChart"))
head(typeChart_dt, 30)
sapply( typeChart_dt, class)
typeChart_dt[Attack == "FAIRY" & Multiplier > 1]

sumMulti_A <- typeChart_dt[, lapply(.SD, sum), by = Attack, .SDcols = "Multiplier"] #strongest in attacking is Ground
sumMulti_D <- typeChart_dt[, lapply(.SD, sum), by = Defense, .SDcols = "Multiplier"] #strongest in defense
which.max(sumMulti[,Multiplier])
sumMulti[6]
sumMulti_A[, "name" := c("sumMulti")]
merge(sumMulti_A, sumMulti_D, all = TRUE)
DT_Multi <- sumMulti_A[,c("Defense", "Multiplier_D") := .(sumMulti_D[,Defense], sumMulti_D[,Multiplier])]
myList <- list(1:10, 11:20, 5:14) #list of lists
































