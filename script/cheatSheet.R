library(data.table)
(DT <- data.table(x = rep(c("a","b","c"), each = 3), y = c(1, 3, 6), v = 1:9)) #note that y values are only 3, becuase too short they are reclycled


DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3], 
                 V3=round(rnorm(4),4), 
                 V4=1:12)
DT 

DT[, .(sum(V4)), by = V1]
DT #no need to call it here 

(DT[1:5,])
(DT[,.(V2 =="A")]) #this appends a col where it tells if it is true that V2 at that position is A
(DT[V2 == "A"]) #selects all rows that have A in V2

(DT[V2 == c("A", "C")])  #i wanted to select rows that have A or C, but it doesn't work
(DT[V2 %in% c("A", "C")]) #this is how you select multiple values
(DT[,V2])
(DT[,.(V2)])
DT[,.(V2,V3)]
(DT[,sum(V1)])
(DT[,.(sum(V1), sd(V3))])
(DT[,.(sum_1 = sum(V1), sd_3=sd(V3))])
(DT[,.(V1, sd_3=sd(V3))])
DT[,.(print(V1), plot(V3), sumV4 = sum(V4), V4)]
DT[,.(sum(V4)), by = V1]
DT[,.(sum.V4 = sum(V4)), by = .(V1, V2)] #no spaces allowed in name title

sign(-2:3)  # > (-2:3) [1] -2 -1  0  1  2  3 # -1 -1 0 1 1 1. sign gives -1 when negative, 0 when 0 and 1 when positive

DT[,.(sum.V4 = sum(V4)), by = .(sign(V1-1))]
DT[,.(sum.V4 = sum(V4)), by = .(V1.01 = sign(V1-1))]
(DT[1:5, .(sum.v4 = sum(V4)), by = V1])



DT[, .(count.row = nrow(DT)), by = V1] #this counts the rows first, then reclycles the count for V1 groups
DT[,.N, V1] #for every group in V1, it counts how many rows there are. "by" can be omitted

# with .() you can add new columns, 
#(DT <- DT[...]) is redundant. The only way not to change the DT is by making a copy, with copy

DT[, V1 := round(exp(V1),2)] # only coloumn V1 is ubdated by what is after := NB, DT has been rewritten
DT
DT[, c("V1", "V2") := list( round(exp(V1),2), LETTERS[4:6])] #letters da 4 a 6 sono "d e f"
DT 

DT[, ':=' (V1 = round(exp(V1),2),
           V2 = LETTERS[4:6])][] #same as above, [] at the end prints result to screen

DT[, c("V1", "V2") := .( round(exp(V1),2), LETTERS[4:6])] #here different compared to previous (although list should be the same as .())
DT


DT[, V1 := NULL] #here the [] doesn't work
DT

DT[, c("V1", "V2") := NULL] #removing two col

DT[, ":=" (V1 = NULL, V2 = NULL)] #as above
DT

Cols.chosen = c("V1","V2")
DT[, Cols.chosen := NULL] #adds new cols.chosen then deletes them
DT

Cols.chosen = c("V1","V2")
DT[, (Cols.chosen) := NULL] # deletes the col specified in cols.chosen
DT

DT[.N-1] # in i, returns last but one row of DT
DT
DT[,.N] # in j, returns the number of rows


DT[,.(V1, V2)] # V1 and V2 returned as data table
DT[, mean(V3), by = .(V1, V2)]
DT[, print(.SD), by=V2] #.SD is a data.table and holds all the values of all columns, except the one specified in by.
DT # it groups the DT by V2 (first all As then all Bs, then all Cs)

DT[c(1, .N-1), .SD, by =V2] # selected first and last row, by V2. but here i told explicitly to take only first and last but one row.
# .SD holds all value of data table
DT[c(1, .N), .SD, by =V2] #first and last
DT[c(1:.N), .SD, by =V2] #all rows, sorted by V2, IT ALSO KEEPS V2
DT[, .SD[c(1,.N)], by =V2] #select first and last col grouping by V2

DT[, lapply(.SD, sum), by =V2]
DT[,V1, V3] #sorting by V3
DT[, .(V1,V3)] #chosing more columns at the same time. 

#to apply a fun to a subset of col of the DT, use .SD and .SDcols

DT[, lapply(.SD, sum), by= V2, .SDcols = c("V3", "V4")]
DT

DT[, lapply(.SD,sum), by=V2,
   .SDcols = c("V3","V4")]

DT[, lapply(.SD,sum), by=V2, 
   .SDcols = paste0("V",3:4)] 

#CHAINING
#first with no chaining

DT <- DT[, .(sum.V4 = sum(V4)), by= V1]
DT
DT[sum.V4 > 40] #any row of this table in which sum.V4 values is bigger than 40

# with chaining
DT <- DT[, .(sum.V4 = sum(V4)), by= V1][sum.V4 > 40] #in the same line doesn't work

DT <- DT[, .(sum.V4 = sum(V4)), 
         by= V1][sum.V4 > 40] #nota that if you run the previous line, now the DT is messed up, so you have to rerun the original table (content is overwritten)

DT[, .(sum.V4 = sum(V4)), 
         by= V1][order(V1)]

DT[, .(sum.V4 = sum(V4)), 
         by= V1][order(-V1)]
#--------------------------------------------


DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3], 
                 V3=round(rnorm(4),4), 
                 V4=1:12)

#here both are the same (before it was a mistake)

DT[, lapply(.SD, sum), by= V2, .SDcols = c("V3", "V4")]

DT[, lapply(.SD,sum), by=V2,
   .SDcols = c("V3","V4")]

#difference among the two is that .() is valid only in j in data table, while list, is a function in R
DT[, c("V1", "V2") := .( round(exp(V1),2), LETTERS[4:6])] 
DT[, c("V1", "V2") := list( round(exp(V1),2), LETTERS[4:6])]
DT
