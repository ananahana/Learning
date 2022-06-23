which_all_min <- function(x, na.rm =FALSE){
  which(x == min(x, na.rm = na.rm)) #which gives indexes of x
}

colMax <- function(DT, colNames = NULL){
  if (is.null(colNames)) colNames = names(DT)
  DT[,sapply(.SD,max), .SDcols = colNames]
}

which_all_max <- function(x, na.rm =FALSE){
  which(x == max(x, na.rm = na.rm)) 
}