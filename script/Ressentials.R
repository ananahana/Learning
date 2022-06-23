x <- c("a", "b", "c", "d")
y <- sample(x) #random order
y
order(y) # index of which position the elements of x are after scrambling them
first <- order(y)[1]
y[3]
