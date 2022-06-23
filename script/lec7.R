#install.packages("mclust")
#install.packages("pheatmap")
library(data.table)
library(pheatmap)
library(mclust)
library(ggplot2)
library(mclust)

iris_dt <- as.data.table(iris)
#data MUST BE normalized before running the PCA. done through the center function
#proportion of variance means how much is explained by that component (Which percentage of variance is explained by each of the principle components)
#W = projection matrix, calculate it manually
#S = X^T*X

iris_dt[, "Species" := NULL] #or data.table(iris)[, -"Species"] or iris[, !"Species", with = FALSE]
pca.data <- prcomp(iris_dt, center = TRUE, scale. = TRUE)
summary(pca.data)
mpca <- as.matrix(pca.data)
t(mpca)

#manually computing the PCA on the non scaled data
X <- scale(iris_dt)
S <- t(X) %*% X
# not the same as doing s <-  x %*% t(x)

#compare results of the manually vs automatically computed PCA
W <- eigen(S)$vectors
summary(pca)

manual_proj <- scale(iris_dt)%*% W
semi_man_proj <- scale(iris_dt) %*% pca.data$rotation
predict_proj <- predict(pca.data)

table(abs(manual_proj - predict_proj) < 1e-7)
table(abs(semi_man_proj - predict_proj) < 1e-7)
ggplot(data.table(predict_proj, Species=iris$Species), aes(PC1, PC2, col = Species)) + geom_point()

#remove all covariation with the PCA
sigma <- cov(predict_proj)
max(abs(sigma[upper.tri(sigma)]))

#hierachical clustering
#euclidian or manhatan
#to calculate distance between there are several ways, es CENTROID, calc the distance between the two using their center
#average, first does the average then calc distance
#COMPLETE LINKAGE, uses the furtherst data as a distance (most far way)


#distance usually is used the euclidian
#how to cluster?
#heatmap is doing the same as the hclust but in the back

d <- dist( mtcars[, c('mpg','cyl')] )
distance 
hc <- hclust(dist(iris_dt), method = "complete")
pheatmap(iris_dt, clustering_meth = "complete", show_rownames = FALSE) #row data as is. The data don't cluster well. We will compare the overall lenght compared to the differnce among the lenghts

#doesn't work with data table cuz dt does not have row names
anno_row <- data.frame(Species = iris$Species)
rownames(anno_row) <- seq_len(nrow(anno_row))
iris_frames <- data.frame(scale(iris_dt))
rownames(iris_frame) <- rownames(anno_row)

pheatmap(iris_frame, clustering_meth = "complete",
         annotation_row = anno_row,
         show_rownames = FALSE, main = "IRIS complete linkage")



#see if clustering worked if the species cluster together
plot.data <- iris[,-(5:6)] #removes the name of the table

#after the clustering the row names are gona be changed.
#rename the data rows
rownames(plot.data) <- paste("iris", 1:nrow(plot.data), sep=".")  #adds to each row a increasing value plus "iris.32
row.ann <- data.frame(species=iris[,5], kmeans=iris[,6]) 

rownames(row.ann) <- rownames(plot.data)
pheatmap(plot.data, annotation_row=row.ann, show_rownames=F)

#complete or average in the iris gives the same clust results


h1 <- pheatmap(data, annotation_row = row_anno, clustering_method="complete") 
complete <- cutree(h1$tree_row, k=3)
complete
cutree(hclust(dist(data), method = "complete"), k=3)

row_anno[['complete']] <- factor(complete)
pheatmap(data, annotation_row = row_anno, clustering_method = "average")
average <- cutree(hclust(dist(data), method="average"), k=3)
table(complete, average) #compare two partitions, cluster 2 is big

# main difference between k means and clustring, clust starts from the bottom and looks up, kmenas starts randomly then goes top down (refining as it goes)
#retreave the clusters and add to the heatmap
km <- kmeans(iris_frame,3)
anno_row[['kmeans']] <- factor(km$cluster)
pheatmap(iris_frame, annotation_row = anno_row, clustering_method = "average")


km <- kmeans(iris_frame,3)
row_ann[['kmeans']] <- factor(km$cluster) #row_ann is a list of lists which has names, by [['kmeans]], you access that list, if that list doesn't exist you add it
pheatmap(iris_frame, annotation_row = row_ann, clustering_method = "average")

fit <- Mclust(iris_frame)
summary(fit)

fit3 <- Mclust(iris_frame, G = 3)
summary(fit3)


mcl <- fit3$classification
anno_row[['Mclus']] <- factor(mcl)
pheatmap(iris_frame, annotation_row = anno_row, clustering_method = "average")

?image() #can give x and y and obtain and specify colors and 
data <- iris[, c("Petal.Length", "Petal.Width")]
range.x <- seq(min(data[, "Petal.Length"]), max(data[, "Petal.Width"]), by=0.01)
range.y <- seq(min(data[, "Petal.Width"]), max(data[, "Petal.Width"]), by=0.01)
grid.data <- data.frame(Petal.Length=rep(range.x, length(range.y)), Petal.Width=rep(range.y, each=length(range.x)))
fit <- Mclust(data, G=3)
z <- predict(fit, grid.data, newdata=grid.data)$z


#have the data points of the data set, colors are the density

#break points for the colors

breaks <- pretty ( seq(0, 1, len = 12))
pal1 <- hsv(1,1,1, breaks)
pal2 <- hsv(0.5,1,1, breaks)
pal3 <- hsv(1,0.2,1, breaks)

image(range.x, matrix(z[, 1], nrow = length(range.x)))
image(range.x, matrix(z[, 1], nrow = length(range.x)))
points(data, col = factor(mcl))





