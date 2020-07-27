mydata<-read.csv(file.choose()) ## use read.csv for wine data
View(mydata)

View(mydata[-1]) # removing first column
# mydata[-1] -> Considering only numerical values for applying PCA
data <- mydata[,-1]
attach(data)
cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
# pcaObj$loadings  (alternate method to check loading)

sum(pcaObj$loadings[,1]**2) # check sum of loading square is equal to 1.
plot(pcaObj$scores[,1],pcaObj$scores[,3]) # check any relation between two PC.
cor(pcaObj$scores[,1],pcaObj$scores[,3]) # check corelation objective method
cor(pcaObj$scores)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores as per assignment

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,15:17]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="single") # method here is single linkage
fit2<-hclust(dist1,method="complete") # method here is complete linkage
fit3<-hclust(dist1,method="average") # method here is average linkage
fit4<-hclust(dist1,method="centroid") # method here is centroid linkage

plot(fit1 , hang = -1) # Displaying Dendrogram
plot(fit2 , hang = -1)
plot(fit3 , hang = -1)
plot(fit4 , hang = -1)

rect.hclust(fit2,k=5,border = "red") 

groups<-cutree(fit2,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) 
wine<- aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)
wine

write.csv(final1,file="wine_clustered.csv",row.names = F,col.names = F)
getwd()

# selecting K for kmeans clustering using K selection
install.packages("kselection")
library(kselection)
install.packages("doParallel")
library(doParallel)
registerDoParallel(cores = 4) # Take all the 4 cores to do this process to save time
k<-kselection(norm_clus,parallel = TRUE,k_threshold = 0.95,max_centers = 15)
k
# K finds 2 clusters

# Elbow curve & k to decide the k value
twss<-NULL
for(i in 1:14)
  twss[i]=sum(kmeans(clus_data,centers = i)$tot.withinss)

window()
plot(1:14,twss,type="b",xlab="Number of clusters",ylab="within groups sum of squares")
title(sub="K-Means Clustering Scree-Plot")
