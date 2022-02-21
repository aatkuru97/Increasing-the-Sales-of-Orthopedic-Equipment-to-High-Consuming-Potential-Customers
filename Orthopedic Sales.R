#############R Code for Final Project ##########################################
#objective - find ways to increase sales of orthopedic products to all hospitals in US
#find those who have highest consumption of such equipment, but where our sales are = 0 
#Come up with a selected group where you think our efforts will be rewarded. (a few hospitals 5 or 10 or 15). 
#Estimate the potential or expected sales on those hospitals.
################################################################################

#Download necessary libraries 
library(caret)
library(MASS)
library(tidyverse)
library(tree)
library(nnet)
library(rpart)
library(naniar)
library(pysch)
library(clue)
library(cluster)
library(factoextra)
library(NbClust)
library(psych)

#Read in data + perform simple data analysis
hospitals1 <- read.csv(file='C:/Users/aatkuru/Desktop/hospitalUSA.csv')
hospitals1
summary(hospitals1)
nn = names(hospitals1)
print(nn)z
nrow(hospitals1) #4703 rows - need to cutdown between 3000-3500
#Sales Data
sales <- hospitals1$SALES[hospitals1$SALES>0] #sales greater than 0 
summary(sales)
#subset the data to get 3,292 observations (between 3000-3500)
set.seed(04248) #last 4 digits of RUID
n <- nrow(hospitals1)
n 
subset = sample(n,0.7*n,replace=FALSE)
hospitals = hospitals1[subset,]

#separate response variable sales - set all 0 sales to NA
hospitals$SALES[hospitals$Sales==0]= NA

#separate demographics variables - BEDS, RBEDS, OUTV, ADM, SIR, TH, TRAUMA, REHAB 
demographics <- hospitals[,c(5:9,13:15)]
summary(demographics)

#separate operation numbers - HIP, KNEE, HIP2, KNEE2, FEMUR2
operationnum <- hospitals[,c(11:12,16:18)]
summary(operationnum)

#analyze the variables to determine the required transformations 
pairs(hospitals[,-c(1:4,13:15)],pch=".",col=2)
summary(hospitals)
par(mfrow=c(3,4))
for(i in c(5:12,16:18))  hist(hospitals[,i],main=nn[i])

#It appears due to right-skewness all variables need to be transformed 
hosp= subset(hospitals)[,-(1:4)] #label dataset as such because we want to look at all sales
hosp
hosp$RBEDS = log(1+hosp$RBEDS)
hosp$SALES= log(1+hosp$SALES)
for(i in c(1,3:5,7:8)) hosp[,i]= log(1+0.01*hosp[,i])
for (i in c(7:8))hosp[,i]=sqrt(hosp[,i])
for (i in c(12:14))hosp[,i]=log(1+0.1*hosp[,i])
pairs(hosp[,-(9:11)],pch=".",col=3)
par(mfrow=c(3,4))
for(i in c(1:8,12:14))  hist(hosp[,i],main=nn[i+4])


##summarize demographic and operation variables through dimension reduction#######
##reduce the list of factor variables to 3-4 
##use 3 by convention
fit <- factanal(na.pass(hosp[,-6]),3,scores="regression",rotation="varimax",lower=0.1)
fit
load <- fit$loadings[,1:3]
plot(load,type="n")
text(load,labels=names(hosp[,-6]),cex=0.7,col="red")
apply(fit$loadings,1,function(x) which.max(abs(x)))-> fn
fn #analyze loadings
#analyze the scores
fit_scores <- data.frame(fit$scores)
fit_scores
plot(fit_scores,pch=16,col="purple",cex=0.5)

#Independent variables are used to divide list of hospitals into subsets
#we are summarizing variables with factors - combine the original dataset (hosp)
#with factors from factor analysis to be analyzed 


#separate the dataset into factored data
data_factor <- cbind(hosp,fit_scores)
head(data_factor) #preview combined set
scores <- data.frame(fit$scores)
##use an indicator to identify where the hospitals per cluster have sales or not
##refernced off r/bloggers.com
data_factor$SALES_IND[data_factor$SALES<0] <- 0 
data_factor$SALES_IND[data_factor$SALES>0] <- 1
new_data<-data_factor[,-c(1:5,7:14)]
new_data #understand which clusters have hospitals that either have sales or not
#use clust_sel to cluster on the factors 
x = as.matrix(new_data[,c(2:4)]) #cluster factor
x0=t(t(x))/apply(x,2,sd) 

#cluster the data
hclust1 <- function(x,k)
  list(cluster=cutree(hclust(dist(x),method="ward.D"),k))

clust_sel=function(x,fun=hclust1,jrange=1:25,dd=2,w=1) {
  ## x is an array,             ##  jrange n of clusters to be checked
  ## y is an hclust object      ##  dd number of differences
  wss4 = function(x,y,w = rep(1, length(y))) sum(lm(x~-1+factor(y),weights = w)$resid^2*w)
  ### wss4 calculates within cluster sum of squares
  sm1 = NULL
  for(i in jrange) sm1[i] = if(i==1) sum(lm(x~1)$resid^2) else wss4(x,fun(x,i)$cluster)
  sm1=sm1[jrange]
  k = if(dd==1) sm1[-1] else -diff(sm1) 
  plot(jrange[c(-1,-length(k))], -diff(k)/k[-length(k)]*100)
  jrange[c(-1,-length(k))] [sort.list(diff(k)/k[-length(k)]*100)[1:4]]
}#use the second derivative method 
second_der <- clust_sel(x0,hclust1,jrange=1:35)
second_der

#can verify 5 clusters with another method 
fviz_nbclust(new_data[,c(2:4)],kmeans,method="silhouette")
#also got k=5

#once the clusters are chosen, we must specify summary statistics for e/a cluster
#you can use a boxplot of sales or transformed sales vs. your cluster number
#choose number of clusters with highest sales
reclust<- cutree(hclust(dist(x),method="ward.D"),5)
reclust #cluster now with specified optimal number of clusters
new_data2 <- cbind(new_data, cluster = reclust)
head(new_data2)
#use a boxplot to find highest sales
boxplot(new_data2$SALES~reclust, main = "Boxplot of Highest Sales",xlab='cluster no', ylab='Sales ($)')
abline(h=mean(x),col="red") #used to get an understanding of avg sales
z = mean(x)
z #sales for the highest cluster (per 1000's)


#cluster with highest sales is = 3
#still it is good to do further analysis to see which cluster agrees with our objectives
#but where there are hospitals were the company's sales is NA so they are not yet our customers
describeBy(new_data2,reclust)
#group 1 has a smaller variance/sd between the different features, so therefore it
#overall this cluster meets our objectives


#estimate potential gain in sales
#average sale to similar hospital - average sale to that cluster
#interested in hospitals where current sales = NA
#Find optimal number of clusters - redo clustering process in case N>100 
#can use the NbClust function with kmeans to find optimal number of clusters
n=nrow(hospitals)
ni=sample(n,3292,rep=F)
hospital_train <- hospitals[ni,]
hospital_test <- hospitals[-ni,]
test_set <- hospital_test[,-6] #remove indep variable


#NbClust(data=scores,diss=NULL,distance='euclidean', min.nc=15,max.nc=30,method='kmeans')
NbClust(data=new_data[,c(2:4)],diss=NULL,distance='euclidean',min.nc=15,max.nc=30,method='kmeans')
hosp_clust = kmeans(hosp,16)
boxplot(hosp$SALES~hosp_clust$cluster,main="Boxplot of Hospital Sales by Cluster",col="blue")

#optimal number of clusters specified by kmeans = 16
#Use to results of this clustering to see how much sales is generated per cluster

#define the testing set
hospital_test <- hospitals[hospitals$SALES==0,-(1:4)]
for(i in c(1:5,7:8,12:14)) hospital_test[,i]= log(1+0.01*hospital_test[,i])

#Need to find the highest potential sales gain 
#K-Mediods is used to calculate the center of the clusterin PAM, and can be used to calculate the average sales per cluster
kmeans_train <- cl_predict(kmeans(hosp,16),newdata=test_set) #train model to testset
summary(kmeans_train)
table(kmeans_train)

#PAM is a robust version of k-means clustering - can be used to estimate/predict cluster with highest avg sales
clust_train <- pam(hosp,16)
clust_train$medoids
summary(clust_train)
table(clust_train$medoids)



















