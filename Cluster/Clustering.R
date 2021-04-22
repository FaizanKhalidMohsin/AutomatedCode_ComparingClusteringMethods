# [Amgd, Mohamed]
# [20153726]
# [MMA]
# [Section 1]
# [MMA 869]
# [August 16,2020]


# Submission to Question [1], Part [1]



# TODO: import other libraries as necessary
library(statar)
library(scales)
library(ggplot2)
library(tidyverse)
library(factoextra)


# TODO: insert code here to perform the given task. 
# Don't forget to document your code!

#reading data
Jewl_csr <- read_csv("jewelry_customers.csv")
#exploring head                     
head(Jewl_csr)


#exploring the data and the features prior clustering

any(is.na(Jewl_csr)) # = FALSE meaning no missing data

# scatter plot visulization based on Age and income with color identifying the saving, very clear clusters 
Pl1 <- ggplot(Jewl_csr, aes(Age, Income)) +
  geom_point(aes(color = Savings), alpha= 0.5, size = 3) + 
  scale_color_gradient(low= "red", high = "green")   
Pl1

#variation between features
var(Jewl_csr$Age)
var(Jewl_csr$Income)
var(Jewl_csr$SpendingScore)  # there is a need for scaling

#the function appear to scale & unscale the data properly to original state
scaled <- scale(Jewl_csr)
head(scaled, 10)
Unscaled <- unscale(Jewl_csr)
head(Unscaled, 10)   # correct data unscaled

#clusters..............

#1. K-means

# determine the number of clusteres by Elbow plot method
#creating function for wss plot

set.seed(1234)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")  # number 5 appears to be optimal also consistent with Pl1 plot on original data

km <- kmeans(scaled, 5)
autoplot (km,scaled,frame = TRUE)
km  #examine centers and features of the clusters, centers appears to be diffrent



                     
