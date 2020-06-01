##### DANA 4840 Project
#### Partitioning-K-Medoids
###Last update: AA, 29 Mar

#apply K-medoids - PAM to carpet age dataset
library(factoextra)
library(clustertend)
library(NbClust)
library(fpc)
library(dplyr)
library(cluster)
library(clValid)


set.seed(1998) #for reproducibility

carpet_df <- read.csv(file.choose())
str(carpet_df)
head(carpet_df)

#####PRE-WORK | DATA CLEANSING, SCALE, ETC

#check for missing data
apply(is.na(carpet_df), 2, sum) 

#2 observations with missing data for age, this is less than 10% of our data so we will remove the records and proceed to scale
carpet_df <- carpet_df[!(is.na(carpet_df$age)),] #delete rows where age = NA
apply(is.na(carpet_df),2,sum) #confirm that there are no missing data left
carpet.scaled <- scale(carpet_df[,-1])
head(carpet.scaled)


####### Step 1: assess cluster tendency

par(mfrow = c(2,2))
# #visualize data to assess for cluster tendency
# # Plot data set, we need to do pca first because we have >2 vars
fviz_pca_ind(prcomp(carpet.scaled), title = "PCA - Carpet Age Data",
             habillage = carpet_df$sample_id,palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# #statistical method of assessing clustering tendency
# #We can conduct the Hopkins Statistic test iteratively, using 0.5 as the threshold to
# # reject the alternative hypothesis. That is, if H < 0.5, then it is unlikely that D has
# # statistically significant clusters.
# # Put in other words, If the value of Hopkins statistic is close to 1, then we can reject
# # the null hypothesis and conclude that the dataset D is significantly a clusterable
# # data.
#
# #using get_clust_tendency, data is highly clusterable if near to 1
res <- get_clust_tendency(carpet.scaled, n = nrow(carpet.scaled)-1, graph = FALSE)
res$hopkins_stat
#Hopkins stat using get_clust_tendency is 0.8489 so it is highly clusterable
#
# #using hopkins() function [in clustertend package]. It implements 1- the definition of H



# #visual method of assessing clustering tendency
fviz_dist(dist(carpet.scaled), show_labels = FALSE)+
  labs(title = "Carpet Age Data")
# #the dissimilarity viz supports the hopkins finding that the seed data contains cluster structures


############STEP 2: DETERMINE OPTIMAL No. OF CLUSTERS
#Direct Methods: Elbow and silhouette methods
#Elbow method | The location of a bend (knee) in the plot is generally considered as an indicator
#of the appropriate number of clusters; sometimes ambiguous
# The Elbow method looks at the total WSS as a function of the number of clusters: One
# should choose a number of clusters so that adding another cluster doesn’t improve much
# better the total WSS.

# Silhouette method
fviz_nbclust(carpet.scaled, pam, method = "silhouette") +
 labs(subtitle = "Average silhouette method | Carpet Age Data set")
#silhouette method finds k=2 as optimal

#Average Silhouette method | measures the quality of a clustering. That
# is, it determines how well each object lies within its cluster. A high average silhouette
# width indicates a good clustering.
# Average silhouette method computes the average silhouette of observations for different
# values of k. The optimal number of clusters k is the one that maximize the average
# silhouette over a range of possible values for k (Kaufman and Rousseeuw, 1990).
#The location of the maximum is considered as the appropriate number of clusters.

# Elbow method
fviz_nbclust(carpet.scaled, cluster::pam, method = "wss")+
   labs(subtitle = "Elbow method | Carpet Age Data set") +
  theme_classic()+
  geom_vline(xintercept = 2, linetype = 2)
#elbow method finds k=2 as optimal



#Statistical Method: Gap stat
# The gap statistic compares the total within intra-cluster variation for different values of
# k with their expected values under null reference distribution of the data. The estimate
# of the optimal clusters will be value that maximize the gap statistic (i.e, that yields
# the largest gap statistic). This means that the clustering structure is far away from the
# random uniform distribution of points.
#Note that, using B = 500 gives quite precise results so that the gap plot is basically
# unchanged after an another run.

fviz_nbclust(carpet.scaled, cluster::pam, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method | Carpet Age Data set")
#gap stat finds k=2 as optimal

#Our checks indicate that K=2 is the optimal number of clusters for K-Medoids

###### STEP 3: PARTITIONING CLUSTERING
#skip this as this is covered by Harpreet's script
# #K-means
# km.res <- kmeans(seed.scaled, 3, nstart = 25)
# print(km.res)
# 
# #add clustering result:
# dd <- cbind(seed.scaled, cluster = km.res$cluster)
# head(dd)
# 
# #Cluster number for each of the observations
# km.res$cluster
# head(km.res$cluster, 10)
# 
# # Cluster size
# km.res$size
# 
# # Cluster means
# km.res$centers
# 
# #Visualize the clusters:
# fviz_cluster(km.res, data = seed.scaled,
#              palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
#              ellipse.type = "euclid", # Concentration ellipse
#              star.plot = TRUE, # Add segments from centroids to items
#              repel = TRUE, # Avoid label overplotting (slow)
#              ggtheme = theme_minimal()
# )
#


#pam has 2 options for metric:manhattan and euclidean
#K-medoids: less prone to outliers
pam.res <- pam(carpet.scaled, 2, metric = "manhattan", stand = FALSE)

pam.res$clusinfo

#let's try using the euclidean distance:
pam.res2 <- pam(carpet.scaled, 2, metric = "euclidean", stand = FALSE)

pam.res2$clusinfo

#we can see that PAM using Euclidean or manhattan distance both resulted in clusters of size 14 and 19.
#However, PAM using manhattan resulted in clusters that are more separated

#add cluster information to original df:
carpet_cluster <- cbind(carpet_df, cluster = pam.res$cluster)
head(carpet_cluster)

carpet_cluster2 <- cbind(carpet_df, cluster = pam.res2$cluster)
head(carpet_cluster2)

#we will proceed with pam using manhattan as the manhattan distance is less prone to outliers
#cluster medoids:
pam.res$medoids

#cluster numbers:
pam.res$clustering


#visualize pam:
fviz_cluster(pam.res,
             #palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_classic(),
             main = "Cluster Plot of Carpet Age using K-Medoids"
             
)


#pam.fit2 <- pam(gowerdist_ageq , diss = , k)
#summary(pam.fit2)
pam2.res <- carpet_df %>%
  mutate(cluster = pam.res$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam2.res$the_summary
#pam.fit2$medoids

###### STEP 4: CLUSTER VALIDATION
#Internal validation measures reflect often the compactness, the connectedness and
# the separation of the cluster partitions.

#Silhouette coefficient
# The silhouette analysis measures how well an observation is clustered and it estimates
# the average distance between clusters. The silhouette plot displays a measure of
# how close each point in one cluster is to points in the neighboring clusters.
# # • Observations with a large Si (almost 1) are very well clustered.
# • A small Si (around 0) means that the observation lies between two clusters.
# • Observations with a negative Si are probably placed in the wrong cluster.

#we will use eclust() to perform pam and then plug it into fvz_silhouette:
pamres.eclust <- eclust(carpet.scaled, "pam", k = 2, graph = FALSE)

# Visualize k-medoidsclusters
fviz_cluster(pamres.eclust, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


#generate silh plot
fviz_silhouette(pamres.eclust, palette = "jco",
                ggtheme = theme_classic())
#average silh width = 0.81

#we shld look for observations with -ve silhouette index as this would indicate that the observation is in the wrong cluster
# visually, there appears to be no observations with -ve silhouette coeff. Let's verify that.We can find the name of these samples
# and determine the clusters they are closer (neighbor cluster), as follow:
# Silhouette width of observation
sil <- pamres.eclust$silinfo$widths[, 1:3]
# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

#we can confirm that there are no observations with -ve silhouette coeff. Based on this, we can say that there are no observations
#assigned to the wrong cluster

#Dunn Index
# If the data set contains compact and well-separated clusters, the diameter of the
# clusters is expected to be small and the distance between the clusters is expected to
# be large. Thus, Dunn index should be maximized.
# The function cluster.stats() returns a list containing many components useful for analyzing
# the intrinsic characteristics of a clustering:
#   • cluster.number: number of clusters
# • cluster.size: vector containing the number of points in each cluster
# • average.distance, median.distance: vector containing the cluster-wise within
# average/median distances
# • average.between: average distance between clusters. We want it to be as large
# as possible
# • average.within: average distance within clusters. We want it to be as small as
# possible
# • clus.avg.silwidths: vector of cluster average silhouette widths. Recall that, the
# silhouette width is also an estimate of the average distance between clusters. Its
# value is comprised between 1 and -1 with a value of 1 indicating a very good cluster.
# • within.cluster.ss: a generalization of the within clusters sum of squares (k-means
# objective function), which is obtained if d is a Euclidean distance matrix.
# 
# • dunn, dunn2: Dunn index
# • corrected.rand, vi: Two indexes to assess the similarity of two clustering: the
# corrected Rand index and Meila’s VI

#to find dunn index:
pam_stats <- cluster.stats(dist(carpet.scaled), pamres.eclust$cluster)
# Dun index
pam_stats$dunn

#display all stats:
pam_stats




#################STEP 5: CHOOSING THE BEST CLUSTERING ALGO
# clValid (Brock et al., 2008), which can be
# used to compare simultaneously multiple clustering algorithms in a single function call
# for identifying the best clustering approach and the optimal number of clusters.
# The clValid package compares clustering algorithms using two cluster validation measures:
#   1. Internal measures, which uses intrinsic information in the data to assess the quality
# of the clustering. Internal measures include the connectivity, the silhouette coefficient
# and the Dunn index

# Compute clValid
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(carpet.scaled, nClust = 2:4,
                  clMethods = clmethods, validation = "internal")
# Summary
summary(intern)

#findings are the same as harpreet's
#Using internal measures, hierarchical, kmeans and pam have the same scores for internal measure.
#for all 3 algorithms, the k=2 is optimal


# 2. Stability measures, a special version of internal measures, which evaluates the consistency
# of a clustering result by comparing it with the clusters obtained after each
# column is removed, one at a time.
# Cluster stability measures include:
#   • The average proportion of non-overlap (APN)
# • The average distance (AD)
# • The average distance between means (ADM)
# • The figure of merit (FOM)
# The values of APN, ADM and FOM ranges from 0 to 1, with smaller value corresponding
# with highly consistent clustering results. AD has a value between 0 and
# infinity, and smaller values are also preferred.

# Stability measures
clmethods <- c("hierarchical","kmeans","pam")
stab <- clValid(carpet.scaled, nClust = 2:4, clMethods = clmethods,
                validation = "stability")
# Display only optimal Scores
optimalScores(stab)

# optimalScores(stab)
# Score       Method Clusters
# APN 0.0000000 hierarchical        2
# AD  0.4511642          pam        4
# ADM 0.0000000 hierarchical        2
# FOM 0.1955139       kmeans        4

# For the APN and ADM measures, hierarchical clustering with k=2
# gives the best score. For AD, PAM with 4 clusters has the best
# score. For FoM, kmeans with k=4 has the best score

