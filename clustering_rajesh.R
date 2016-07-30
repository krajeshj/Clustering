# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

#install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df<-wine
df["Type"] <-NULL
str(df)
df <-scale(df)
View(df)
df<-as.data.frame(df)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

print(" It looks like 3->4 seems to be where the elbow is") 
print(" The drop in difference  is the most between 1 and 3 clusters")

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

print( "The NbClust method also came up with number of clusters to be 3")

# Exercise 3: How many clusters does this method suggest?
print(" Accordingto NbClust method it was 3")

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
fit.km <- kmeans(df, 3, nstart=20)
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, as.factor(wine$Type))


# Wine types 1 and 3 are well classified

print("#    1  2  3")
print("# 1  0  3 48")
print("# 2  0 65  0")
print("# 3 59  3  0")

print("Wine Types 1 and 3 are in perfect agreement")
print("Wine Type 2 is mostly  well clustered")

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

#clusplot( ... )
clusplot (df,fit.km$cluster , diss = F, cor = T, stand = F, lines = 2, shade = F, color = F, labels = 0, plotchar = T, span = T)

