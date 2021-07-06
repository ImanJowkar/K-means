# Define DataSet----------------------------------------

# Required Libraries------------------

library(factoextra)
library(ggplot2)

# Load Data-----------------------

data = read.csv('K:/data/DataSience/data_sicence course/ML/farzad/18/iman/1- kmeans/kmeans_web/Clustering.csv', header = T)
plot(data$x, data$y)
head(data)

data$label = ifelse(data$x < 5, 0, 1)
head(data)


# shuffle

rndom = sample(1:190, 190, replace = F)
data = data[rndom, ]


# visulize
library(ggplot2)

ggplot(data, aes(x = x, y = y, col = factor(label))) + 
  geom_point()



# Create a subset of data without label column

data_km = data[, c('x', 'y')]
data_km

# Data Preprocessing for applied Clustering Methods
# in this Section we want to apply Kmeans algorithm to our data set
# Befor we apply k-means algorithm to our dataset, we have to normalize it.
data_km = as.data.frame(scale(data_km))
head(data_km)

# In Kmeans Algorithm we have to define the number of Cluster which represent with k
# fortunatly in R, We have some intersting functions for selecting k.

fviz_nbclust(data_km, kmeans, method = "wss")
  


fviz_nbclust(data_km, kmeans, method = "silhouette") + # method = 'gap_stat', 'wss', 'silhouette'
  geom_vline(xintercept = 4, linetype = 2)  



set.seed(123) # set seed for all of us get the same result
km.res <- kmeans(data_km, 2, nstart = 25)
km.res


dd <- cbind(data, cluster = km.res$cluster)
head(dd)

table(dd$label)
table(dd$cluster)



fviz_cluster(km.res, data = data,
  palette = c("#2E9FDF", "#00AFBB"),
  ellipse.type = "euclid", # Concentration ellipse
  star.plot = TRUE, # Add segments from centroids to items
  repel = TRUE, # Avoid label overplotting (slow)
  ggtheme = theme_minimal())

