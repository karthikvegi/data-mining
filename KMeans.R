#--------------------------------------------
# Implement K-Means Algorithm
# Author: Karthik Vegi
#--------------------------------------------

# Creating four 2-d clusters with 50 points each
k <- 4
n <- 50
color <- c("red","skyblue","green","darkgoldenrod1")

generatePoints <- function() {
  x<-y<-cluster<-c()
  # Generate cluster from Tk, Zi and Bk
  for(i in 1:k) {
    T <- matrix(rnorm(4, mean=0, sd=1), ncol=2) # Tk ~ N(0,1)
    b <- matrix(rnorm(2, mean=0, sd=10), ncol=1) # Bk ~ N(0,10)
    for(j in 1:n) {
      z <- matrix(rnorm(2, mean=0, sd=1), ncol=1) # Zi ~ N(0,1)
      point<-(T%*%z)+b
      x<-c(x, point[1,1])
      y<-c(y, point[2,1])
      cluster<-c(cluster, i)
    }
  } # end for
  points <-data.frame(x,y,cluster)
  return(points)
} # end function

# Call generatePoints() function
points <- generatePoints()

# (a): Generate your points and plot them with different colors for each cluster
plot(points$x, points$y, pch=16, col=color[points$cluster], main="Generated Points")
readline("Inspect the points and press any key to continue..")

# (b): Implement K-Means algorithm

# Function k_means starts ---->
k_means <- function(dat) {
  # Randomly pick k centroids
  centroid <- dat[sample(1:nrow(dat), k), -3]
  repeat {
    # Assign points to the closest centroid
    for(i in 1:nrow(dat)) {
      min.dist <- 999
      for(j in 1:k) {
        x <- dat[i,-3]
        y <- centroid[j, ]
        dist <- dist(rbind(x, y), method = "euclidean")
        if(dist < min.dist) {
          min.dist <- dist # update the min distance
          dat[i, 3] <- j # update the centroid
        }
      }
    }
    # Plot the clusters and prototypes
    plot(dat[, -3], main = "K-Means Clustering")
    for(i in 1:k) {
      points(subset(dat, cluster==i), col=color[i])
    }
    points(centroid, pch="+", cex=3) # plot prototype
    # Allow user to inspect the current clusters and prototypes
    a<-readline("Inspect and press any key to continue..")
    # Update the centroids
    dat <- dat[order(dat$cluster), ]
    new.centroid <<- t(as.data.frame(lapply(split(dat[, -3], dat$cluster), colMeans)))
    if(all(centroid == new.centroid)) {
      if(any(unlist(lapply(split(dat[, -3], dat$cluster), nrow)) == 1)) {
        centroid <- dat[sample(1:nrow(dat), k), -3]
        next
      }
      break
    }
    else {
      centroid <- new.centroid
    }
  }
  return(dat)
}
# Function k_means ends ----->

cluster <- matrix(1, nrow(points), 1)
dat <- data.frame(points[,-3], cluster)
# call k-means clustering
k_means(dat)

# Plot the number of steps to convergence using 100 random data sets..
for(i in 1:3) {
  generatePoints()
  cluster <- matrix(1, nrow(points), 1)
  dat <- data.frame(points[,-3], cluster)
  k_means(dat)
}
