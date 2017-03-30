my.kmeans <- function(X, k=3, eps=1E-3, init_mean=X[sample(1:nrow(X),k),]) {
  N <- nrow(X)
  P <- ncol(X)

  # Assert that the initialized mean has dimension == number of clusters (k)
  stopifnot(nrow(init_mean) == k)

  # Find Euclidean Distance b/w 2 Vectors
  d2 <- function(a,b) sqrt( sum((a-b)^2) )

  # Find closest means
  closest <- function(cluster_means) {
    out <- apply(cluster_means, 1, function(c_mean) {
      apply(X,1,function(x) d2(x,c_mean))
    })
    apply(out,1,which.min)
  }

  # Recompute Means
  recompute_means <- function(closest_means) {
    t(sapply(1:k, function(j) {
      x_idx_nearest_mean_j <- which(closest_means == j)
      apply(X[x_idx_nearest_mean_j,],2,mean)
    }))
  }

  # Initialize
  prev_mean <- init_mean
  its <- 0

  # Do until convergence
  while( d2(curr_mean <- recompute_means(closest(prev_mean)),prev_mean)>eps ) {
    prev_mean <- curr_mean
    its <- its+1
  }

  #recompute_means(X,closest(X,init_mean))

  list(clusters=closest(curr_mean), means=curr_mean, its=its, init_mean=init_mean)
}

#out <- my.kmeans(iris[,-5], k=3,eps=1E-5)


