source("kmeans.R")

P <- 2
N <- 300
K <- 3
err <- .7 

M <- matrix(c(3,2,
              1,4,
              0,1),nrow=K,ncol=P,byrow=TRUE)

X <- do.call(rbind, lapply(1:K, function(i) 
  matrix(rep(M[i,],each=N),ncol=P)
))
lab <- rep(1:K,each=N)

Y <- X + rnorm(N*P*K,0,err)


out <- my.kmeans(Y,k=K,eps=1E-3)

# PLOT
plot(Y[,1],Y[,2],col=out$clusters+1,pch=20)
points(M[,1],M[,2],cex=5,pch=4,col='grey30',lwd=5)
points(out$means[,1],out$means[,2],cex=5,col=2:4,pch=20)

