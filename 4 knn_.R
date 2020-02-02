
knn_ <- function(test, traing, trainingY, k){
  knn_individual<- function(testLine, traing, trainingY, k){
    dist <- as.matrix( dist(rbind(testLine, traing), method = "euclidean"), labels=TRUE)
    dist <- dist[-1,1]
    
    tmp <- c()
    tmp <- cbind(traing, dist ,trainingY)
    tmp <- tmp[order(tmp$dist, na.last=TRUE, decreasing=T)[1:k],]
    
    result <- 0
    result <- ifelse( sum(tmp$trainingY==0)  > sum(tmp$trainingY==1) , 0, 1)
    return ( result )
  }
  
  result <- apply(test, 1, function(x) { knn_individual( t(x), traing, trainingY, k )} )
  return ( result )
}
