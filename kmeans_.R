### refer using R package
df <- iris #ppMeasures
plot(iris$Sepal.Length, iris$Sepal.Width, type="p", main="origin data", xlab="Sepal.Length", ylab="Sepal.Width", col=iris$Species)

result_ref <- kmeans(df[,-5], 3)
y <- as.factor(result_ref$cluster)
df_ref <- cbind(df, y)  
plot(df_ref$Sepal.Length, iris$Sepal.Width, type="p", main="ref. using r Method", xlab="Sepal.Length", ylab="Sepal.Width", col=df_ref$y)

result <- kmeans_(df[, -5] ,3)
y <- as.factor(result$cluster)
df_ <- cbind(df, y) 
plot(df_$Sepal.Length, df_$Sepal.Width, type="p", main="implementation kmeans", xlab="Sepal.Length", ylab="Sepal.Width", col=df_$y)


kmeans_ <- function(df, k){
  set.seed(2016)
  center <- df[sample(nrow(df), k), ]# initial center
  row.names(center)<-1:nrow(center)
  tmpCenter <- center 
  
  repeat {
    tmp <- rbind(center[ ,], df)
    dist <- as.matrix( dist(tmp, method = "euclidean"), labels=TRUE)
    dist <- dist[1:k, (k+1):ncol(dist)]
    
    cluster <- rep(0,nrow(df))
    for ( i in 1:nrow(df) ){
      cluster[i] <- which.min(dist[,i]) 
    }
    
    for( i in 1:k ){
      tmpCenter[i,] <- colMeans(df[cluster==i, ])
    }
    
    if( all(center == tmpCenter) ){
      break
    } 
    center <- tmpCenter
  }
  
  return ( list( center = center, cluster = cluster ) )
}


