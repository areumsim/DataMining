Find_criteria <- function( numberOfY, df, entropyThreshold ) {
  
  Cal_e <- function( numberOfY, df ) {
    n <- nrow(df)
    y <-  unique(df[, numberOfY])
    
    entropy <- lapply( y, function(x) {
      nrow(df[df[, numberOfY]==x, ])/n * log2(nrow(df[df[, numberOfY]==x, ])/n)
    } )
    return ( -sum(unlist(entropy), na.rm=T) )
  }
  
  originEntropy <- Cal_e(numberOfY, df)
  
  if( originEntropy == 0 ){
    return ( list(colNumber=-1) )
  }
  
  criteria <- list( criteria=c(), entropy=c() )
  
  for( i in 1:ncol(df[, -numberOfY]) ) { # i : column
    # sort df according to ind column
    tmpDf <- df[order(df[, i]), ]  
    n <- nrow(tmpDf)
    
    tmpEntropy<- c()
    for( j in 1:(n-1) ){ # j : row
      tmpDf1 <- tmpDf[1:j, ]
      tmpDf2 <- tmpDf[(j+1):n, ]
      
      tmpEntropy[j] <- (nrow(tmpDf1)/n)*Cal_e(numberOfY, tmpDf1) +
        (nrow(tmpDf2)/n)*Cal_e(numberOfY, tmpDf2)
    }
    
    criteria$criteria[i] <- tmpDf[which.min(tmpEntropy), i]
    criteria$entropy[i] <- tmpEntropy[which.min(tmpEntropy)]
  }
  
  if( originEntropy < min(criteria$entropy) ||
      min(criteria$entropy) == 0 ||
      min(criteria$entropy) < entropyThreshold ) {
    return ( list( colNumber=-1) )
  }
  
  return( list( colNumber=which.min(criteria$entropy),
                value=criteria$criteria[which.min(criteria$entropy)],
                entropy=min(criteria$entropy) ) )
}


rpart_ <- function( numberOfY, df, originEntropy=-99, depth=0, side=0, entropyThreshold=0.2 ) {
  Get_mode <- function(df) {
    uniqv <- unique(df)
    return( as.character(uniqv[which.max(tabulate(match(df, uniqv)))] ) )
  }
  
  Print_tab <- function(depth, side){
    cat(depth,":")
    while( depth > 0 ){
      cat("\t")
      depth <- depth -1
    }
  }
  
  criteria <- Find_criteria(numberOfY, df, entropyThreshold)
  newEntropy <- criteria$entropy 
  if( originEntropy == -99 ){ # root node일 경우, 초기화
    originEntropy<- newEntropy
  }
  
  #TODO depth가 입력되면 depth이하의 tree 생성
  
  if( criteria$colNumber == -1  || originEntropy < newEntropy ){
    # Leaf node 
    Print_tab(depth-1, side)
    if( side == -1 ) {
      cat( "L- <", Get_mode(df[,numberOfY]) , ">\n" )
    } else if ( side == 1) {
      cat( "R- <", Get_mode(df[,numberOfY]) , ">\n" )   
    }
  } else {
    # Split tree
    originEntropy <- newEntropy
    colNumber <- criteria$colNumber
    value <- criteria$value
    
    library(stringr)
    SplitCriteria <- str_c( colnames(df)[[colNumber]] , " <= " , value )
    Print_tab(depth, side)
    cat(SplitCriteria , "\n" )
    
    leftDf <- df[df[,colNumber]<=value, ]
    rpart_( numberOfY, leftDf , originEntropy, depth+1 , -1, entropyThreshold)
    
    
    rightDf <- df[df[,colNumber]>value, ]
    rpart_(numberOfY, rightDf, originEntropy, depth+1 , 1 , entropyThreshold)
  }
}


### ### Test 1 ### ###
df <- iris
rpart_(5, df, entropyThreshold=0.2)

### Refer. using R code ###
library(rpart)
model_ref <- rpart(Species ~., data=iris)
plot(model_ref, compress=TRUE, margin=.2)
text(model_ref, cex=1.5)


### ### Test 2 ### ###
df <- USArrests
df$UrbanPop1 <- ifelse(df$UrbanPop> median(USArrests$UrbanPop), "Large", "Small")
df <- df[, names(df)!="UrbanPop"] 
rpart_(4, df,  entropyThreshold=0.5)

### Refer. using R code ###
model_ref <- rpart(UrbanPop1 ~ ., data = df, method = "class")
plot(model_ref, compress=TRUE, margin=.2)
text(model_ref, cex=1.5)
### ### ### ### ### ### ###
