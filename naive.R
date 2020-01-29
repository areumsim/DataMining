# library(e1071)

library("mlbench")
data(PimaIndiansDiabetes)
df <- PimaIndiansDiabetes
#  pos (268) , neg (500)

library(caret)
set.seed(1) 
idx<-createDataPartition(y=df$diabetes, p=0.7, list=FALSE) 
train<-df[idx, ]
test<-df[-idx, ]

### Run Refer NaiveBayes Method
fit_ref <- naiveBayes(diabetes~., data = train)
pred_ref <- predict(fit_ref, test, type='class')
confusionMatrix(pred_ref, test$diabetes)$table

### Run Implement Method
model <- naiveBayes_(train[,9], train[,-9])
pred <- predict_(model, test[,-9])
confusionMatrix(pred, test$diabetes)$table


naiveBayes_ <- function( y, trainDf ){
  negPriorProb <- sum(y=='neg')/length(y)
  posPriorProb <- sum(y=='pos')/length(y)
  
  # probabiality of individual column
  # 각 변수의 column마다 예측 확률을 구하기 위하여, 각 column별로 예측 모델 생성성
  # predModelList <- as.list(NA)
  # for( i in 1:(ncol(train)-1) ){
  #   predModelList[[i]] <- glm(diabetes~train[,c(i)], data = train, family = "binomial")
  # }
  
  negCondProb <- as.list(NA)
  posCondProb <- as.list(NA)
  for( i in 1:ncol(trainDf) ){
    negDf <- data.frame( trainDf[y=='neg',i] , y[y=='neg'] )
    posDf <- data.frame( trainDf[y=='pos',i] , y[y=='pos'] )
    colnames(negDf) <- c("x", "class")
    colnames(posDf) <- c("x", "class")
    
    # negCondProb[[i]] <- glm(y~., data=tmp[y=='neg',], family="binomial")$coefficients
    # posCondProb[[i]] <- glm(y~., data=tmp[y=='pos',], family="binomial")$coefficients
    negCondProb[[i]] <- coef( glm(class~., negDf, family="binomial") )
    posCondProb[[i]] <- coef( glm(class~., posDf, family="binomial") )
  }
  model <- list( Prir_Prob = c(neg=negPriorProb,pos=posPriorProb), 
                 Cond_Prob = cbind(negCondProb, posCondProb) )
  return( model )
}


predict_ <- function( model, test ) {
  calConditionalProb <- function( testLine, model) {  # testLine : without Factor
    # testLine <- test[1,-9]
    negProb <- model[[1]][[1]] ## initial = negPriorProb
    posProb <- model[[1]][[2]] ## initial = posPriorProb
    
    condProb <- model[[2]] #predModelList
    
    for( i in 1:length(testLine) ){
      tmpProProb <- exp( condProb[[i]][[1]] + condProb[[i]][[2]]*testLine[[i]] ) /
        ( 1 + exp( condProb[[i]][[1]] + condProb[[i]][[2]]*testLine[[i]]) )
      posProb <- posProb * tmpProProb
      negProb <- negProb * (1 - tmpProProb)
    }
    
    # for( i in 1: nrow(test) ){
    #   posProb <-  posProb *
    #     as.numeric( predict(condProb[[i]], testLine[1], type = "response") ) *
    #     as.numeric( predict(condProb[[i]][[2]], testLine[2], type = "response") ) *
    #     as.numeric( predict(condProb[[i]][[3]], testLine[3], type = "response") )
    # 
    #   negProb <- posProb *
    #     ( 1 - as.numeric( predict(condProb[[i]][[1]], testLine[1], type = "response") )) *
    #     ( 1 - as.numeric( predict(condProb[[i]][[2]], testLine[2], type = "response") ))  *
    #     ( 1 - as.numeric( predict(condProb[[i]][[3]], testLine[3], type = "response") ))
    # }
    
    return (  ifelse( posProb > negProb , 'pos', 'neg') )
  }
  predList <- c()
  predList <- apply( test[, -ncol(test)], 1, function(x){ calConditionalProb(x, model) }  )
  
  return( as.factor(predList) )
}
