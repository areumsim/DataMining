
Bayes_ <- function(Class, df){ 
  neg <- levels(Class)[1]
  pos <- levels(Class)[2]
  prob_neg <- sum(Class==neg)/nrow(df)   # 1- prob_pos
  prob_pos <- sum(Class==pos)/nrow(df)
  prob_ <- c( neg = prob_neg, pos = prob_pos)
  
  condtional_Prob <- list()
  for( i in 1:ncol(df)){
    n <- levels(df[,i])[1]
    y <- levels(df[,i])[2]
    
    prob_n_neg <- sum( df[Class!=pos,i]!=y , na.rm = T ) / sum( Class != pos )
    cond_prob_neg <- c( n = prob_n_neg, y = 1-prob_n_neg)
    
    prob_n_pos <- sum( df[Class==pos,i]!=y , na.rm = T ) / sum( Class == pos )
    cond_prob_pos <- c( n = prob_n_pos, y = 1-prob_n_pos)
    
    condtional_Prob[[i]] <- rbind(neg=cond_prob_neg, pos=cond_prob_pos)
  }
  
  return ( list( "Probabilities" = prob_ ,
                 "Conditional probabilities" = condtional_Prob )  )
}


predict_ <- function( model , df ){
  pred_List <- rep(0, nrow(df))
 
  # prior probability
  prob_neg <-  model$Probabilities[[1]]
  prob_pos <- model$Probabilities[[2]]
  
  condtional_Prob <- model$`Conditional probabilities`
  
  for( j in 1:nrow(df) ){
    tmp_prob_neg <- prob_neg
    tmp_prob_pos <- prob_pos
    
    for( i in 1:ncol(df) ){
      n <- colnames(condtional_Prob[[i]])[1]
      
      if( df[j,i]==n ) {
        tmp_prob_neg <- tmp_prob_neg * condtional_Prob[[i]][1,1]
        tmp_prob_pos <- tmp_prob_pos * condtional_Prob[[i]][2,1]
      } else {
        tmp_prob_neg <- tmp_prob_neg * condtional_Prob[[i]][1,2]
        tmp_prob_pos <- tmp_prob_pos * condtional_Prob[[i]][2,2]
      }
    }
    ifelse( tmp_prob_neg > tmp_prob_pos,   pred_List[j]  <- "democrat",   pred_List[j]  <- "republican" )
  }
  
  pred_List <- as.factor(pred_List)
  return( pred_List )
}


### ### Data onlyBinary ### ###
data(HouseVotes84, package = "mlbench")
data <- na.omit(HouseVotes84)

### Run ### 
model <- Bayes_( data[,1], data[,-1])
pred <- predict_(model, data[,-1])
table(pred, data$Class)

### Refer . using R method ###
library(e1071)
model_ref <- naiveBayes(Class ~ ., data = data)
pred_ref <- predict(model_ref, data[,-1])
table(pred_ref, data$Class)

