# DataMining

---

## 1. Logistric Regrssion (binary)
  - with optim() 
  - without optim()

## 2. Naive Bayes
  - only binary data
  - TODO : Numerical data

## 3. CART
  - Test1 (iris)
  ---
  > df <- iris
  > rpart_(5, df, entropyThreshold=0.2)
  0 :Petal.Length <= 1.9 
  0 :L- < setosa >
  1 :	Petal.Width <= 1.8 
  1 :	L- < versicolor >
  1 :	R- < virginica >
  
  - Test2 ( USArrests )
  > df <- USArrests
  > df$UrbanPop1 <- ifelse(df$UrbanPop> median(USArrests$UrbanPop), "Large", "Small")
  > df <- df[, names(df)!="UrbanPop"] # UrbanPop 제외
  > rpart_(4, df,  entropyThreshold=0.5)\
   0 :Rape <= 22.5 
   1 :	Murder <= 2.7 
   1 :	L- < Small >
   2 :		Murder <= 7.4 
   2 :		L- < Large >
   2 :		R- < Small >
   0 :R- < Large >
   ---

## 4. K-means

## 4. K-NN

## 5. NN
  - sequential leaning ( Wkj, Whj )
  - online learing ( optim )
