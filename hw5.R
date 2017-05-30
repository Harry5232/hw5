#作業5 完成版 103753027 顏碩亨

rename <- function(file){  #擷取路徑中的檔案名稱 (去掉.csv)
  file2 <- c(file)
  file2 <- strsplit(file2,fixed=TRUE,"\\")[[1]]
  file2 <- strsplit(file2[length(file2)],fixed=TRUE,".csv")[[1]]
  return (file2[length(file2)])
}

#install.packages("cvTools")
library(cvTools)
library(stats) 
library('ROCR')
library(randomForest)
#library(pROC)
#------------------read parameters---------------
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("", call.=FALSE)
}else{
  
  #----------------parse parameters--------------
  i<-1 
  while(i < length(args))
  {
    if(args[i] == "--fold"){
      nfold<-args[i+1]
      i<-i+1
    
    }else if(args[i] == "--out"){
      out_f<-args[i+1]
      i<-i+1
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i<-i+1
  }
  
  #--------show status-----------
  set <- c()
  accuracy <- c()
  
  #--------read files------------
  d <- read.csv("Archaeal_tfpssm.csv",header=F)
  #levels(d[,2])
  #head(d[,5600:5602]) 

  f <- d[,3:5602]
  pca <- prcomp(x=f,center = TRUE, scale. = TRUE) 
  pca_top6 <- pca$x[,1:6]
  temp <- data.frame(ID=d[,1],target=d[,2],stringsAsFactors = FALSE)
  inte <- cbind(temp,pca_top6)
    
  k <- nfold #the number of folds
  
  folds <- cvFolds(nrow(inte), K=k)
  d$holdoutpred <- rep(0,nrow(inte))
  
  for(i in 1:k){
    train <- inte[folds$subsets[folds$which != i && folds$which != i+1], ] #Set the training set
    validation <- inte[folds$subsets[folds$which == i], ] #Set the validation set
    test <- inte[folds$subsets[folds$which == (i+1) %% k ], ] #Set the test set
    
    
    
    ob <- randomForest(target ~ PC1+PC2+PC3+PC4+PC5+PC6, data=train)
    pred_valid <- predict(ob,newdata=validation) #Get the predicitons for the validation set 
    pred_train <- predict(ob,newdata=train) #Get the predicitons for the train set 
    pred_test <- predict(ob,newdata=test) #Get the predicitons for the test set
    
    d[folds$subsets[folds$which == i], ]$holdoutpred <- newpred #Put the hold out prediction in the data set for later use
  }
  
  
  #------------------write data frame -----------
  #out_data<-data.frame(set, accuracy, stringsAsFactors = F)
  
  #----------------- output file ----------------

  #out_f <- paste0(out_f,".csv")
  #write.table(out_data, file=out_f, row.names = F, quote = F,sep=",")

}


