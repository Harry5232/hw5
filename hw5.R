#作業3 完成版 103753027 顏碩亨

rename <- function(file){
  file2 <- c(file)
  file2 <- strsplit(file2,fixed=TRUE,"\\")[[1]]
  file2 <- strsplit(file2[length(file2)],fixed=TRUE,".csv")[[1]]
  return (file2[length(file2)])
}

library('ROCR')
#library(pROC)
#------------------read parameters---------------
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("", call.=FALSE)
}else{
  
  #----------------parse parameters----------------
  i<-1 
  while(i < length(args))
  {
    if(args[i] == "--target"){
      query_m<-args[i+1]
      i<-i+1
    }else if(args[i] == "--files"){
      j<-grep("--", c(args[(i+1):length(args)], "--"))[1]
      files<-args[(i+1):(i+j-1)]
      i<-i+j-1
    }else if(args[i] == "--out"){
      out_f<-args[i+1]
      i<-i+1
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i<-i+1
  }
  
  #-----------show status-----------
  print("PROCESS")
  print(paste("positive target :", query_m))
  print(paste("output file:", out_f))
  print(paste("files      :", files))
  print("Please wait...")
  
  
  method <- c()
  method2 <- c()
  sensitivity <- c() 
  specificity <- c()
  F1 <- c()
  AUCs <- c()
  sens_max <- list(num=0,name="")
  spec_max <- list(num=0,name="")
  F1_max <- list(num=0,name="")
  AUC_max <- list(num=0,name="")
  
  
  #-----------read files------------
  for(file in files)
  {
    TP <- 0
    TN <- 0
    FP <- 0
    FN <- 0
    Pre <- 0 #precision
    Rec <- 0 #recall 
    AUC <- 0 #AUC
    Pre.score <- c()
    Pre.label <- c()
    
    #------------build confusion table---------
    d<-read.table(file, header=T,sep=",")
    for(num in c(1:nrow(d))){
      if(d[num,2] == d[num,3]){
        if(d[num,3] == query_m){
          TP <- TP + 1
        }else{
          TN <- TN + 1
        }
        
      }else if(d[num,2] != d[num,3]){
        if(d[num,3] == query_m){
          FN <- FN + 1
        }else{
          FP <- FP + 1
        }
      }
      
    }
    
    #-----------calculate sensitivity specificity F1 AUC ----------
    Pre <- TP/(TP+FP)
    Rec <- TP/(TP+FN)
    f1 <- (2 * Pre * Rec)/(Pre + Rec)
    f1 <- round(f1,digits = 2)
    F1 <- c(F1,f1)
    sen <- (TP/(TP + FN))
    sen <- round(sen,digits = 2)
    sensitivity <- c(sensitivity,sen)
    Spe <- (TN/(TN + FP))
    Spe <- round(Spe,digits = 2)
    specificity <- c(specificity,Spe)
    
    method <- c(method,file)
    
    method2 <- c(method2,rename(file))
    
    if(query_m == "female"){
      Pre.score <- 1 - c(d$pred.score)
    }else{
      Pre.score <- c(d$pred.score)  
    }
    
    #Pre.label <- c(d$reference)
    Pre.label <- c(d$reference == query_m)
    #AUC <- auc(multiclass.roc(Pre.score,Pre.label), min = 0, max = 1)
    AUC <- prediction(Pre.score,Pre.label)
    AUC <- round(attributes(performance(AUC, 'auc'))$y.values[[1]], 2)
    #AUC <- round(AUC,digits = 2)
    AUCs <- c(AUCs,AUC)
    
  }
  
  #---------------用來判斷是否顯著(Nimaer test)和修改檔名-------------
  tran <- function(m){
    t1_name <- m[nrow(m),1]
    t1 <- read.table(t1_name, header=T,sep=",")
    t2_name <- m[nrow(m)-1,1]
    t2 <- read.table(t2_name,header = T,sep = ",")
    pp <- 0
    pn <- 0
    np <- 0
    nn <- 0
    for(num in c(1:nrow(t1))){
      if(t1[num,2] == t2[num,2]){
        if(query_m == t1[num,2]){
          pp <- pp+1
        }else{
          nn <- nn+1
        }
      }else{
        if(query_m == t1[num,2]){
          pn <- pn+1
        }else{
          np <- np+1
        }
      }
    }
    d <- rbind(c(pp,pn),c(np,nn))
    
    f <- mcnemar.test(d,correct = TRUE)
    t1_name <- rename(t1_name)
    
    #(m[nrow(m),1]) <<- rename((m[nrow(m),1])) 
    
    if(f$p.value < 0.05){
      return (paste0(t1_name,"*"))
    }else{
      return (t1_name)
    }
    #f <- fisher.test(d)
    #print(f)
    #print((f$p.value))
    
  }
  
  sens_data <- data.frame(method,sensitivity, stringsAsFactors = F)
  sens_data <- sens_data[with(sens_data,order(sensitivity)),]  #從小到大排序
  sens_data[nrow(sens_data),1] <- tran(sens_data) #顯著判斷和更新檔名
  
  spec_data <- data.frame(method,specificity, stringsAsFactors = F)
  spec_data <- spec_data[with(spec_data,order(specificity)),]
  spec_data[nrow(spec_data),1] <- tran(spec_data)
  
  F1_data <- data.frame(method,F1, stringsAsFactors = F)
  F1_data <- F1_data[with(F1_data,order(F1)),]
  F1_data[nrow(F1_data),1] <- tran(F1_data)
  
  AUC_data <- data.frame(method,AUCs, stringsAsFactors = F)
  AUC_data <- AUC_data[with(AUC_data,order(AUCs)),]
  AUC_data[nrow(AUC_data),1] <- tran(AUC_data)
  
  #------------------write data frame -----------------
  out_data<-data.frame(method=method2, sensitivity, specificity, F1, AUC=AUCs, stringsAsFactors = F)
  #print(F1_data[5,])
  #------------- add final row to the end of the table-----------------
  out_data[nrow(out_data)+1,] <- c("highest",sens_data[nrow(sens_data),1],spec_data[nrow(spec_data),1],F1_data[nrow(F1_data),1],AUC_data[nrow(AUC_data),1])
  print(out_data)
  #----------------- output file -----------------
  out_f <- paste0(out_f,".csv")
  write.table(out_data, file=out_f, row.names = F, quote = F,sep=",")
  
}

#作業3 完成版 103753027 顏碩亨
