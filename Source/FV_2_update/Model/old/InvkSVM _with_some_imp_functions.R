################################################################################################## 
# InvkSVM.R                                                                                      #
# Description: Builds a model by using SVMs with linear / polynomial / RBF kernels               #
#                                                                                                #
# Input Variables:                                                                               #
#               ONE: If it is set to TRUE, an one-classifier is trained otherwise a binary       #
#               classifier is trained                                                            #
#               ThreshLim: Upper limit for undiagnosed patients to be identified as TSC patients,#
#               default: 50                                                                      #
#               kertype: Kernel function to be used in conjunction with SVMs                     #
#               TrFile1, VlFile, TrFile : Input files for training (1), validation,              #
#               training(trainging TSC + validation TSC) sets                                    #
#               TrRsFile : Output file for saving results on training data                       #
#              
#                                                                                                # 
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################

 

InvkSVM <- function(TrFile1,VlFile,TrFile,kertype,ONE,TrRsFile,ThreshLim) {  

  
  # Set parameters
  
  threshint <- c(0,ThreshLim)
  
  selROCAUC <- 0.0
  selTP <- 0
  
  nuv <- c(0.1,0.3, 0.5,0.7) # a set of values for nu 

  seev <- c(1,10,100) # a set of values for C
  
  gammav <- c(0.05, 0.1, 0.3, 0.5, 0.7, 1,2)  # a set of values for gamma
  
  degreev <- c(2,3)
  
  
  # Read data
  
  trainv <- read.matrix.csr(TrFile1,fac=TRUE)
  validation <- read.matrix.csr(VlFile,fac=TRUE)
  train <- read.matrix.csr(TrFile,fac=TRUE)
    
  #dimtrain <- slot(trainv$x,"dimension") cat("dimtrain\n",dimtrain)
  #dimvalid <- slot(validation$x,"dimension") cat("dimtvalid\n",dimvalid)
  
  ifelse(ONE,prmv <- nuv,prmv<- seev)
  ifelse(ONE,svmtype <- "one-classification",svmtype <- "C-classification")
  ifelse(ONE,wts <- NULL,wts <- c("-1"=.05,"1"=1))

  # 
  
  if (kertype == "linear") {
    
    validResMat <- matrix(nrow= length(prmv),ncol = 4,byrow=TRUE)
    
    for (iprm in 1 : length(prmv) ) {
        
      svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, class.weights = wts  )
      #print(summary(svm_model))
      pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
      
      predscore <- attr(pred, "decision.values")
      predobj <- prediction(predscore,validation$y)
      
      j <- 1
      
      iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
      
      iDR <- sum(predscore > 0 & validation$y == -1)

      iTP <- sum(predscore > 0 & validation$y == 1)
      validResMat[iprm,j] <-  prmv[iprm]
      j <- j + 1
      validResMat[iprm,j] <-  iDR
      j <- j + 1
      validResMat[iprm,j] <-  iTP
      j <- j + 1
      validResMat[iprm,j] <-  iROCAUC
      j <- j + 1
    }

  index <- which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[2] ))

  for (seli in 1 : length(index)) {
    if (validResMat[index[seli],4] > selROCAUC) {
      selROCAUC <- validResMat[index[seli],4]
      selTP <- validResMat[index[seli],3]
      selDR <- validResMat[index[seli],2]
      selprm <- validResMat[index[seli],1]
    }     
  }
  
  cat(" \n Selected Nu / C:", selprm,  " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n")
  
  cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, "\n",  "Validation ROCAUC: ", selROCAUC,  "Validation DR: ", selDR,  " Validation TP: ", selTP, "\n")
    
  svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost = selprm, kernel = kertype)
  
   #WeightVec <- t(svm_model$coefs) %*%  svm_model$SV
    #print(WeightVec)
    #NVec <- as.vector(WeightVec)
    #CVec <- as.vector(FtNames)
    #CVec <- as.vector(1:length(WeightVec))
    #WeightMat <- data.frame(NVec)
    #WeightMat <- WeightMat[rev(order(WeightMat$NVec)),]
    
    #WeightMat <- data.frame(CVec,NVec)
    #WeightMat <- WeightMat[rev(order(WeightMat$NVec)),]
    
    #FtFile <- file("c:/Users/hlodhi/Desktop/TSC/Data/FETR/FtFile1.txt",open="wt")
    #write.table(t(WeightMat),file=FtFile)
    #cat("\n",file=FtFile)
    #close(FtFile)

    pred <- predict(svm_model,train$x,sclae=FALSE,decision.values = TRUE)

    if (!ONE) {
      predscore <- attr(pred, "decision.values")
      predobj <- prediction(predscore,train$y)
    
      ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
      DR <- sum(predscore > 0 & train$y == -1)  # Number of undiagnosed patients identified with TSC
      TP <- sum(predscore > 0 & train$y == 1)  # True positive
    
      cat(" Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")  
      cat(file=TrRsFile,append=TRUE, "Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")
    }
  
    if (ONE) {
      TP <- sum(predscore > 0 & train$y == 1)
      cat("Training TP ",TP, "\n")
      cat(file=TrRsFile,append=TRUE,"Training TP ", TP, "\n")
    }
          
  }
  
##  
  if (kertype == "radial") {
    validResMat <- matrix(nrow= length(prmv) * length(gammav) ,ncol = 5,byrow=TRUE)
    k <- 1
    for (iprm in 1 : length(prmv) ) {
     
      for (ig in 1:length(gammav)) {
        
        sigma = gammav[ig]
      
        svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, gamma = sigma  )
        
        pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
        
        predscore <- attr(pred, "decision.values")
        predobj <- prediction(predscore,validation$y)
        
        j <- 1
        iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
        cat(" \n AUC",iROCAUC, "\n")
        iDR <- sum(predscore > 0 & validation$y == -1)
        iTP <- sum(predscore > 0 & validation$y == 1)
        cat("\n DR ",iDR, "\n")
        validResMat[k,j] <-  prmv[iprm]
        j <- j + 1
        validResMat[k,j] <-  sigma
        j <- j + 1
        validResMat[k,j] <-  iDR
        j <- j + 1
        validResMat[k,j] <-  iTP
        j <- j + 1
        validResMat[k,j] <-  iROCAUC
        j <- j + 1
        k <- k + 1
        cat("\n TP ", iTP)
        cat("\n DR ", iDR)
        
      }  
    }
    print(validResMat)
    index <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[2] )) 
    for (seli in 1 : length(index)) {
      #if (validResMat[index[seli],5] > selROCAUC) {
      if (validResMat[index[seli],4] > selTP) {
        selROCAUC <- validResMat[index[seli],5]
        selTP <- validResMat[index[seli],4]
        selDR <- validResMat[index[seli],3]
        selprm <- validResMat[index[seli],1]
        selgamma <- validResMat[index[seli],2]
      }     
    }
    
    cat(" Selected Nu / C:", selprm,  " Selected Sigma: ", selgamma, " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )
    
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Sigma: ", selgamma,"\n", " Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")
    
    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost= selprm,  kernel = kertype, gamma = selgamma)
    
    pred <- predict(svm_model,train$x,sclae=FALSE,decision.values = TRUE)
  
    if (!ONE) {
      predscore <- attr(pred, "decision.values")
      predobj <- prediction(predscore,train$y)
      ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
      PRBE <- performance(predobj, measure = "prbe")@y.values[[1]]      
      DR <- sum(predscore > 0 & train$y == -1)
      TP <- sum(predscore > 0 & train$y == 1)
    
      cat(" Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")  
    
      cat(file=TrRsFile,append=TRUE, " Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")
    }
    
    if (ONE) {
      TP <- sum(predscore > 0 & train$y == 1)
      cat(" Training TP ",TP, "\n")
      cat(file=TrRsFile,append=TRUE," Training TP ", TP, "\n")
    }
   
  }

##  
  if (kertype == "polynomial") {
    
    validResMat <- matrix(nrow= length(prmv) * length(degreev) ,ncol = 5,byrow=TRUE)
    
    k <- 1
    
    for (iprm in 1 : length(prmv) ) {
      
      for (id in 1:length(degreev)) {
    
          deg = degreev[id]
      
          svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, degree = deg, class.weights = wts  )
      
          pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
        
          predscore <- attr(pred, "decision.values")
          predobj <- prediction(predscore,validation$y)
            
          j <- 1
          iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
          iDR <- sum(predscore > 0 & validation$y == -1)
          iTP <- sum(predscore > 0 & validation$y == 1)
        
          validResMat[k,j] <-  prmv[iprm]
          j <- j + 1
          validResMat[k,j] <-  deg
          j <- j + 1
          validResMat[k,j] <-  iDR
          j <- j + 1
          validResMat[k,j] <-  iTP
          j <- j + 1
          validResMat[k,j] <-  iROCAUC
          j <- j + 1
          k <- k + 1 
      }  
    }

    index <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[2] )) 
    for (seli in 1 : length(index)) {
      if (validResMat[index[seli],5] > selROCAUC) {
        selROCAUC <- validResMat[index[seli],5]
        selTP <- validResMat[index[seli],4]
        selDR <- validResMat[index[seli],3]
        selprm <- validResMat[index[seli],1]
        seldeg <- validResMat[index[seli],2]
      }     
    }
    
    cat(" Selected Nu / C:", selprm,  " Selected Degree: ", seldeg, " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )    
    
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Degree: ", seldeg,"\n"," Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")    

    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm, cost = selprm,  kernel = kertype, degree = seldeg)

    pred <- predict(svm_model,train$x,sclae=FALSE,decision.values = TRUE)
    
    if (!ONE) {
      predscore <- attr(pred, "decision.values")
      predobj <- prediction(predscore,train$y)
      ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]  
      DR <- sum(predscore > 0 & train$y == -1)
      TP <- sum(predscore > 0 & train$y == 1)
      
      cat(" Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")  
      
      cat(file=TrRsFile,append=TRUE," Training ROCAUC: ", ROCAUC, " Training DR: ", DR, " Training TP: ", TP, "\n")
    }
    if (ONE) {
      TP <- sum(predscore > 0 & train$y == 1)
      cat(" Training TP ",TP, "\n")
      cat(file=TrRsFile,append=TRUE,"Training TP ", TP, "\n")
    }
    
  }

  return(svm_model)

}
  