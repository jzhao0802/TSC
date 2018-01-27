
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
  selTP <- -1
  selwt <- NULL
  
  nuv <- c(0.3, 0.5,0.7,0.99) # a set of values for nu 

  seev <- c(1,10,100) # a set of values for C
  
  gammav <- c(0.001,0.01,0.1, 0.3, 0.5, 0.7,1,2,4,8,16)  # a set of values for gamma
  
  degreev <- c(2,3)
  
  
  # Read data
  
  trainv <- read.matrix.csr(TrFile1,fac=TRUE)
  validation <- read.matrix.csr(VlFile,fac=TRUE)
  train <- read.matrix.csr(TrFile,fac=TRUE)

  
  ifelse(ONE,prmv <- nuv,prmv<- seev)
  ifelse(ONE,svmtype <- "one-classification",svmtype <- "C-classification")
  ifelse(ONE,wtslst <- list(NULL),wtslst <- list( c("-1"=.05,"1"=1),c("-1"=.1,"1"=1)  ) )
  ifelse(ONE,wtctr <- 1,wtctr <- length(wtslst) )
  
  
##################################################################################################   

  # Linear kernel 
  
  if (kertype == "linear") {
    
    validResMat <- matrix(nrow= length(prmv)* wtctr,ncol = 4,byrow=TRUE)
    validResLst <- list(NULL)
    
    
    k <- 1
    for (iprm in 1 : length(prmv) ) {
      
      for (iwt in 1: wtctr) {  
        
        wt <- wtslst[[iwt]]
        
        svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, class.weights = wt  )
        
        pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
      
        predscore <- attr(pred, "decision.values")
        predobj <- prediction(predscore,validation$y)
      
        j <- 1
      
        iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
      
        iDR <- sum(predscore > 0 & validation$y == -1)

        iTP <- sum(predscore > 0 & validation$y == 1)
      
        validResMat[k,j] <-  prmv[iprm]
        j <- j + 1
        validResMat[k,j] <-  iDR
        j <- j + 1
        validResMat[k,j] <-  iTP
        j <- j + 1
        validResMat[k,j] <-  iROCAUC
        validResLst[[k]] <- wt
        j <- j + 1
        k <- k + 1
      }
    }
   print(validResMat)
    index <- which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[2] ))

    for (seli in 1 : length(index)) {
      if (validResMat[index[seli],3] > selTP) {
      #if (validResMat[index[seli],4] > selROCAUC) {
        selROCAUC <- validResMat[index[seli],4]
        selTP <- validResMat[index[seli],3]
        selDR <- validResMat[index[seli],2]
        selprm <- validResMat[index[seli],1]
        selwt <- validResLst[[seli]]
      }     
    }
  

    cat(" \n Selected Nu / C:", selprm, "Selected weights :", selwt, "\n",  " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n")
    readline("Press a key to continue")
   
  
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, "Selected weights:", selwt, "\n",  "Validation ROCAUC: ", selROCAUC,  "Validation DR: ", selDR,  " Validation TP: ", selTP, "\n")
    
    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost = selprm, kernel = kertype, class.weights = selwt)
  
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


################################################################################################## 
  
  # Gaussian RBF Kernel

  if (kertype == "radial") {
    validResMat <- matrix(nrow = length(prmv) * length(gammav) * wtctr,ncol = 5,byrow=TRUE)
    validResLst <- list(NULL)
    
    k <- 1
    
    for (iprm in 1 : length(prmv) ) {
     
      for (ig in 1 :length(gammav)) {
        
        for (iwt in 1 : wtctr) {
          
          wt <- wtslst[[iwt]]
        
          sigma = gammav[ig]
          
          svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, gamma = sigma, class.weights = wt  )
        
          pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
        
          predscore <- attr(pred, "decision.values")
          predobj <- prediction(predscore,validation$y)
        
          j <- 1
          
          iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
          iDR <- sum(predscore > 0 & validation$y == -1)
          iTP <- sum(predscore > 0 & validation$y == 1)

          validResMat[k,j] <-  prmv[iprm]
          j <- j + 1
          validResMat[k,j] <-  sigma
          j <- j + 1
          validResMat[k,j] <-  iDR
          j <- j + 1
          validResMat[k,j] <-  iTP
          j <- j + 1
          validResMat[k,j] <-  iROCAUC
          validResLst[[k]] <- wtslst[[iwt]]
          j <- j + 1
          k <- k + 1
        }  
      }  
    }
    
    print(validResMat)
   
   
    DRIndex <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[2] )) 
    index <- DRIndex
    print(DRIndex)
    TPIndex <- DRIndex[which(validResMat[DRIndex,4] > selTP)]
    print(TPIndex)
    
    MaxIndex <- DRIndex[which( validResMat[TPIndex,4] == max(validResMat[TPIndex,4]))] 
    print(MaxIndex)
    set.seed(2900000)
    selIndex <- sample(MaxIndex,1)
    print(selIndex)
    print(validResMat[selIndex,])
    for (seli in 1 : length(index)) {
      #if (validResMat[index[seli],5] > selROCAUC) {
      if (validResMat[index[seli],4] > selTP) {
        selROCAUC <- validResMat[index[seli],5]
        selTP <- validResMat[index[seli],4]
        selDR <- validResMat[index[seli],3]
        selprm <- validResMat[index[seli],1]
        selgamma <- validResMat[index[seli],2]
        selwt <- validResLst[[seli]]
      }     
    }
    
    cat(" Selected Nu / C:", selprm,  " Selected Sigma: ", selgamma, "Selected weights :", selwt, "\n", " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )
    readline("Press a key to continue") 
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Sigma: ", selgamma,"Selected weights :", selwt, "\n", " Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")
    
    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost= selprm,  kernel = kertype, gamma = selgamma, class.weights = selwt)
    
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


################################################################################################## 

 # Polynomial kernel

  if (kertype == "polynomial") {
    gamma <- 1
    coef <- 1
    validResMat <- matrix(nrow = length(prmv) * length(degreev) * wtctr ,ncol = 5,byrow=TRUE)
    validResLst <- list(NULL)
    
    
    k <- 1
    
    for (iprm in 1 : length(prmv) ) {
      
      for (id in 1 : length(degreev)) {
        
        for (iwt in 1 : wtctr) {
          
          wt <- wtslst[[iwt]]
    
          deg <- degreev[id]
      
          svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, degree = deg, class.weights = wt  )
      
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
          validResLst[[k]] <- wtslst[[iwt]]
          j <- j + 1
          k <- k + 1 
          
        }  
      }
    }

    index <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[2] )) 
print(validResMat)
print(validResMat)
    for (seli in 1 : length(index)) {
      
      #if (validResMat[index[seli],5] > selROCAUC) {
      if (validResMat[index[seli],4] > selTP) {
        selROCAUC <- validResMat[index[seli],5]
        selTP <- validResMat[index[seli],4]
        selDR <- validResMat[index[seli],3]
        selprm <- validResMat[index[seli],1]
        seldeg <- validResMat[index[seli],2]
        selwt <- validResLst[[seli]]
      }     
    }
    
    cat(" Selected Nu / C:", selprm,  " Selected Degree: ", seldeg, "Selected weights :", selwt, "\n"," ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )    
    readline("Press a key to continue")
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Degree: ", seldeg, "Selected weights :", selwt,"\n"," Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")    

    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm, cost = selprm,  kernel = kertype, degree = seldeg, class.weights = selwt)

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
  