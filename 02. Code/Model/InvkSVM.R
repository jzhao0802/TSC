
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
#               training(training TSC + validation TSC) sets                                    #
#               TrRsFile : Output file for saving results on training data                       #
#               counter : Number of simulation                                                   #
#               ModelFile : Output file for saving SVM model
#                                                                                                #
#                                                                                                # 
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################

 

InvkSVM <- function(TrFile1,VlFile,TrFile,kertype,ONE,TrRsFile,ThreshLim,counter,ModelFile) {  

  
  # Set parameters
  
  threshint <- c(1,0,ThreshLim)
  
  selROCAUC <- 0.0
  selTP <- -1
  selwt <- NULL
  
  sval <- counter * 590700
  
  nuv <- c(0.5,0.7,0.9) # a set of values for nu for one-class SVMs

  seev <- c(1,10,100) # a set of values for C for binary SVMs
  
  
  #gammav <- c(0.1, 0.3, 0.5)  # a set of values for gamma for binary SVM (Use this set for binary classifiication)
  
  
  gammav <- c(2.5,2.7,2.9)  # a set of values for gamma for one-class SVM (Use this set for one-class classification)
  
  degreev <- c(2)
  
  
  # Read data
  trainv <- read.matrix.csr(TrFile1,fac=TRUE)
  validation <- read.matrix.csr(VlFile,fac=TRUE)
  train <- read.matrix.csr(TrFile,fac=TRUE)

  
  ifelse(ONE,prmv <- nuv,prmv<- seev)
  ifelse(ONE,svmtype <- "one-classification",svmtype <- "C-classification")
  #ifelse(ONE,wtslst <- list(NULL),wtslst <- list( c("-1"=.01,"1"=1),c("-1"=.05,"1"=1), c("-1"=.1,"1"=1)  ) )
  ifelse(ONE,wtslst <- list(NULL),wtslst <- list( c("-1"=.05,"1"=1), c("-1"=.1,"1"=1)  ) )
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
    
    ifelse( length(which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[3] ))) == 0, DRIndex <- which((validResMat[,2] >= threshint[2]) & (validResMat[,2] <= threshint[3] )), DRIndex <- which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[3] )) )
    
    TPIndex <- DRIndex[which(validResMat[DRIndex,3] > selTP)]
   
    MaxIndex <- DRIndex[which( validResMat[TPIndex,3] == max(validResMat[TPIndex,3]))] 
   
   
    if (length(MaxIndex) > 1) {
      set.seed(sval) 
      selIndex <- sample(MaxIndex,1) 
    }
    if (length(MaxIndex) == 1) {
      selIndex <- MaxIndex
    }
    
    selROCAUC <- validResMat[selIndex,4]
    selTP <- validResMat[selIndex,3]
    selDR <- validResMat[selIndex,2]
    selprm <- validResMat[selIndex,1]
   
    if (!invalid(validResLst))
      selwt <- validResLst[[selIndex]]
   
    cat(" \n Selected Nu / C:", selprm, "Selected weights :", selwt, "\n",  " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n")
   
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, "Selected weights:", selwt, "\n",  "Validation ROCAUC: ", selROCAUC,  "Validation DR: ", selDR,  " Validation TP: ", selTP, "\n")
    
    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost = selprm, kernel = kertype, class.weights = selwt)
    
    write.svm(svm_model,ModelFile) 
    
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
      predscore <- attr(pred, "decision.values")
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
    iprm <- prmv[1]
    ig <- gammav[1]
    iwt <- wtctr[1]
    
    for (iprm in 1 : length(prmv) ) {
     
      for (ig in 1 :length(gammav)) {
        
        for (iwt in 1 : wtctr) {
          
          wt <- wtslst[[iwt]]
        
          sigma = gammav[ig]
          
          svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, gamma = sigma, class.weights = wt  )
          attr(trainv$x, 'dim') #[1] 234  5181
          attr(validation$x, 'dim') #[1] 20050  5181
          pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
        
          predscore <- attr(pred, "decision.values")   #20050
          predobj <- prediction(predscore,validation$y)
          length(attr(predobj, 'prediction')[[1]])  #20050
          j <- 1
          
          iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
          iDR <- sum(predscore > 0 & validation$y == -1)   #False positive num
          iTP <- sum(predscore > 0 & validation$y == 1)     #TRue positive num
           
          validResMat[k,j] <-  prmv[iprm]
          j <- j + 1
          validResMat[k,j] <-  sigma
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
    }
    
    ifelse( length(which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] ))) == 0, DRIndex <- which((validResMat[,3] >= threshint[2]) & (validResMat[,3] <= threshint[3] )), DRIndex <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] )) )
    #obtain DRIndex according to whether #of FP ==0
    #DRIndex means training dataset index whose # of DR falling in [0, 150] or  [1, 150]
    TPIndex <- DRIndex[which(validResMat[DRIndex,4] > selTP)]
    
    MaxIndex <- DRIndex[which( validResMat[TPIndex,4] == max(validResMat[TPIndex,4]))] 
  
      if (length(MaxIndex) > 1) {
      set.seed(sval) 
      selIndex <- sample(MaxIndex,1)
    }
    if (length(MaxIndex) == 1) {
      selIndex <- MaxIndex
    }
        
    selROCAUC <- validResMat[selIndex,5]
    selTP <- validResMat[selIndex,4]
    selDR <- validResMat[selIndex,3]
    selprm <- validResMat[selIndex,1]
    selgamma <- validResMat[selIndex,2]
    
    if (!invalid(validResLst))
      selwt <- validResLst[[selIndex]]
    
    cat(" Selected Nu / C:", selprm,  " Selected Sigma: ", selgamma, "Selected weights :", selwt, "\n", " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )

    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Sigma: ", selgamma,"Selected weights :", selwt, "\n", " Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")
    
    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm,  cost= selprm,  kernel = kertype, gamma = selgamma, class.weights = selwt)
    
    write.svm(svm_model,ModelFile)
    
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
      predscore <- attr(pred, "decision.values")
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

    
    ifelse( length(which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] ))) == 0, DRIndex <- which((validResMat[,3] >= threshint[2]) & (validResMat[,3] <= threshint[3] )), DRIndex <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] )) )
    
    TPIndex <- DRIndex[which(validResMat[DRIndex,4] > selTP)]
    
    MaxIndex <- DRIndex[which( validResMat[TPIndex,4] == max(validResMat[TPIndex,4]))] 
    
    if (length(MaxIndex) > 1) {
      set.seed(sval) 
      selIndex <- sample(MaxIndex,1) 
    }
    if (length(MaxIndex) == 1) {
      selIndex <- MaxIndex
    }
   
    selROCAUC <- validResMat[selIndex,5]
    selTP <- validResMat[selIndex,4]
    selDR <- validResMat[selIndex,3]
    selprm <- validResMat[selIndex,1]
    seldeg <- validResMat[selIndex,2]
    if (!invalid(validResLst))
      selwt <- validResLst[[selIndex]]
    
      
    cat(" Selected Nu / C:", selprm,  " Selected Degree: ", seldeg, "Selected weights :", selwt, "\n"," ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n" )    
    readline("Press a key to continue")
    cat(file=TrRsFile,append=TRUE,"Kernel Type ", kertype, "Selected Nu / C:", selprm, " Selected Degree: ", seldeg, "Selected weights :", selwt,"\n"," Validation ROCAUC: ", selROCAUC, "Validation DR: ", selDR, " Validation TP: ", selTP,"\n")    

    svm_model <- svm(x = train$x, y = train$y, scale = FALSE, type = svmtype, nu = selprm, cost = selprm,  kernel = kertype, degree = seldeg, class.weights = selwt)

    write.svm(svm_model,ModelFile)
    
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
      predscore <- attr(pred, "decision.values")
      TP <- sum(predscore > 0 & train$y == 1)
      cat(" Training TP ",TP, "\n")
      cat(file=TrRsFile,append=TRUE,"Training TP ", TP, "\n")
    }
    
  }

  return(svm_model)

}
  