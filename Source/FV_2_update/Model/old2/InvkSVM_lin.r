
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

#TrRsFile <- TrVlTsRsFile

InvkSVM <- function(TrFile1,VlFile,TrFile,kertype,ONE,TrRsFile,ThreshLim,counter,ModelFile) {  


  
  # Set parameters
  
  threshint <- c(1,0,ThreshLim)
  
  selROCAUC <- 0.0
  selTP <- -1
  selwt <- NULL
  
  sval <- counter * 590700
  
  nuv <- c(0.5,0.7,0.9) # a set of values for nu for one-class SVMs

  seev <- c(200,300,400) # a set of values for C for binary SVMs
  #seev <- c(100) # a set of values for C for binary SVMs
  #seev <- seq(1, 101, 10)# a set of values for C for binary SVMs
  
  gammav <- c(0.1, 0.3, 0.5)  # a set of values for gamma for binary SVM (Use this set for binary classifiication)
  
  
  #gammav <- c(2.5,2.7,2.9)  # a set of values for gamma for one-class SVM (Use this set for one-class classification)
  
  degreev <- c(2)
  
  
  # Read data
  trainv <- read.matrix.csr(TrFile1,fac=TRUE)
  validation <- read.matrix.csr(VlFile,fac=TRUE)
  train <- read.matrix.csr(TrFile,fac=TRUE)

  
  ifelse(ONE,prmv <- nuv,prmv<- seev)
  ifelse(ONE,svmtype <- "one-classification",svmtype <- "C-classification")
  #ifelse(ONE,wtslst <- list(NULL),wtslst <- list(c("-1"=.05,"1"=1),  c("-1"=.1,"1"=1) ))
  ifelse(ONE,wtslst <- list(NULL),wtslst <- list(c("-1"=.05,"1"=1), c("-1"=.1,"1"=1) ))
  #ifelse(ONE,wtslst <- list(NULL),wtslst <- list( c("-1"=.0001,"1"=1), c("-1"=.0234,"1"=1),c("-1"=.05,"1"=1), c("-1"=.1,"1"=1) ) )  #added by Jie to add 2 reasonable wt.
  ifelse(ONE,wtctr <- 1,wtctr <- length(wtslst) )
  
  
##################################################################################################   

  # Linear kernel 
  
  if (kertype == "linear") {
      seev <- c(200,300,400) # a set of values for C for binary SVMs
  ifelse(ONE,wtslst <- list(NULL),wtslst <- list(c("-1"=.05,"1"=1), c("-1"=.1,"1"=1) ))

    validResMat <- matrix(nrow= length(prmv)* wtctr,ncol = 4,byrow=TRUE)
    validResLst <- list(NULL)
    
    
    k <- 1
    for (iprm in 1 : length(prmv) ) {
      
      for (iwt in 1: wtctr) {  
        
        wt <- wtslst[[iwt]]
        
        svm_model <- svm(x = trainv$x, y = trainv$y, scale = FALSE, type = svmtype, nu = prmv[iprm],  cost= prmv[iprm],  kernel = kertype, class.weights = wt  )
        
        pred <- predict(svm_model,validation$x,sclae=FALSE,decision.values = TRUE)
      
        predscore <- attr(pred, "decision.values")
		predscoreVl <- predscore  #added by Jie to store the validation predscore
        predobj <- prediction(predscore,validation$y)
		#add plot
		#pdf(file=paste('D:/RareDisease_TSC/Results/FV_2_update/2n_lin/recall-precision-curve_simulation', counter, '.pdf', sep=''))
      	#	perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
		#	plot(perf, main=paste('simulation:', counter, sep=''))
#add plot end
        j <- 1
      
        iROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
      
        iDR <- sum(predscore > 0 & validation$y == -1)#false positive number

        iTP <- sum(predscore > 0 & validation$y == 1)#true postive number
      
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
    
    ifelse(invalid(DRIndex), DRIndex <- which(validResMat[, 2]==min(validResMat[, 2])), DRIndex) #added by Jie to solve the invalid DRIndex problem
    TPIndex <- DRIndex[which(validResMat[DRIndex,3] > selTP)]
   
    MaxIndex <- DRIndex[which( validResMat[TPIndex,3] == max(validResMat[TPIndex,3]))] 
   
   
    if (length(MaxIndex) > 1) {
      #set.seed(sval) 
      #selIndex <- sample(MaxIndex,1) 
	  flag <- validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3]))
	  n <- sum(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))
	  if(n>1){
			selIndex_all <- MaxIndex[which(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))]
			set.seed(sval) 
			selIndex <- sample(selIndex_all,1)
	  }else{
			selIndex <- MaxIndex[which(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))]
	  }
    }
    if (length(MaxIndex) == 1) {
      selIndex <- MaxIndex
    }
    
    selROCAUC <- validResMat[selIndex,4]
    selTP <- validResMat[selIndex,3]
    selDR <- validResMat[selIndex,2]
	selPPV <- selTP/(selTP+selDR) #added by Jie 
    selprm <- validResMat[selIndex,1]
		DNumFile <-  "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin/DNumRecord.csv" #added by Jie
   		cat(file=DNumFile, append=TRUE, 'Simulation ', counter, ': Validation set selDR = ', selDR,'\n') #added by Jie
    if (!invalid(validResLst))
      selwt <- validResLst[[selIndex]]
   	valid_auc <- c(counter, selROCAUC, selDR, selTP, selPPV, selprm, selwt[1]) #added by Jie
	
		CselectedFile <-  "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin/CselectedFile.csv" #added by Jie
   		cat(file=CselectedFile, append=TRUE, 'Simulation:', counter, " Selected Nu / C:", selprm, "Selected weights :", selwt, "\n",  "ROCAUC: ", selROCAUC, "DR: ", selDR, "TP: ", selTP, " PPV: ", selPPV , "\n") #added by Jie
    cat('Simulation:', counter, " Selected Nu / C:", selprm, "Selected weights :", selwt, "\n",  " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP,"\n")
   
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
          
	result_invkSVM <- list()
	result_invkSVM <- list(model=svm_model, score=predscoreVl, valid_auc=valid_auc)
	return(result_invkSVM)
  }


################################################################################################## 
  
  # Gaussian RBF Kernel

  if (kertype == "radial") {
    seev <- c(200,300,400) # a set of values for C for binary SVMs
  #ifelse(ONE,wtslst <- list(NULL),wtslst <- list(c("-1"=.05,"1"=1), c("-1"=.1,"1"=1) ))

	#seev <- seev_rbf #added by Jie
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
		  		predscoreVl <- predscore  #added by Jie to store the validation predscore

          predobj <- prediction(predscore,validation$y)
          length(attr(predobj, 'prediction')[[1]])  #20050
		  		#add plot
		#pdf(file=paste('D:/RareDisease_TSC/Results/FV_2_update/2n_rbf/recall-precision-curve', '.pdf', sep=''), width=11, height=20)
      	#	perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
		#	plot(perf, main=paste('simulation:', counter, sep=''))
#add plot end

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
    #modified by Jie to change the criteria to select the optimum model
    ifelse( length(which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] ))) == 0, DRIndex <- which((validResMat[,3] >= threshint[2]) & (validResMat[,3] <= threshint[3] )), DRIndex <- which((validResMat[,3] >= threshint[1]) & (validResMat[,3] <= threshint[3] )) )
    #obtain DRIndex according to whether #of FP ==0
    #DRIndex means training dataset index whose # of DR falling in [0, 150] or  [1, 150]
    ifelse(invalid(DRIndex), DRIndex <- which(validResMat[, 3]==min(validResMat[, 3])), DRIndex) #added by Jie to solve the invalid DRIndex problem
    TPIndex <- DRIndex[which(validResMat[DRIndex,4] > selTP)]
    
    MaxIndex <- DRIndex[which( validResMat[TPIndex,4] == max(validResMat[TPIndex,4]))] 
  
    if (length(MaxIndex) > 1) {
      #set.seed(sval) 
      #selIndex <- sample(MaxIndex,1) 
	  flag <- validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3]))
	  n <- sum(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))
	  if(n>1){
			selIndex_all <- MaxIndex[which(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))]
			set.seed(sval) 
			selIndex <- sample(selIndex_all,1)
	  }else{
			selIndex <- MaxIndex[which(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])==max(validResMat[MaxIndex,3]/(validResMat[MaxIndex,2]+validResMat[MaxIndex,3])))]
	  }
    }
    if (length(MaxIndex) == 1) {
      selIndex <- MaxIndex
    }
    
        
    selROCAUC <- validResMat[selIndex,5]
    selTP <- validResMat[selIndex,4]
    selDR <- validResMat[selIndex,3]
		selPPV <- selTP/(selTP+selDR) #added by Jie 

    selprm <- validResMat[selIndex,1]
    selgamma <- validResMat[selIndex,2]
		#valid_auc <- c(counter, selROCAUC, selDR, selTP, selPPV) #added by Jie

			DNumFile <-  "D:/RareDisease_TSC/Results/FV_2_update/2n_rbf/Rs/rbf/DNumRecord.csv"
   		cat(file=DNumFile, append=TRUE, 'Simulation ', counter, ': Validation set selDR = ', selDR,'\n')

    
    if (!invalid(validResLst))
      selwt <- validResLst[[selIndex]]
   	valid_auc <- c(counter, selROCAUC, selDR, selTP, selPPV, selprm, selwt[1], selgamma) #added by Jie

    		CselectedFile <-  "D:/RareDisease_TSC/Results/FV_2_update/2n_rbf/Rs/rbf/CselectedFile.csv"
   		cat(file=CselectedFile, append=TRUE, 'Simulation:', counter, " Selected Nu / C:", selprm,  " Selected Sigma: ", selgamma, "Selected weights :", selwt, "\n",  " ROCAUC: ", selROCAUC, "DR: ", selDR, " TP: ", selTP, "PPV: ", selPPV, "\n")

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
	result_invkSVM <- list()
	result_invkSVM <- list(model=svm_model, score=predscoreVl, valid_auc=valid_auc)
 return(result_invkSVM)
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
  