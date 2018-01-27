################################################################################################## 
# main.R                                                                                         #
# Description: Main program for identification of TSC patients  and  undiagnosed patients        #
#              who may have TSC                                                                  #
#              Perform one-class and binary classification. Build model using training set,      #
#              tune parameters using validation set, and perform evaluation on test set          #
#                                                                                                #
# Variables:                                                                                     #
#               PARALLEL : Set to TRUE for parallel processing otherwise FALSE, default: TRUE    #
#               ONE: Set to TRUE for one-class classification and FALSE for binary classification#
#               NumSplit: Number of split for non-TSC test data, default: 8                      #
#               NumUnDiag: Number of non-TSC patients in test set, default: approx 70000         #
#               ThreshLim: Upper limit for undiagnosed patients to be identified as TSC patients,#
#               default: 50                                                                      #
#               NumSimulation: Number of simulations, default: 10                                #
#               kertype: Kernel function to be used in conjunction with SVMs                     #
#               TrFile1, VlFile, TrFile : Input files for training (1), validation,              #
#               training(training TSC + validation TSC) sets                                     #
#               TsFilePos, TsFileNeg : Input files for test TSC and test non-TSC                 #
#                patients respectively                                                           #      
#               Ext : Files extension                                                            #
#               TSCIndexFile, ConTSCIndexFile: Output files to store indices and scores of       #
#               undiagnosed TSC  patients (classified as TSC by SVMs)  and confirmed TSC test    #
#               patients                                                                         #
#               TrVlTsRsFile : Output file for saving results                                    #
#               AvgRsFile : Output file for saving results                                       #
#               ModelFile : Output file for saving model learnt by SVM                           #
#                                                                                                #
#                                                                                                # 
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################



 # Load required packages 

  library("snowfall")
  sfLibrary("snow",character.only = TRUE)
  sfLibrary("snowfall", character.only = TRUE)
  sfLibrary("MASS", character.only = TRUE)
  sfLibrary("e1071", character.only = TRUE)
  sfLibrary("ROCR",character.only = TRUE)
  sfLibrary("pracma",character.only = TRUE)
  sfLibrary("gtools",character.only = TRUE)

 # Source files
  
  sfSource("D:/Jie/RareDisease_TSC/02. Code/Model/InvkSVM.R")
  sfSource("D:/Jie/RareDisease_TSC/02. Code/Model/PredSVM.R")
  

  PARALLEL <- TRUE  # For parallel processing (TRUE)
  
  ONE <- FALSE      # For one-class classification (TRUE)
  
  NumSplit <- 8      # Number of splits for test data (for parallel processing)
    
  ThreshLim <- 50 # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)

  #ThreshLim <- 150 # Uncomment this line for one-class classification using RBF kernels (when ONE is set to TRUE and kertype is set to radial, set ThresLim to 150)

  #ThreshLim <- 700 # Uncomment this line for one-class classification using linear kernels (when ONE is set to TRUE and kertype is set to linear, set ThresLim to 150)
  
  NumSimulation <- 1 # Number of simulationsline

  NumMsr <- 5

  ResMat <- matrix(nrow=NumSimulation+1,ncol=NumMsr,byrow=TRUE)

################################################################################################## 

 # Model Building and Identificaton of TSC

  #kertype <- "linear"  # Kernel function to be used in conjunction with SVMs

  kertype <- "radial"  
  
  #kertype <- "polynomial" # Dont use this kernel for identification of undiagnosed TSC patients
#counter<- 1
  for (counter in 1:NumSimulation) {
    tm <- proc.time()
  # Input files (Training set, validation set and test set)
  
    #TrFile1 <- "D:/RareDisease_TSC/Data/FV_1/1n/trainv"  #dim=[234, ]
    #TrFile <- "D:/RareDisease_TSC/Data/FV_1/1n/train"     #dim=[284, ]
    #VlFile <- "D:/RareDisease_TSC/Data/FV_1/1n/valid"      #dim=[1048576, ]
    #TsFilePos <- "D:/RareDisease_TSC/Data/FV_1/1n/testpos"   #dim=[50, ]
    #TsFileNeg <- "D:/RareDisease_TSC/Data/FV_1/1n/testneg"    #dim=[8750, ]
    
	  TrFile1 <- "D:/Jie/RareDisease_TSC/01. Data/FV_2/1/trainv"  #dim=[234, ]
    TrFile <- "D:/Jie/RareDisease_TSC/01. Data/FV_2/1/train"     #dim=[284, ]
    VlFile <- "D:/Jie/RareDisease_TSC/01. Data/FV_2/1/valid"      #dim=[1048576, ]
    TsFilePos <- "D:/Jie/RareDisease_TSC/01. Data/FV_2/1/testpos"   #dim=[50, ]
    TsFileNeg <- "D:/Jie/RareDisease_TSC/01. Data/FV_2/1/testneg"    #dim=[8750, ]

  # Output file (Index and result files)  
    
    TSCIndexFile  <- "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/Indices/rbf/ProspectiveTSC"
    ConTSCIndexFile  <- "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/Indices/rbf/ConfirmedTSC"

    ModelFile <- "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/Model/rbf/SVModel"
  
    TrVlTsRsFile <- "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/Rs/rbf/TrVlTsResFile"
    AvgRsFile <- "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/Rs/rbf/AvgResFile"
    
  
    Ext <- ".csv"
    
    TrFile1 <- paste(TrFile1,counter,Ext,sep="")
    TrFile <- paste(TrFile,counter,Ext,sep="")
    VlFile <- paste(VlFile,counter,Ext,sep="")
    TrVlTsRsFile <- paste(TrVlTsRsFile,counter,Ext,sep="")
    AvgRsFile <- paste(AvgRsFile,Ext,sep="")  
  
    TsFilePos <- paste(TsFilePos,counter,Ext,sep="")
  
    ModelFile <- paste(ModelFile,counter,"dat",sep="")
  
    ConTSCIndexFile <- paste(ConTSCIndexFile,counter,Ext,sep="")
    
  
 # Build model
    svm_model <- InvkSVM(TrFile1,VlFile,TrFile,kertype,ONE,TrVlTsRsFile,ThreshLim,counter,ModelFile)
        
 # Read positive (TSC) test data
    
    testpos <- read.matrix.csr(TsFilePos,fac=TRUE) 
    TsPosTarget <- testpos$y
    
 # Evaluate model: positive test data
    
    TsPosEmb <- testpos$x
      
    predpos <- predict(svm_model,TsPosEmb,sclae=FALSE,decision.values = TRUE)
    dpredscore <- attr(predpos, "decision.values")
  
 # Save scores in a file
    write.table(dpredscore,file = ConTSCIndexFile , sep = "\t",quote=FALSE,row.names = FALSE, col.names = FALSE)
 
  # Read negative (non-TSC) test data and Evaluate model 
    
    TsFileNeg <- paste(TsFileNeg,counter,sep="")
    TSCIndexFile <- paste(TSCIndexFile,counter,sep="")

    if (PARALLEL) {
      num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
      sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/Jie/RareDisease_TSC/03. Output/FV_2/1/logModel.txt")
      sfLibrary(snowfall)
    }
  
  
    sfExport("svm_model","PredSVM","TsFileNeg","TSCIndexFile","Ext")  
    sfExport("svm", namespace = "e1071")
    sfExport("predict.svm", namespace = "e1071")
    sfExport("read.matrix.csr", namespace = "e1071")
  
  # Number of files which contain negative test data
    NumSplit <- 8 
  
    upredscore <- sfClusterApplyLB(1:NumSplit,PredSVM)
  
    upredscore <- unlist(upredscore)
    sfStop()
  
    
    NumUnDiag <- length(upredscore)
    TsNegTarget <- rep(-1,NumUnDiag) 
 
    predscore <- c(dpredscore,upredscore)
 

 
    TsTarget  <- c(TsPosTarget,TsNegTarget)
 
 
    predobj <- prediction(predscore,TsTarget)
 
    ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
    
    DR <- sum(predscore > 0 & TsTarget == -1)
    TP <- sum(predscore > 0 & TsTarget == 1)
    
    cat(file=TrVlTsRsFile,append=TRUE, "Test ROCAUC: ", ROCAUC, " Test DR: ", DR, "Test TP ", TP, "\n")
    cat(" Test ROCAUC: ", ROCAUC, " Test DR: ", DR, "Test TP ", TP, "\n")  
    
    ResMat[counter,1] <- ROCAUC
    ResMat[counter,2] <- DR
    ResMat[counter,3] <- TP
    print(proc.time()- tm)
  
  }
  
  ResMat[,4] <- ResMat[,2]/NumUnDiag
  ResMat[,5] <- ResMat[,3]/length(TsPosTarget)
  
  AvgRs <- apply(ResMat[1:NumSimulation,],2,mean)
  ResMat[NumSimulation+1,] <- AvgRs
  
  colnames(ResMat) <- c("ROCAUC","D","TP", "DR", "TPR")

  write.table(ResMat,file = AvgRsFile)


