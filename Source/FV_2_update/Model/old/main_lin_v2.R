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
  sfClusterEval(library(e1071))

 # Source files
  
  sfSource("D:/jzhao/RareDisease_TSC/02. Code/Model/InvkSVM.R")
  sfSource("D:/jzhao/RareDisease_TSC/02. Code/Model/PredSVM.R")
  

  PARALLEL <- TRUE  # For parallel processing (TRUE)
  
  ONE <- FALSE       # For one-class classification (TRUE)
  
  NumSplit <- 8      # Number of splits for test data (for parallel processing)
    
  ThreshLim <- 50 # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)

  #ThreshLim <- 150 # Uncomment this line for one-class classification using RBF kernels (when ONE is set to TRUE and kertype is set to radial, set ThresLim to 150)

  #ThreshLim <- 700 # Uncomment this line for one-class classification using linear kernels (when ONE is set to TRUE and kertype is set to linear, set ThresLim to 150)
  
  NumSimulation <- 10 # Number of simulationsline

  NumMsr <- 5

  ResMat <- matrix(nrow=NumSimulation+1,ncol=NumMsr,byrow=TRUE)

################################################################################################## 

 # Model Building and Identificaton of TSC

  kertype <- "linear"  # Kernel function to be used in conjunction with SVMs

  #kertype <- "radial"  
  
  #kertype <- "polynomial" # Dont use this kernel for identification of undiagnosed TSC patients
  # Input files (Training set, validation set and test set)
AvgRsFile <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Rs/lin/AvgResFile"
Ext <- '.csv'
AvgRsFile <- paste(AvgRsFile,Ext,sep="")
timeStart<- proc.time()

num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/jzhao/RareDisease_TSC/03. Output/FV_2/logModel_binary_rbf_parOnSimAll.txt")
sfLibrary(snowfall)
sfExport('ResMat', 'PARALLEL', 'ONE','ThreshLim', 'kertype', 'InvkSVM','PredSVM')
    sfClusterEval(library("e1071"))
	sfClusterEval(library("ROCR"))
	sfClusterEval(library("pracma"))
	sfClusterEval(library("gtools"))
	sfClusterEval(library("MASS"))
TimeLog <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Rs/lin"
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2:oneClass=', ONE, 'time used for sfExport: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

parOnSim <- function(counter) {
  TrFile1 <- "D:/RareDisease_TSC/Data/FV_2/2n/trainv"
  TrFile <- "D:/RareDisease_TSC/Data/FV_2/2n/train"
  VlFile <- "D:/RareDisease_TSC/Data/FV_2/2n/valid"
  TsFilePos <- "D:/RareDisease_TSC/Data/FV_2/2n/testpos"
  TsFileNeg <- "D:/RareDisease_TSC/Data/FV_2/2n/testneg"
  
  # Output file (Index and result files)  
  
  TSCIndexFile  <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Indices/lin/ProspectiveTSC"
  ConTSCIndexFile  <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Indices/lin/ConfirmedTSC"
  
  ModelFile <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Model/lin/SVModel"
  
  TrVlTsRsFile <- "D:/jzhao/RareDisease_TSC/03. Output/FV_2/2n/Rs/lin/TrVlTsResFile"
  
  
  Ext <- ".csv"
  
  
  TrFile1 <- paste(TrFile1,counter,Ext,sep="")
  TrFile <- paste(TrFile,counter,Ext,sep="")
  VlFile <- paste(VlFile,counter,Ext,sep="")
  TrVlTsRsFile <- paste(TrVlTsRsFile,counter,Ext,sep="")
  #AvgRsFile <- paste(AvgRsFile,Ext,sep="")  
  
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
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/jzhao/RareDisease_TSC/03. Output/FV_2/logModel_binary_rbf_parOnSim.txt")
    sfLibrary(snowfall)
  }
  
  
  sfExport("svm_model","PredSVM","TsFileNeg","TSCIndexFile","Ext")  
  sfExport("svm", namespace = "e1071")
  sfExport("predict.svm", namespace = "e1071")
  sfExport("read.matrix.csr", namespace = "e1071")
  sfClusterEval(library("e1071"))
  # Number of files which contain negative test data
  NumSplit <- 8 
  
  upredscore <- sfClusterApplyLB(1:NumSplit,PredSVM)
  #  upredscore <- PredSVM(svm_model, TsFileNeg, TSCIndexFile, Ext)
  upredscore <- unlist(upredscore)
  sfStop()
  
  
  NumUnDiag <- length(upredscore)
  TsNegTarget <- rep(-1,NumUnDiag) 
  
  predscore <- c(dpredscore,upredscore)
  
  
  
  TsTarget  <- c(TsPosTarget,TsNegTarget)
  
  
  predobj <- prediction(predscore,TsTarget)
  
  ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
  
  DR <- sum(predscore > 0 & TsTarget == -1) # False Positive
  TP <- sum(predscore > 0 & TsTarget == 1) # True Positive
  
  cat(file=TrVlTsRsFile,append=TRUE, "Test ROCAUC: ", ROCAUC, " Test DR: ", DR, "Test TP ", TP, "\n")
  cat(" Test ROCAUC: ", ROCAUC, " Test DR: ", DR, "Test TP ", TP, "\n")  
  
  ResMat[counter,1] <- ROCAUC
  ResMat[counter,2] <- DR
  ResMat[counter,3] <- TP
  return(ResMat)
  #print(proc.time()- tm)
  
}

    ResMat <- sfClusterApplyLB(1:NumSimulation,parOnSim)
library('plyr')
ldply(ResMat, quickdf)
ResMat1 <- matrix(nr=11, nc=5)
for (i in 1: NumSimulation){
  ResMat1[i, ] <- ResMat[[i]][i,]
}
NumUnDiag <- 70000
NumDiag <- 50
  ResMat1[,4] <- ResMat1[,2]/NumUnDiag
  ResMat1[,5] <- ResMat1[,3]/NumDiag
  
  AvgRs <- apply(ResMat1[1:NumSimulation,],2,mean)
  ResMat1[NumSimulation+1,] <- AvgRs
  
  colnames(ResMat1) <- c("ROCAUC","D","TP", "DR", "TPR")

  write.table(ResMat1,file = AvgRsFile)
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2:oneClass=', ONE, 'time used for sfExport: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')


