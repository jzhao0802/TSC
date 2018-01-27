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
# Author : Huma Lodhi                                                                            #                                                                    #
# Date :  2014 --                                                                                #
#                                                                                                #
# Modified By Jie Zhao                                                                           #
# Date :  2015.4 --                                                                              #
##################################################################################################
rm(list=ls())
#recreate the output folder
path1 <- "D:/RareDisease_TSC/Results/FV_2/2n_lin_"
model_num <- 'model9'
pathOutput <- paste(path1, model_num, sep='')
dir.create(pathOutput)
unlink(pathOutput, recursive=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[1], 'lin', sep='/'), recursive=T, showWarnings=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[2], 'lin', sep='/'), recursive=T, showWarnings=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[3], 'lin', sep='/'), recursive=T, showWarnings=T)

 # Load required packages 
    #rm(list=ls())
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
  
  sfSource("D:/RareDisease_TSC/Source/Model/FV_2/InvkSVM_lin_model9.R")
  sfSource("D:/RareDisease_TSC/Source/Model/FV_2/PredSVM.R")
  

  PARALLEL <- TRUE  # For parallel processing (TRUE)
  
  ONE <- FALSE       # For one-class classification (TRUE)
  
  NumSplit <- 8      # Number of splits for test data (for parallel processing)
   
  ThreshLim <- 50 # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)
  limitDr <- F

  NumSimulation <- 10 # Number of simulationsline

  NumMsr <- 6

  ResMat <- matrix(nrow=NumSimulation+1,ncol=NumMsr,byrow=TRUE)

################################################################################################## 

 # Model Building and Identificaton of TSC

  kertype <- "linear"  # Kernel function to be used in conjunction with SVMs

  #kertype <- "radial"  
  
  #kertype <- "polynomial" # Dont use this kernel for identification of undiagnosed TSC patients
  # Input files (Training set, validation set and test set)
AvgRsFile <- paste(pathOutput,"/Rs/lin/AvgResFile",sep='')
RsFileVl <- paste(pathOutput,"/Rs/lin/ResVlFile",sep='')
RsFileTr <- paste(pathOutput,"/Rs/lin/ResTrFile",sep='')
RsFileTrAllSet <- paste(pathOutput,"/Rs/lin/ResTrSetAllFile",sep='')
RsFileTrVl <- paste(pathOutput,"/Rs/lin/ResTrVlFile",sep='')
CoefFileTrVl <- paste(pathOutput,"/Rs/lin/CoefTrVlFile",sep='')
CoefsFileTrVl <- paste(pathOutput,"/Rs/lin/CoefsTrVlFile",sep='')
SVFileTrVl <- paste(pathOutput,"/Rs/lin/SVTrVlFile",sep='')

Ext <- '.csv'
Ext1 <- '.txt'

AvgRsFile <- paste(AvgRsFile,Ext,sep="")
RsFileVl <- paste(RsFileVl, Ext, sep='')
RsFileTr <- paste(RsFileTr, Ext, sep='')
RsFileTrAllSet <- paste(RsFileTrAllSet, Ext, sep='')
RsFileTrVl <- paste(RsFileTrVl, Ext, sep='')
CoefFileTrVl <- paste(CoefFileTrVl, Ext, sep="")
???

ResMat1 <- matrix(nr=11, nc=6)
predScore <- matrix(nr=length(result[[1]][[5]]), nc=10)
predScoreVl <- matrix(nr=length(result[[1]][[4]]), nc=10)
aucVl <- matrix(nr=10, nc=length(result[[1]][[6]]))
aucTr <- matrix(nr=10, nc=length(result[[1]][[6]]))
aucTrAllSet <- matrix(nr=60, nc=length(result[[1]][[6]]))
aucTrVl <-  matrix(nr=10, nc=length(result[[1]][[6]]))
coefTrVl <- numeric()
coefsTrVl <- numeric()
SVTrVl <- numeric()
for (i in 1: NumSimulation){
  ResMat1[i, ] <- result[[i]][[1]][i,]
  
  predScoreVl[, i] <- result[[i]][[4]]
  predScore[, i] <- result[[i]][[5]]
  aucVl[i,] <- result[[i]][[6]]
  aucTr[i,] <- result[[i]][[7]]
  aucTrAllSet[(6*i-5):(6*i),] <- result[[i]][[8]]
  aucTrVl[i,] <- result[[i]][[9]]
  #coefTrVl <- rbind(coefTrVl,cbind(i, result[[i]][[10]]))
  #coefsTrVl <- rbind(coefsTrVl,cbind(i, result[[i]][[11]]))
  #SVTrVl <- rbind(SVTrVl,cbind(i, result[[i]][[12]]))

}
NumUnDiag <- result[[1]][[3]]
NumDiag <- result[[1]][[2]]
  ResMat1[,4] <- ResMat1[,2]/NumUnDiag
  ResMat1[,5] <- ResMat1[,3]/NumDiag
   ResMat1[,6] <- ResMat1[,3]/(ResMat1[, 2]+ResMat1[, 3])

  AvgRs <- apply(ResMat1[1:NumSimulation,],2,mean)
  ResMat1[NumSimulation+1,] <- AvgRs
  
  colnames(ResMat1) <- c("ROCAUC","D","TP", "DR", "TPR", "PPV")

  write.csv(ResMat1,file = AvgRsFile, row.names=T)
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2_update:oneClass=', ONE, 'time used for whole model: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')


#prepare average score for test data for John's convenience
write.csv(predScoreVl, paste(TimeLog, 'scoreVl.csv', sep='/'))
write.csv(predScore, paste(TimeLog, 'scoreTs.csv', sep='/'))
#score_cutoff <- runif(1000, min(scoreAvg), max(scoreAvg))

#average predScore for valid and test and store the recall and precision in each cutoff





