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

#recreate the output folder
pathOutput <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin"
#unlink(pathOutput, recursive=T)
#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[1], 'rbf', sep='/'), recursive=T, showWarnings=T)

#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[2], 'rbf', sep='/'), recursive=T, showWarnings=T)

#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[3], 'rbf', sep='/'), recursive=T, showWarnings=T)
dir <- c('Indices', 'Model', 'Rs')
lapply(X=1:3, function(X){file.remove(list.files(paste(pathOutput, dir[X], 'lin',sep='/'), all.files = TRUE, full.names = TRUE))})

 # Load required packages 
    rm(list=ls())
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
  
  sfSource("D:/RareDisease_TSC/Source/FV_2_update/Model/InvkSVM_lin.R")
  sfSource("D:/RareDisease_TSC/Source/FV_2_update/Model/PredSVM.R")
  

  PARALLEL <- TRUE  # For parallel processing (TRUE)
  
  ONE <- FALSE       # For one-class classification (TRUE)
  
  NumSplit <- 8      # Number of splits for test data (for parallel processing)
   
  ThreshLim <- 50 # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)
  #ThreshLim_ppv <- 0.5

  #ThreshLim <- 150 # Uncomment this line for one-class classification using RBF kernels (when ONE is set to TRUE and kertype is set to radial, set ThresLim to 150)

  #ThreshLim <- 700 # Uncomment this line for one-class classification using linear kernels (when ONE is set to TRUE and kertype is set to linear, set ThresLim to 150)
  
  NumSimulation <- 10 # Number of simulationsline

  NumMsr <- 6

  ResMat <- matrix(nrow=NumSimulation+1,ncol=NumMsr,byrow=TRUE)

################################################################################################## 

 # Model Building and Identificaton of TSC

  kertype <- "linear"  # Kernel function to be used in conjunction with SVMs

  #kertype <- "radial"  
  
  #kertype <- "polynomial" # Dont use this kernel for identification of undiagnosed TSC patients
  # Input files (Training set, validation set and test set)
AvgRsFile <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin/AvgResFile"
RsFileVl <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin/ResVlFile"
Ext <- '.csv'
AvgRsFile <- paste(AvgRsFile,Ext,sep="")
RsFileVl <- paste(RsFileVl, Ext, sep='')
timeStart<- proc.time()

num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/logModel_binary_lin_parOnSimAll.txt")
sfLibrary(snowfall)
sfExport('ResMat', 'PARALLEL', 'ONE','ThreshLim', 'kertype', 'InvkSVM','PredSVM', 'Ext')
    sfClusterEval(library("e1071"))
	sfClusterEval(library("ROCR"))
	sfClusterEval(library("pracma"))
	sfClusterEval(library("gtools"))
	sfClusterEval(library("MASS"))
TimeLog <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin"
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2_update:oneClass=', ONE, 'time used for sfExport: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

parOnSim <- function(counter) {
  TrFile1 <- "D:/RareDisease_TSC/Data/FV_2_update/2n/trainv"
  TrFile <- "D:/RareDisease_TSC/Data/FV_2_update/2n/train"
  VlFile <- "D:/RareDisease_TSC/Data/FV_2_update/2n/valid"
  TsFilePos <- "D:/RareDisease_TSC/Data/FV_2_update/2n/testpos"
  TsFileNeg <- "D:/RareDisease_TSC/Data/FV_2_update/2n/testneg"
  
  # Output file (Index and result files)  
  
  TSCIndexFile  <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Indices/lin/ProspectiveTSC"
  ConTSCIndexFile  <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Indices/lin/ConfirmedTSC"
  
  ModelFile <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Model/lin/SVModel"
  
  TrVlTsRsFile <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/Rs/lin/TrVlTsResFile"
  
  
  
  TrFile1 <- paste(TrFile1,counter,Ext,sep="")
  TrFile <- paste(TrFile,counter,Ext,sep="")
  VlFile <- paste(VlFile,counter,Ext,sep="")
  TrVlTsRsFile <- paste(TrVlTsRsFile,counter,Ext,sep="")
  #AvgRsFile <- paste(AvgRsFile,Ext,sep="")  
  
  TsFilePos <- paste(TsFilePos,counter,Ext,sep="")
  
  ModelFile <- paste(ModelFile,counter,"dat",sep="")
  
  ConTSCIndexFile <- paste(ConTSCIndexFile,counter,Ext,sep="")
  
  
  # Build model
  #pdf(file=paste('D:/RareDisease_TSC/Results/FV_2_update/2n_lin/recall-precision-curve', '.pdf', sep=''), width=11, height=20)
  #par(mfrow=c(5,2))
  #par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
  resultInvkSVM <- InvkSVM(TrFile1,VlFile,TrFile,kertype,ONE,TrVlTsRsFile,ThreshLim,counter,ModelFile)
  
  svm_model <- resultInvkSVM[[1]]
  scoreVl <-resultInvkSVM[[2]]
  aucVl <- resultInvkSVM[[3]]
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
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/RareDisease_TSC/Results/FV_2_update/2n_lin/logModel_binary_lin_parOnSim.txt")
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
  result <- list(ResMat=ResMat, NumDiag=length(TsPosTarget), NumUnDiag=NumUnDiag, scoreVl=scoreVl, predscoreTs=predscore, aucVl=aucVl)  #added by Jie to store the score of test data
  return(result)
  #print(proc.time()- tm)
  
}

  #----------------plot the recall-precision cerve for 10 simulations----------------#

result <- sfClusterApplyLB(1:NumSimulation,parOnSim)
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2_update:oneClass=', ONE, 'time used for parallel on 10 simulations: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

sfStop()

ResMat1 <- matrix(nr=11, nc=6)
predScore <- matrix(nr=70050, nc=10)
predScoreVl <- matrix(nr=20050, nc=10)
aucVl <- matrix(nr=10, nc=length(result[[1]][[6]]))
for (i in 1: NumSimulation){
  ResMat1[i, ] <- result[[i]][[1]][i,]
  
  predScoreVl[, i] <- result[[i]][[4]]
  predScore[, i] <- result[[i]][[5]]
  aucVl[i,] <- result[[i]][[6]]
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

plotf <- function(data){
	pdf(file=paste('D:/RareDisease_TSC/Results/FV_2_update/2n_lin/recall-precision-curve_simulation_', data, '.pdf', sep=''), width=11, height=20)
  par(mfrow=c(5,2))
  par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
  nonTSCNum <- 70000
if(data=='Vl'){
	predScore <- predScoreVl
	nonTSCNum <- 20000
}else if(data=="Ts"){
	cat('\n ts plot beginning\n')
}else{
	stop('\n the wrong parameter input!\n')
}
for(i in 1:10){
	 predobj <- prediction(predScore[,i],c(rep(1, 50), rep(-1, nonTSCNum)))
		#add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
	plot(perf, main=paste('simulation:', i, sep=''))
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
}
dev.off()

}

plotf('Vl')
plotf('Ts')

#library('plyr')
#aucVl<-ldply(result[[]], quickdf)
colnames(aucVl) <- c('Simulation',"ROCAUC","D","TP","PPV", "Best_C", "Best_Wt")

write.csv(aucVl,file = RsFileVl, row.names=T)

#prepare average score for test data for John's convenience
write.csv(predScoreVl, paste(TimeLog, 'scoreVl.csv', sep='/'))
write.csv(predScore, paste(TimeLog, 'scoreTs.csv', sep='/'))
#score_cutoff <- runif(1000, min(scoreAvg), max(scoreAvg))

#average predScore for valid and test and store the recall and precision in each cutoff
recPrecBucket <- function(data, n.buck){
    obsPred <- data
    recall <- obsPred[,1]
    predCut <- cut2(recall, g=n.buck)
    
    num_pos <- aggregate(obsPred[,], by=list(predCut), mean)[, 2]
    num_buck <- table(predCut)
    #num_neg <- aggregate(1-obsPred[, 1], by=list(predCut), sum)[, 2]
    #perc_pos <- num_pos/num_buck
    result <- data.frame(cbind(Bucket_number=seq(1, n.buck),  Perc_with_positive_outcomes=perc_pos, Actual_number_of_positive_outcomes=num_pos,Actual_number_of_negative_outcomes=num_neg, total_num=num_buck ))
    write.xlsx(result, file=paste('PostiveDistributeForBucketModel.xlsx', sep=''), sheetName=paste('bucket_', n.buck, sep=''), row.names=F, append=T, showNA=T)
    
}

rec_prec_avg <- function(data){
    if(data=='Vl'){
        predScore <- predScoreVl
        nonTSCNum <- 20000
    }else if(data=='Ts'){
        
        cat('\ntest begin!\n')
        predScore <- predScore
        
        nonTSCNum <- 70000
        
    }else{
        stop('\nthe wrong input data!\n')
    }
    predScoreAvg <- apply(predScore, 1, mean)
    predobj <- prediction(predScoreAvg,c(rep(1, 50), rep(-1, nonTSCNum)))
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    rec_prec <- cbind(recall, precision)
    write.csv(rec_prec, paste(TimeLog, '/rec_prec_setting1', data,'.csv', sep=''))
    return(rec_prec)
    
}
rec_prec <- rec_prec_avg('Vl')

rec_prec <- rec_prec_avg('Ts')





#dim(predScore)
#scoreAvg <- apply(predScore, 1, mean)
#obs <- c(rep(1, 50), rep(-1, 70000))
#cutoff.n <- length(levels(as.factor(scoreAvg)))
#cutoff <- scoreAvg
#TP <- unlist(lapply(X=1:70050, function(X){sum(scoreAvg[1:50] >= cutoff[X])}))
#FP <- unlist(lapply(X=1:70050, function(X){sum(scoreAvg[-(1:50)] >= cutoff[X])}))

#recall <- TP/50
#precision <- FP/(TP+FP)

#points <-cbind(recall=order(recall), precision=precision[order(recall)])
rec_prec <- function(data){
    if(data=='Vl'){
        nonTSCNum <- 20000
    }else if(data=='Ts'){
        
        cat('\ntest begin!\n')
        
        nonTSCNum <- 70000
        
    }else{
        stop('\nthe wrong input data!\n')
    }
    score <- read.table(paste(TimeLog, '/score', data,'.csv', sep=''), header=T, sep=',') [, -1]
    predScoreAvg <- apply(score, 1, mean)
    predobj <- prediction(predScoreAvg,c(rep(1, 50), rep(-1, nonTSCNum)))
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    rec_prec <- cbind(recall, precision)
    write.csv(rec_prec, paste(TimeLog, '/rec_prec_setting2', data,'.csv', sep=''))
    return(rec_prec)
    
}
#data <- 'Vl'
##rec_prec1 <- rec_prec('Vl')
#rec_prec2 <- rec_prec('Ts')