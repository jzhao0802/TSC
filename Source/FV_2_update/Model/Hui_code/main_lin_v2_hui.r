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

#recreate the output folder
path1 <- "D:/RareDisease_TSC/Results/FV_2/2n_lin_"
model_num <- 'model1'
pathOutput <- paste(path1, model_num, sep='')
dir.create(pathOutput)
unlink(pathOutput, recursive=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[1], 'lin', sep='/'), recursive=T, showWarnings=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[2], 'lin', sep='/'), recursive=T, showWarnings=T)
dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[3], 'lin', sep='/'), recursive=T, showWarnings=T)

#unlink(pathOutput, recursive=T)
#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[1], 'rbf', sep='/'), recursive=T, showWarnings=T)

#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[2], 'rbf', sep='/'), recursive=T, showWarnings=T)

#dir.create(path=paste(pathOutput, c('Indices', 'Model', 'Rs')[3], 'rbf', sep='/'), recursive=T, showWarnings=T)
#dir <- c('Indices', 'Model', 'Rs')
#lapply(X=1:3, function(X){file.remove(list.files(paste(pathOutput, dir[X], 'lin',sep='/'), all.files = TRUE, full.names = TRUE))})

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
  
  sfSource("D:/RareDisease_TSC/Source/FV_2_update/Model/Hui_code/InvkSVM_hui.R")
  sfSource("D:/RareDisease_TSC/Source/FV_2_update/Model/PredSVM.R")
  

  PARALLEL <- TRUE  # For parallel processing (TRUE)
  
  ONE <- FALSE       # For one-class classification (TRUE)
  
  NumSplit <- 8      # Number of splits for test data (for parallel processing)
   
  ThreshLim <- 50 # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)
    limitDr <- T
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
AvgRsFile <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/AvgResFile"
RsFileVl <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/ResVlFile"
RsFileTr <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/ResTrFile"
RsFileTrAllSet <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/ResTrSetAllFile"
RsFileTrVl <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/ResTrVlFile"
CoefFileTrVl <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/CoefTrVlFile"
CoefsFileTrVl <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/CoefsTrVlFile"
SVFileTrVl <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/SVTrVlFile"

Ext <- '.csv'
Ext1 <- '.txt'

AvgRsFile <- paste(AvgRsFile,Ext,sep="")
RsFileVl <- paste(RsFileVl, Ext, sep='')
RsFileTr <- paste(RsFileTr, Ext, sep='')
RsFileTrAllSet <- paste(RsFileTrAllSet, Ext, sep='')
RsFileTrVl <- paste(RsFileTrVl, Ext, sep='')
CoefFileTrVl <- paste(CoefFileTrVl, Ext, sep="")

timeStart<- proc.time()

num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/RareDisease_TSC/Results/FV_2/2n_lin/logModel_binary_lin_parOnSimAll.txt")
sfLibrary(snowfall)
sfExport('ResMat', 'PARALLEL', 'ONE','ThreshLim', 'kertype', 'InvkSVM','PredSVM', 'Ext', 'Ext1','limitDr')
    sfClusterEval(library("e1071"))
	sfClusterEval(library("ROCR"))
	sfClusterEval(library("pracma"))
	sfClusterEval(library("gtools"))
	sfClusterEval(library("MASS"))
TimeLog <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin"
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2_update:oneClass=', ONE, 'time used for sfExport: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

parOnSim <- function(counter) {
  TrFile1 <- "D:/RareDisease_TSC/Data/FV_2/2n/trainv"
  TrFile <- "D:/RareDisease_TSC/Data/FV_2/2n/train"
  VlFile <- "D:/RareDisease_TSC/Data/FV_2/2n/valid"
  TsFilePos <- "D:/RareDisease_TSC/Data/FV_2/2n/testpos"
  TsFileNeg <- "D:/RareDisease_TSC/Data/FV_2/2n/testneg"
  
  # Output file (Index and result files)  
  
  TSCIndexFile  <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Indices/lin/ProspectiveTSC"
  ConTSCIndexFile  <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Indices/lin/ConfirmedTSC"
  
  ModelFile <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Model/lin/SVModel"
  
  TrVlTsRsFile <- "D:/RareDisease_TSC/Results/FV_2/2n_lin/Rs/lin/TrVlTsResFile"
  
  
  
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
  resultInvkSVM <- InvkSVM(TrFile1,VlFile,TrFile,kertype,ONE,TrVlTsRsFile,ThreshLim,counter,ModelFile, limitDr)
  
  svm_model <- resultInvkSVM[[1]]
  scoreVl <-resultInvkSVM[[2]]
  aucVl <- resultInvkSVM[[3]]
  aucTr <- resultInvkSVM[[4]]
  aucTrAllSet <- resultInvkSVM[[5]]
  aucTrVl <- resultInvkSVM[[6]]
  #coefTrVl <- resultInvkSVM[[7]]
  #coefsTrVl <- resultInvkSVM[[8]]
  #SVTrVl <- resultInvkSVM[[9]]
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
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/RareDisease_TSC/Results/FV_2/2n_lin/logModel_binary_lin_parOnSim.txt")
    sfLibrary(snowfall)
  }
  
  
  sfExport("svm_model","PredSVM","TsFileNeg","TSCIndexFile","Ext", 'Ext1')  
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
  result <- list(ResMat=ResMat, NumDiag=length(TsPosTarget), NumUnDiag=NumUnDiag, scoreVl=scoreVl, predscoreTs=predscore, aucVl=aucVl, aucTr=aucTr, aucTrAllSet=aucTrAllSet, aucTrVl=aucTrVl)  #added by Jie to store the score of test data
  return(result)
  #print(proc.time()- tm)
  
}

  #----------------plot the recall-precision cerve for 10 simulations----------------#

result <- sfClusterApplyLB(1:NumSimulation,parOnSim)
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_lin_v2_update:oneClass=', ONE, 'time used for parallel on 10 simulations: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

sfStop()

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

plotf <- function(data){
	pdf(file=paste('D:/RareDisease_TSC/Results/FV_2/2n_lin/recall-precision-curve_simulation_', data, '.pdf', sep=''), width=11, height=20)
  par(mfrow=c(5,2))
  par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
  nonTSCNum <- length(result[[1]][[5]])-50
if(data=='Vl'){
	predScore <- predScoreVl
	nonTSCNum <- length(result[[1]][[4]])-50
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
add_avg <- function(data){
	Average <- apply(data, 2, mean)
	data1 <- rbind(data, Average)
	colnames(data1) <- c('Simulation',"ROCAUC","D","TP","PPV", "Best_C", "Best_Wt")
	return(data1)
}
aucVl <- add_avg(aucVl)
aucTr <- add_avg(aucTr)
aucTrVl <- add_avg(aucTrVl)
colnames(aucTrAllSet ) <-  c('Simulation',"ROCAUC","D","TP","PPV", "Best_C", "Best_Wt")
write.csv(aucVl,file = RsFileVl, row.names=F)
write.csv(aucTr,file = RsFileTr, row.names=F)
write.csv(aucTrAllSet,file = RsFileTrAllSet, row.names=F)
write.csv(aucTrVl,file = RsFileTrVl, row.names=F)
#write.csv(coefTrVl,file = CoefFileTrVl, row.names=F)
#write.csv(coefsTrVl,file = CoefsFileTrVl, row.names=F)
#write.csv(SVTrVl,file = SVFileTrVl, row.names=F)

#prepare average score for test data for John's convenience
write.csv(predScoreVl, paste(TimeLog, 'scoreVl.csv', sep='/'))
write.csv(predScore, paste(TimeLog, 'scoreTs.csv', sep='/'))
#score_cutoff <- runif(1000, min(scoreAvg), max(scoreAvg))

#average predScore for valid and test and store the recall and precision in each cutoff








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
scoreAvg <- predScore[, 1]
obs <- c(rep(1, 50), rep(-1, 70000))
cutoff.n <- length(levels(as.factor(scoreAvg)))
cutoff <- scoreAvg
TP <- unlist(lapply(X=1:70050, function(X){sum(scoreAvg[1:50] >= cutoff[X])}))
FP <- unlist(lapply(X=1:70050, function(X){sum(scoreAvg[-(1:50)] >= cutoff[X])}))

recall <- TP/50
precision <- FP/(TP+FP)

#points <-cbind(recall=order(recall), precision=precision[order(recall)])

######-------------------------report the recall-precision bucket----------------------------------###########
#pathOutput <- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin"
#infile <- 'setting1'
##pathInput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_lin_", infile, "_maxPPV/Rs/lin/")
#predScoreVl <- read.table(paste(pathInput, 'scoreTs.csv', sep=''), head=T, sep=',')[, -1]
#predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep=''), head=T, sep=',')[, -1]
data <- 'Vl'
infile <- 'setting1'
8
i <- 1
rec_prec_bucket <- function(data, n.bucket, trim, infile, kernel, consolid){
    path <- paste("D:/RareDisease_TSC/Results/FV_2/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, sep='')
    dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
    pathOutput <- paste("D:/RareDisease_TSC/Results/FV_2/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", data, sep='')
    
    setwd(pathOutput)
    pathInput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", sep='')
    predScoreVl <- read.table(paste(pathInput, 'scoreVl.csv', sep=''), head=T, sep=',')[, -1]
    predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep=''), head=T, sep=',')[, -1]
    if(data=='Vl'){
        score <- predScoreVl
        nonTSCNum <- 20000
        
    }else if(data=='Ts'){
        score <- predScoreTs
        nonTSCNum <- 70000
        
    }else{
        stop('\n the wrong input string!\n')
    }
    bucket_rbind <- numeric()
    for(i in 1:10){
        #predobj <- prediction(score[,i],c(rep(1, 50), rep(-1, nonTSCNum)))
        #add plot
        #perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
        #plot(perf, main=paste('simulation:', i, sep=''))
        #recall <- perf@x.values[[1]]
        #precision <- perf@y.values[[1]]
        n <- 50+nonTSCNum
        cutoff <- score[, i]
        TP <- unlist(lapply(X=1:n, function(X){sum(scoreAvg[1:50] >= cutoff[X])}))
        FP <- unlist(lapply(X=1:n, function(X){sum(scoreAvg[-(1:50)] >= cutoff[X])}))
        
        recall <- TP/50
        precision <- FP/(TP+FP)
        
        if(consolid==T){
            rec_prec <- cbind(indAllSim[, i], recall, precision)
            
        }else{
            rec_prec <- cbind(recall, precision)
            
        }
        if(trim==T){
            rec_prec <- rec_prec[1:100,]
            cat('\n based on the first 100 data!\n')
        }else{
            cat('\n based on the full data!\n')
        }
        bucket <- recPrecBucket(rec_prec, n.bucket, consolid)
        bucketi <- as.data.frame(cbind(Simulation=i, bucket))
        #bucket_rbind <- rbind(bucket_rbind, bucketi)
        write.table(bucketi, paste("Simu_", i,  '_consolid.txt',sep=''), row.names=F, quote=F)
    }
    #write.csv(bucket_rbind, paste(infile, '_', data, '.txt',sep=''), row.names=F, quote=F)
    
    return(bucket_rbind)
}
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting1')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting1')
bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=F, infile='setting1', kernel='lin', consolid=T)
bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=F, infile='setting1')
bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=F, infile='setting3')
bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=F, infile='setting3')


#consolide the test Indices through all the 10 simulation 8 split

pathInd <- "D:\\RareDisease_TSC\\Data\\Dict_Ind_2\\2n\\"
fileInd <- "TestIndices"
fileInd1 <- "TSCTestIndices"
Ext <- '.csv'
indAllSim <- numeric()
for(i in 1:10){
    indAllSplit <- numeric()
    ind1 <- read.table(paste(pathInd, fileInd1, i, Ext, sep=''))
    
    for(j in 1:8){
        
        ind <- read.table(paste(pathInd, fileInd, i,j, Ext, sep=''))
        indAllSplit <- rbind(indAllSplit, ind)
    }
    indAllSplitAddTsc <- c(ind1[, 1], indAllSplit[, 1])
    indAllSim <- c(indAllSim, indAllSplitAddTsc)
}

data <- 'Ts'
trim=F
infile='setting1'
kernel <- 'lin'
consolid <- T
