options(digits = 7)
library(base)
library(plyr)

# 1. Find distribution of scores for the nonTSC cases which the algorithm identifies as potentially TSC
# Input file
# 1). These files contain indices of Patient Ids (patid) and scores assigned to these patient by a SV machine, 
# for example, 2040   0.056.
input_path1 <- 'D:/RareDisease_TSC/Results/FV_1/2n/Indices/lin'
input_path2 <- 'D:/RareDisease_TSC/Results/FV_2/2n/Indices/lin'
input_data1 <- 'ProspectiveTSC'

# 2). Test indices
input_path11 <- 'D:/RareDisease_TSC/Data/Dict_Ind_1/2n'
input_path22 <- 'D:/RareDisease_TSC/Data/Dict_Ind_2/2n'
input_data2 <- 'TestIndices'
input_data3 <- 'ConfirmedTSC'
input_data4 <- 'TSCTestIndices'

Ext <- '.csv'

#output file 
output_path <- 'D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409'
setwd(output_path)
output_data1 <- 'Potential_TSC_Score'
output_data2 <- 'Confirmed_TSC_Score'
output_data3 <- 'Distribution_Score'

# constants
SimulationNumber <- 10
SplitNumber <- 8

# these segments don't have potential TSC patients
NonList1 <- c(38, 61:68, 71:78, 98, 101:103, 105:108)
NonList2 <- c(11:12, 14, 16,17,61:62, 64:68, 88 )

#for FV_1
sampleSet <- 2
i=1
j=1
UptInd.f <- function(sampleSet, thresh){
  ptIndScoreAll <- numeric()
  
  for (i in 1:SimulationNumber){
    for (j in 1:SplitNumber){
      s <- paste(i,j, sep='')
      if(sampleSet==1){
        NonList <- NonList1
        input_path_f1 <- input_path1
        input_path_f2 <- input_path11
      }else{
        NonList <- NonList2
        input_path_f1 <- input_path2
        input_path_f2 <- input_path22
        
      }
      if (!(s %in% as.character(NonList))){
        dt1 <- read.table(paste(input_path_f1, '/', input_data1, i, j, Ext, sep=''))
        row <- dt1[, 1]
        score <- dt1[, 2]
        dt2 <- read.table(paste(input_path_f2,'/', input_data2, i, j, Ext, sep=''))
        ptIndices <- dt2[row, 1]
        ptIndScore <- data.frame(ptIndices,score)
        ptIndScoreAll <- rbind(ptIndScoreAll, ptIndScore)
        
      }
    }
  }
  
  #delete the ptid using criteria (score < 0.2)
  ptIndScoreHigh <- ptIndScoreAll[ptIndScoreAll$score >= thresh, ]
  
  #de-dup the ptid and use the mean score as the unique pt score
  fc <- factor(ptIndScoreHigh$ptIndices)
  levels <- as.numeric(levels(fc))
  ptInScoreHighUniq <- aggregate(ptIndScoreHigh, list(UPtInd=fc), mean)
  ptInScoreHighUniq <- data.frame(Thresh=thresh, ptInScoreHighUniq[, -1])
  return(ptInScoreHighUniq)
}
result11 <- UptInd.f(1, 0)
result21 <- UptInd.f(2, 0)
result12 <- UptInd.f(1, 0.3)
result22 <- UptInd.f(2, 0.3)

result <- rbind(result11, result12, result21, result22)
result_thresh0 <- rbind(result11, result21)
pt_thresh0 <- result_thresh0[, 2]
pt_untarget <- c(16830360, 8276627, 17055240)
result
ptInScoreHighUniq_FV12 <- aggregate(result, list(UPtInd=as.factor(result[,1])), mean)[, c(1,4)]


#match the 3 untargeted ptid in svm output files
numTrial <- 1

findIdx <- function(numTrial){
#	result_findIdx <- list()
	if(numTrial==1){
		input_path <- input_path11
	}else if(numTrial==2){
		input_path <- input_path22
	}
	files <- dir(input_path) #FV_2
	ptIndFile <- grep('^TestIndices', files, value=T)
	ptIndFile <- c(ptIndFile[-grep('\\w+\\d{3}\\.', ptIndFile, value=F)], grep('\\w+\\d{3}\\.', ptIndFile, value=T))
	fileNum <- gsub('(\\D+)(\\d+)(\\.\\D+)', '\\2', ptIndFile, perl=T)
	ptIndPathFile <- paste(input_path, ptIndFile, sep='/')
	datalist <- lapply(ptIndPathFile, function(name){read.table(name)})
	dimlist <- lapply(ptIndPathFile, function(name){dim(read.table(name))[1]})
	ptNumAll <- sum(unlist(dimlist)[1:8])
	ptNumSplit <- unlist(dimlist)[1]
	pt_all <- unlist(datalist)
	row <- match( pt_all, pt_untarget)
	tarRvtInd <- which(!is.na(match(pt_all, pt_untarget)))
	tarPtInd <- pt_all[tarRvtInd]
	tarFileNum <- fileNum[tarRvtInd %/% ptNumSplit+1]

	tarIndDF <-  data.frame(FV=numTrial, ptNumAll=ptNumAll, ptNumSplit=ptNumSplit, PtInd=tarPtInd, RvtInd=tarRvtInd, FileNum=tarFileNum)
	return(tarIndDF)
}
result1 <- findIdx(1)
result2 <- findIdx(2)
tarIndDF <- rbind(result1, result2)
modelIdUniqe <- levels(as.factor(paste(tarIndDF[, 1], '_', tarIndDF[, 6], sep='')))
#find the score
library("snowfall")
sfLibrary("snow",character.only = TRUE)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("MASS", character.only = TRUE)
sfLibrary("e1071", character.only = TRUE)
sfLibrary("ROCR",character.only = TRUE)
sfLibrary("pracma",character.only = TRUE)
sfLibrary("gtools",character.only = TRUE)
sfClusterEval(library(e1071))
sfClusterEval(library(snowfall))
sfClusterEval(library(MASS))
sfClusterEval(library(ROCR))
sfClusterEval(library(pracma))
sfClusterEval(library(gtools))
# Source files

sfSource("D:/jzhao/RareDisease_TSC/02. Code/Model/InvkSVM_forTraceScore.R")
sfSource("D:/jzhao/RareDisease_TSC/02. Code/Model/PredSVM_forTraceScore.R")


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
num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409/logModel_traceScore.txt")
sfLibrary(snowfall)
sfExport('ONE','ThreshLim', 'kertype', 'tarIndDF', 'modelIdUniqe', 'InvkSVM','PredSVM')
    sfClusterEval(library("e1071"))
	sfClusterEval(library("ROCR"))
	sfClusterEval(library("pracma"))
	sfClusterEval(library("gtools"))
	sfClusterEval(library("MASS"))


	
#idxScore <- matrix(nr=length(tarFileNum), nc=3)
i <- modelIdUniqe[length(modelIdUniqe)-1]
i <- modelIdUniqe[which(modelIdUniqe=='1_13')]
traceScore <- function(i){
	FVi <- as.numeric(gsub('(\\d)(\\w)(\\d+)(\\d)', '\\1', i, perl=T))
  counter <- as.numeric(gsub('(\\d)(_)(\\d+)(\\d)', '\\3', i, perl=T))
  NumSplit <- as.numeric(gsub('(\\d)(\\w)(\\d+)(\\d)', '\\4', i, perl=T))
  DFi <- tarIndDF[tarIndDF[, 6]==paste(counter, NumSplit, sep='') & tarIndDF[, 1]==FVi,]
  ptInd <- numeric()
  row <- numeric()
  ptNumAll <- tarIndDF[tarIndDF[, 1]==FVi & tarIndDF[, 6]==paste(counter, NumSplit, sep=''), 2]
	ptNumSplit <- tarIndDF[tarIndDF[, 1]==FVi & tarIndDF[, 6]==paste(counter, NumSplit, sep=''), 3]
  for(j in 1:nrow(DFi)){
		 rowi <- DFi[j,5]-(ptNumAll*(counter-1)+ptNumSplit*(NumSplit-1))
		 row <- rbind(row, rowi)
		ptIndi <- DFi[j,4]
		ptInd <- rbind(ptInd, ptIndi)

  }
  #read in ptNUm and tarIndDF
  # Input files (Training set, validation set and test set)
  Dir <- 'D:/RareDisease_TSC/Data/'
  TrFile1 <- paste(Dir, 'FV_', FVi, '/2n/trainv', sep='')
  TrFile <- paste(Dir, 'FV_', FVi, '/2n/train', sep='')
  VlFile <- paste(Dir, 'FV_', FVi, '/2n/valid', sep='')
  TsFilePos <- paste(Dir, 'FV_', FVi, '/2n/testpos', sep='')
  TsFileNeg <- paste(Dir, 'FV_', FVi, '/2n/testneg', sep='')  
 # Output file (Index and result files)  
  
  TSCIndexFile  <- "D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409/2n/Indices/lin/ProspectiveTSC"
  ConTSCIndexFile  <- "D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409/2n/Indices/lin/ConfirmedTSC"
  
  ModelFile <- "D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409/2n/Model/lin/SVModel"
  
  TrVlTsRsFile <- "D:/jzhao/RareDisease_TSC/03. Output/TraceScore/0409/2n/Rs/lin/TrVlTsResFile"

 AvgRsFile <- "D:/Jie/RareDisease_TSC/03. Output/TraceScore/0409/2n/Rs/lin/AvgResFile"
 
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
  uPredScore <- PredSVM(NumSplit, TsFileNeg, TSCIndexFile, Ext, svm_model)
  idxScorei <- c(Index=ptInd, Row=row, Score=uPredScore[row])
  #idxScore[i,] <- idxScorei
  return(idxScorei)
}
tarScore <- sfClusterApplyLB(modelIdUniqe, traceScore)
sfStop()
