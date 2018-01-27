rm(list=ls())
options(digits = 7)
library(base)
library(plyr)
library(dplyr)

# 1. Find distribution of scores for the nonTSC cases which the algorithm identifies as potentially TSC
# Input file
# 1). These files contain indices of Patient Ids (patid) and scores assigned to these patient by a SV machine, 
# for example, 2040   0.056.
re_path <- 'D:\\RareDisease_TSC\\Results\\FV_4'
input_path1 <- paste(re_path, '\\2n_lin_setting1_maxPPV_Yan2\\Indices\\lin', sep='')
input_data11 <- 'ProspectiveTSC'

# 2). Test indices
ind_path <- 'D:\\RareDisease_TSC\\Data\\Dict_Ind_4'
input_path2 <- paste(ind_path,"\\2n", sep='')
input_data21 <- 'TestIndices'

Ext <- '.csv'

#output file 
output_path <- 'D:\\RareDisease_TSC\\Results\\FV_4\\2n_lin_setting1_maxPPV_Yan2'
if(!file.exists(output_path)){
    dir.create(output_path, recursive=T, showWarnings=T)
    setwd(output_path)
}
setwd(output_path)
output_data1 <- 'Potential_TSC_Score'
output_data2 <- 'Confirmed_TSC_Score'
output_data3 <- 'Distribution_Score'

# constants
SimulationNumber <- 10
SplitNumber <- 8

# these segments don't have potential TSC patients
#NonList1 <- c(38, 61:68, 71:78, 98, 101:103, 105:108)
#NonList2 <- c(11:12, 14, 16,17,61:62, 64:68, 88 )

#for FV_1
i=1
j=1
UptInd.f <- function(thresh){
    ProspectiveScore_Flist <- grep("^Pros", list.files(path=input_path1, all.files=F, full.names=F, recursive=F), value=T)
    ptIndScoreAll <- numeric()
    for (fi in ProspectiveScore_Flist){
        simNum <- gsub("(^\\D+)(\\d+)(\\d{1})(\\W{1}\\w+$)", "\\2", fi, perl=T)
        splitNum <- gsub("(^\\D+)(\\d+)(\\d{1})(\\W{1}\\w+$)", "\\3", fi, perl=T)
        dt1 <- read.table(paste(input_path1, '\\', fi, sep=''))
        row <- dt1[, 1]
        score <- dt1[, 2]
        dt2 <- read.table(paste(input_path2,'\\', input_data21, simNum, splitNum, Ext, sep=''))
        ptIndices <- dt2[row, 1]
        ptIndScore <- data.frame(ptIndices,score)
        ptIndScoreAll <- rbind(ptIndScoreAll, ptIndScore)
        
        
    }
    
  
  #delete the ptid using criteria (score < 0.2)
  ptIndScoreHigh <- ptIndScoreAll[ptIndScoreAll$score >= thresh, ]
  
  #de-dup the ptid and use the mean score as the unique pt score
  fc <- factor(ptIndScoreHigh$ptIndices)
  #levels <- as.numeric(levels(fc))
  ptInScoreHighUniq <- aggregate(ptIndScoreHigh, list(UPtInd=fc), mean)
  ptInScoreHighUniq <- data.frame(Thresh=thresh, ptInScoreHighUniq[, -1])
  return(ptInScoreHighUniq)
}
score1 <- UptInd.f(0.3)
indScoreForProspective <- score1[, -1]
write.csv(indScoreForProspective, 'indScoreForProspective_yan2.csv', row.names=F)


#get top20 score
top20_temp <- indScoreForProspective[order(indScoreForProspective[, 2], decreasing=T)[1:20],]
huma_v1 <- read.table('D:\\RareDisease_TSC\\Results\\pt_list_v1.csv', header=F)
huma_v2 <- read.table('D:\\RareDisease_TSC\\Results\\pt_list_v2.csv', header=F)
yan_v3 <- read.table('D:\\RareDisease_TSC\\Results\\pt_list_v3.csv', header=F)

pat_list_pre <- unique(rbind(huma_v1, huma_v2, yan_v3))
pat_list_cur <- data.frame(top20_temp[,1])
colnames(pat_list_cur) <- 'V1'

dup_pt <- inner_join(pat_list_pre,pat_list_cur, 'V1')
num_dup <- nrow(dup_pt)
#QC
pat_list_pre <- unique(c(huma_v1[, 1], huma_v2[, 1], yan_v3[, 1]))
indScoreForProspective_filter <- indScoreForProspective[!(indScoreForProspective[, 1] %in% pat_list_pre), ]  #5 exists in pre
#get top 20 and top 10
indScoreForProspective_filter_top20 <- indScoreForProspective_filter[order(indScoreForProspective_filter[, 2], decreasing =T)[1:20], ]
indScoreForProspective_filter_top10 <- indScoreForProspective_filter[order(indScoreForProspective_filter[, 2], decreasing =T)[1:10], ]
write.csv(indScoreForProspective_filter_top20, 'indScoreForProspective_filter_top20_yan2.csv', row.names=F)
write.csv(indScoreForProspective_filter_top10, 'indScoreForProspective_filter_top10_yan2.csv', row.names=F)
#QC
match(indScoreForProspective_filter_top20[, 1],pat_list_pre )


top20_temp2 <- indScoreForProspective[order(indScoreForProspective[, 2], decreasing=T)[1:(20+num_dup)],]

top20 <- top20_temp2[ !(top20_temp2$ptIndices %in% dup_pt$V1),]

#get top20 score
top10 <- top20[order(top20[, 2], decreasing=T)[1:10],]





result <- rbind(result11, result12, result21, result22)
result_thresh0 <- rbind(result11, result21)
pt_thresh0 <- result_thresh0[, 2]
pt_untarget <- c(16830360, 8276627, 17055240)
result
ptInScoreHighUniq_FV12 <- aggregate(result, list(UPtInd=as.factor(result[,1])), mean)[, c(1,4)]


#match the 3 untargeted ptid in svm output files
numTrial <- 2

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
  uPredScore <- PredSVM(NumSplit, TsFileNeg, TSCIndexFile, Ext)
  idxScorei <- c(Index=ptInd, Row=row, Score=uPredScore[row])
  #idxScore[i,] <- idxScorei
  return(idxScorei)
}
tarScore <- sfClusterApplyLB(modelIdUniqe, traceScore)

