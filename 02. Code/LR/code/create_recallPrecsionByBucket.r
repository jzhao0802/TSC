#create recall-precision curve data for logistic regression from Yan
library("snowfall")
sfLibrary("snow",character.only = TRUE)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("MASS", character.only = TRUE)
sfLibrary("e1071", character.only = TRUE)
sfLibrary("ROCR",character.only = TRUE)
sfLibrary("pracma",character.only = TRUE)
sfLibrary("gtools",character.only = TRUE)
library("ROCR")
input_path <- "D:\\yxue\\TSC\\LR\\work\\PredTest\\"
#input_path <- "D:\\yxue\\TSC\\LR\\Jun18\\Output\\"
output_path <- "D:\\jzhao\\RareDisease_TSC\\LR"

infileAll <- list.files(path=input_path, all.files=F, full.names=F, recursive=F)
#PredTest_wt1_Simulation1
wt <- gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[1], perl=T)
wt_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[X], perl=T)))
wt_level <- sort(as.numeric(levels(as.factor(wt_lst))))
sim_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+\\D)(\\d+)(\\W\\w+$)", "\\2", infileAll[X], perl=T)))
sim_level <- sort(as.numeric(levels(as.factor(sim_lst))))

#infile <- paste('PredTest_wt', wt, '_Simulation', sim, sep='')

get_recPrec_byBucket <- function(input_path, output_path){
    
    infileAll <- list.files(path=input_path, all.files=F, full.names=F, recursive=F)
    #PredTest_wt1_Simulation1
    wt <- gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[1], perl=T)
    wt_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[X], perl=T)))
    wt_level <- sort(as.numeric(levels(as.factor(wt_lst))))
    sim_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+\\D)(\\d+)(\\W\\w+$)", "\\2", infileAll[X], perl=T)))
    sim_level <- sort(as.numeric(levels(as.factor(sim_lst))))
    for (w in wt_level){
        
    }
}

get_recPrec_perSim <- function(sim, wt, input_path, output_path){
    if(!file.exists(output_path)){
        dir.create(output_path, recursive=T, showWarnings=T)
        setwd(output_path)
        
    }else{
        setwd(output_path)
        
    }
    infile <- paste('Prediction_On_Test_Sim', sim, sep='')
	dt <- read.table(paste(input_path, infile, '.csv', sep=''), header=T, sep=',')
	label <- c(rep(1, 50), rep(-1, nrow(dt)-50))
	pred <- dt$Pred
	cutoff <- pred
    n <- nrow(dt)
    TP <- unlist(lapply(X=1:n, function(X){sum(pred[1:50] >= cutoff[X])}))
    FP <- unlist(lapply(X=1:n, function(X){sum(pred[-(1:50)] >= cutoff[X])}))
    
    recall <- TP/50
    precision <- TP/(TP+FP)
    
	rec_prec <- data.frame(cbind(dt[, 1], recall, precision))
	colnames(rec_prec) <- c('Ptid', 'Recall', 'Precision')
	return(rec_prec)
}

num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
sfLibrary(snowfall)
sfExport('get_recPrec_perSim', "input_path", 'output_path', 'wt')
#sfClusterEval(library("e1071"))
rec_prec_idx_lst <- sfClusterApplyLB(1:10,get_recPrec_perSim, wt, input_path, output_path)

sfStop()

get_bucket <- function(n.bucket, wt){
	idx_recPrecAllSim <- numeric()
	for(i in 1:10){
		idx_recPrecAllSim <- rbind(idx_recPrecAllSim, rec_prec_idx_lst[[i]])
	}
	recPrec_byIdx <- aggregate(idx_recPrecAllSim[, -1], by=list(as.factor(idx_recPrecAllSim[, 1])), mean)
	recall <- recPrec_byIdx[, 2]
	recallCut<-cut(recall,breaks=n.bucket,include.lowest=T,right=T,labels=c(1:n.bucket))
    recPrec_byBucket <- aggregate(recPrec_byIdx[,-1], by=list(recallCut), function(i)round(mean(i, na.rm=T), 2))
	write.csv(recPrec_byBucket, paste("recall_precision_byBucket_model", wt, '_', n.bucket, ".csv", sep=''), row.names=F, quote=F)
    return(recPrec_byBucket)

}

recRec_byBucket <- get_bucket(100, "")



