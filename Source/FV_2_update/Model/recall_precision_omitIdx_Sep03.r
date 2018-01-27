############################################################################################
#Description:                                                                              #
#             create the consolidated recall-precision-curve data based on the model result#                
#             method : omit the patient id and use the recall in all the 10 simulations
#                       to get the consolidated recall-precision curve                     #
#Author: Jie Zhao                                                                          #
############################################################################################

library("ROCR")



  library("snowfall")
  sfLibrary("snow",character.only = TRUE)
  sfLibrary("snowfall", character.only = TRUE)
  sfLibrary("MASS", character.only = TRUE)
  sfLibrary("e1071", character.only = TRUE)
  sfLibrary("ROCR",character.only = TRUE)
  sfLibrary("pracma",character.only = TRUE)
  sfLibrary("gtools",character.only = TRUE)
library("ROCR")




getBucket <- function(i, n.bucket, score, nonTSCNum, target){
    predobj <- prediction(score[, i],c(rep(1, 50), rep(-1, nonTSCNum)))
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    #plot(perf, main=paste('simulation:', i, sep=''))
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    #n <- 50+nonTSCNum
    #cutoff <- unique(score[, i])
    #TP <- unlist(lapply(X=1:n, function(X){sum(score[1:50, i] >= cutoff[X])}))
    #FP <- unlist(lapply(X=1:n, function(X){sum(score[-(1:50), i] >= cutoff[X])}))
    
    #recall <- TP/50
    #precision <- TP/(TP+FP)
    rePrec <- cbind(recall, precision)
    bucket <- cut(precision, breaks=seq(0, 1, 1/n.bucket), include.lowest=T,right=F)
    rec_prec <- cbind(recall, precision)
    #rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    rec_prec_byBucket <- rec_prec
    irecall_atTgt <- unlist(lapply(target, function(Tgt){
        #idx <- which(abs(Tgt-rec_prec_byBucket[,3])==min(abs(Tgt-rec_prec_byBucket[,3]), na.rm=T))
        #recall_atTgt <- unique(rec_prec_byBucket[idx, 2])
        idx <- which(rec_prec_byBucket[, 2] >= Tgt)
        recall_atTgt <- mean(rec_prec_byBucket[idx, 1], na.rm=T)
        if (length(recall_atTgt) >1){
            recall_atTgt <- max(recall_atTgt, na.rm=T)
        }
        return(recall_atTgt)
    }))
    
    return(irecall_atTgt)
}
rec_prec_bucket <- function(data, n.bucket, infile, kernel, target){
    path <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "/Rs/", kernel, sep='')
    pathOutput <- paste(path, "/", data, sep='')
    
    if(!file.exists(paste(path, data, sep='/'))){
        dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
        setwd(pathOutput)
        
    }else{
        setwd(pathOutput)
        
    }
    pathInput <- path
    predScoreVl <- read.table(paste(pathInput, 'scoreVl.csv', sep='/'), head=T, sep=',')[, -1]
    predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep='/'), head=T, sep=',')[, -1]
    if(data=='Vl'){
        score <- predScoreVl
        
    }else if(data=='Ts'){
        score <- predScoreTs
        
    }else{
        stop('\n the wrong input string!\n')
    }
    nonTSCNum <- nrow(score)-50

    #indAllSim <- getIdx()
	
    num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = paste(pathInput,"recall_precision_consolid_parOnSimAll.txt", sep=''))
    sfLibrary(snowfall)
    sfLibrary(ROCR)
    sfExport('nonTSCNum', 'score',  'getBucket', 'n.bucket', 'target')
    #sfClusterEval(library("e1071"))
    recall_list <- sfClusterApplyLB(1:10,getBucket, n.bucket, score, nonTSCNum, target)
    
    sfStop()
    #library('plyr')
    #rec_prec_idx.df <- ldply(rec_prec_idx_allSimu, quickdf)
    library(plyr)
    recall_df <-ldply(recall_list, quickdf)
    recall_f <- apply(recall_df, 2, function(x)mean(x, na.rm=T))
    return(recall_f)

}
recall_fnl <- rec_prec_bucket('Ts', 100,  infile='setting1_maxPPV_huma', kernel='lin', target=c(0.25, 0.65, 0.75))
save(recall_fnl, file="measure_std_2.RData")
#plot

unique(bucket_result[,1])











getBucket <- function(i){
    predobj <- prediction(score[,i],c(rep(1, 50), rep(-1, nonTSCNum)))
    #add plot
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    #plot(perf, main=paste('simulation:', i, sep=''))
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    #n <- 50+nonTSCNum
    #cutoff <- unique(score[, i])
    #TP <- unlist(lapply(X=1:n, function(X){sum(score[1:50, i] >= cutoff[X])}))
    #FP <- unlist(lapply(X=1:n, function(X){sum(score[-(1:50), i] >= cutoff[X])}))
    
    #recall <- TP/50
    #precision <- TP/(TP+FP)
    rePrec <- cbind(recall, precision)
    bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
    rec_prec <- cbind(recall, precision)
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    return(rec_prec_byBucket)
}
rec_prec_bucket <- function(data, n.bucket, infile, kernel){
    #path <- paste("D:/RareDisease_TSC/Results/FV_2/2n_", kernel, "_", infile, "/Rs/", kernel, sep='')
    path <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_huma_nowt/Rs/", kernel, sep='')
    pathOutput <- paste(path, "/", data, sep='')
    
    if(!file.exists(paste(path, data, sep='/'))){
        dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
        setwd(pathOutput)
        
    }else{
        setwd(pathOutput)
        
    }
    pathInput <- path
    predScoreVl <- read.table(paste(pathInput, 'scoreVl.csv', sep='/'), head=T, sep=',')[, -1]
    predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep='/'), head=T, sep=',')[, -1]
    if(data=='Vl'){
        score <- predScoreVl
        
    }else if(data=='Ts'){
        score <- predScoreTs
        
    }else{
        stop('\n the wrong input string!\n')
    }
    nonTSCNum <- nrow(score)-50
    
    #indAllSim <- getIdx()
    
    num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    #sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = paste(pathInput,"recall_precision_consolid_parOnSimAll.txt", sep=''))
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
    sfLibrary(snowfall)
    sfLibrary(ROCR)
    sfExport('nonTSCNum', 'score',  'getBucket', 'n.bucket')
    #sfClusterEval(library("e1071"))
    rec_prec_byBucket <- sfClusterApplyLB(1:10,getBucket)
    
    sfStop()
    #library('plyr')
    #rec_prec_idx.df <- ldply(rec_prec_idx_allSimu, quickdf)
    rec_prec_byBucket_allSim <- numeric()
    for(i in 1:10){
        rec_prec_byBucket_allSim <- rbind(rec_prec_byBucket_allSim, rec_prec_byBucket[[i]])
    }
    
    rec_prec_result <- aggregate(rec_prec_byBucket_allSim[, -1], by=list(rec_prec_byBucket_allSim[, 1]), function(i)mean(i, na.rm=T))
    write.csv(rec_prec_result, 'Precision-Recall Curve.csv', row.names=F)
    #plot
    #pdf(file=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
    #recall <- rec_prec_result[, 2]
    ##precision <- rec_prec_result[, 3]
    #plot(recall, precision, type='l', main=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
    #dev.off()
    
    #get pos_num of patient according to target recall
    rec <- rec_prec_result[, 2]
    temp1 <- numeric()
    for (target_recall in target_recall_list){
        tar_idx <- which(abs(rec-target_recall)==min(abs(rec-target_recall)))
        prec <- rec_prec_result[tar_idx, 3]
        pos_num <- 50*target_recall/prec
        F_score <- 2*target_recall*prec/(target_recall+prec)
        temp1 <- rbind(temp1, c(prec, pos_num, F_score))
    }
    #pos_num_allWt <- c(pos_num_allWt, pos_num)

    #pos_num_allWt_table <- cbind(wt_list, pos_num_allWt)
    temp2 <- as.data.frame(temp1)
    colnames(temp2)<- c('Precision associated with 50% recall ', 'Number of patients flagged to identify 25 TSC patients', 'F1')
    write.csv(temp2, 'Extra_columns_forTable4.csv', row.names=F)

    return(temp2)

    
}

extra1 <- rec_prec_bucket('Ts', 100,  infile='model1', kernel='lin')
extra3 <- rec_prec_bucket('Ts', 100,  infile='model3', kernel='lin')
extra6 <- rec_prec_bucket('Ts', 100,  infile='model6', kernel='lin')
extra7 <- rec_prec_bucket('Ts', 100,  infile='model7', kernel='lin')
extra9 <- rec_prec_bucket('Ts', 100,  infile='model9_v2', kernel='lin')
extra21 <- rec_prec_bucket('Ts', 100,  infile='model1', kernel='rbf')
extra23 <- rec_prec_bucket('Ts', 100,  infile='model3', kernel='rbf')
extra_all <- rbind(extra1, extra3, extra6, extra7, extra9, extra21, extra23)
write.csv(extra_all, 'D:\\RareDisease_TSC\\Results\\FV_2\\extra_all.csv', row.names=F)



#create summary table4 for base setting for each simulation
outPath <- 'D:\\jzhao\\RareDisease_TSC\\03. Output\\Jul29'
setwd(outPath)
inPath <- 'D:\\RareDisease_TSC\\Results\\FV_2\\2n_lin_model1\\Rs\\lin'

create_table4_forEachSim_baseSetting <- function(target_recall_list){
    score <- read.table(paste(inPath, '\\scoreTs.csv', sep=''), sep=',', header=T)[, -1]
    prec_target_list <- c(0.05, 0.1, 0.25, 0.5)
    
    
    temp3 <- as.data.frame(t(as.data.frame(lapply(1:ncol(score), function(i){
        predobj <- prediction(score[,i],c(rep(1, 50), rep(-1, nrow(score)-50)))
        #add plot
        perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
        #plot(perf, main=paste('simulation:', i, sep=''))
        auc <- performance(predobj, 'auc')@y.values[[1]]
        recall <- perf@x.values[[1]]
        precision <- perf@y.values[[1]]
        #rePrec <- cbind(recall, precision)
        bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
        rec_prec <- cbind(recall, precision)
        rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
        temp1 <- unlist(lapply(prec_target_list, function(X){
            idx <- which(abs(rec_prec_byBucket[, 3]-X)==min(abs(rec_prec_byBucket[, 3]-X), na.rm=T))
            rec_sel <- rec_prec_byBucket[idx, 2]
            return(rec_sel)
        }))        
        
        return(c(auc,temp1))
    }))))
    
    cutoff <- 50/(nrow(score)-50)
    temp2 <- as.data.frame(t(as.data.frame(lapply(1:10, function(X){
        #score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
        TP <- sum(score[1:50, X]>=cutoff)
        FP <- sum(score[-(1:50), X] >= cutoff)
        TN <- sum(score[-(1:50), X] < cutoff)
        
        Recall <- TP/50
        Precision <- TP/(TP+FP)
        Specificity <- TN/(FP+TN)
        rate <- c(Recall, Specificity, Precision)
        return(rate)
    }))))
    
    extra_3columns1 <- lapply(1:ncol(score), function(i){
        predobj <- prediction(score[,i],c(rep(1, 50), rep(-1, nrow(score)-50)))
        #add plot
        perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
        #plot(perf, main=paste('simulation:', i, sep=''))
        auc <- performance(predobj, 'auc')@y.values[[1]]
        recall <- perf@x.values[[1]]
        precision <- perf@y.values[[1]]
        #rePrec <- cbind(recall, precision)
        bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
        rec_prec <- cbind(recall, precision)
        rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))

        tar_idx <- which(abs(rec_prec_byBucket[, 2]-target_recall_list)==min(abs(rec_prec_byBucket[, 2]-target_recall_list)))
        prec <- rec_prec_byBucket[tar_idx, 3]
        pos_num <- 50*target_recall_list/prec
        F_score <- 2*target_recall_list*prec/(target_recall_list+prec)
        temp4 <-  c(prec, pos_num, F_score)
        
        
        
        return(temp4)
        
    })
    
    library(dplyr)
    extra_3columns <- ldply(extra_3columns1,quickdf)
    t(as.data.frame(extra_3columns1))
    temp_f <- cbind(temp3, temp2, extra_3columns)
    avg <- apply(temp_f, 2, mean)
    temp_f1 <- rbind(temp_f, avg)
    temp_f2 <- cbind(c(1:10, 'Average'), temp_f1)
    colnames(temp_f2) <- c('Simulation', 'AUC', 'Recall at 5% precision', 'Recall at 10% precision', 'Recall at 25% precision', 'Recall at 50% precision', 'Recall for pos / neg (0.5 for LR) cut-off', 
                          'Specifivity for pos / neg (0.5 for LR) cut-off', 'Precision for pos / neg (0.5 for LR)  cut-off', 
                          'Precision associated with 50% recall', 'Number of patients flagged to identify 25 TSC patients', 'F1')
    write.csv(temp_f2, 'Table4_bySimu_baseSetting.csv', row.names=F)
    return(temp_f2)
}
table4_forEachSim_baseSetting_result <- create_table4_forEachSim_baseSetting(c(0.5))
