############################################################################################
#Description:                                                                              #
#             create the consolidated recall-precision-curve data based on the model result#                
#             method ONE:  set scores of all the 10 simulations together first.            #   
#             method TWO:  set recall and precision of the 10 simulations together.        #
#                                                                                          #
#Author: Jie Zhao                                                                          #
############################################################################################

library("ROCR")

getIdx <- function(){
    pathInd <- "D:\\RareDisease_TSC\\Data\\Dict_Ind_2_update\\2n\\"
    fileInd <- "TestIndices"
    fileInd1 <- "TSCTestIndices"
    Ext <- '.csv'
    indAllSim <- numeric()
    for(i in 1:10){
        indAllSplit <- numeric()
        ind1 <- read.table(paste(pathInd, fileInd1, i, Ext, sep=''))[, 1]
        
        for(j in 1:8){
            
            ind <- read.table(paste(pathInd, fileInd, i,j, Ext, sep=''))[, 1]
            indAllSplit <- c(indAllSplit, ind)
        }
        indAllSplitAddTsc <- c(ind1, indAllSplit)
        indAllSim <- cbind(indAllSim, indAllSplitAddTsc)
    }
    return(indAllSim)
}

getScore_allSimu <- function(predScore){
    score_allSim <- unlist(lapply(1:10, function(X)predScore[, i]))
    return(score_allSim)
}

getIdx_v2 <- function(){
    pathInd <- "D:\\RareDisease_TSC\\Data\\Dict_Ind_2_update\\2n\\"
    fileInd <- "TestIndices"
    fileInd1 <- "TSCTestIndices"
    Ext <- '.csv'
    indAllSim2 <- numeric()
    for(i in 1:10){
        indAllSplit <- numeric()
        #ind1 <- read.table(paste(pathInd, fileInd1, i, Ext, sep=''))
        
        for(j in 1:8){
            
            ind <- read.table(paste(pathInd, fileInd, i,j, Ext, sep=''))[, 1]
            indAllSplit <- rbind(indAllSplit, ind)
        }
        #indAllSplitAddTsc <- c(ind1[, 1], indAllSplit[, 1])
        indAllSim2 <- c(indAllSim2, indAllSplit)
    }
    indAllSim1 <- unlist(lapply(1:10, function(X)read.table(paste(pathInd, fileInd1, X, Ext, sep=''))[, 1]))
    indAllSim <- c(indAllSim1,indAllSim2 )
    return(indAllSim)
}

getScore_allSimu_v2 <- function(predScore){
    score_allSim1 <- unlist(lapply(1:10, function(X)predScore[1:50, i]))
    score_allSim2 <- unlist(lapply(1:10, function(X)predScore[-(1:50), i]))
    score_allSim <- c(score_allSim1, score_allSim2)
    return(score_allSim)
}

recPrecBucket_v2 <- function(score, n.bucket){
    scoreByIdx <- aggregate(score[, -1], by=list(as.factor(score[, 1])), function(i)mean(i, na.rm=T))
    
    label <- ifelse(as.numeric(as.vector(scoreByIdx[, 1])) %in% score[1:500,1], 1, -1)
    scoreLabel <- cbind(label, scoreByIdx[, 2])
    
    predobj <- prediction(scoreByIdx[, 2], label)
    perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    pdf(file=paste(kernel,infile, data,"onAllSimuByPtIdx.pdf", sep='_'), width=11, height=12)
    #par(mfrow=c(5,2))
    par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
    plot(perf, main=paste(kernel,infile, data, "onAllSimuByPtIdx", sep='_'))
    dev.off()
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    rec_prec <- cbind(recall, precision)
    
    #predCut <- cut2(recall, g=n.buck)
    recallCut<-cut(recall,breaks=n.bucket,include.lowest=T,right=T,labels=c(1:n.bucket))
    
    rec_prec_byBucket <- aggregate(rec_prec[,], by=list(recallCut), function(i)round(mean(i, na.rm=T), 7))
    #num_buck <- table(predCut)
    #num_neg <- aggregate(1-obsPred[, 1], by=list(predCut), sum)[, 2]
    #perc_pos <- num_pos/num_buck
    #write.xlsx(num_pos, file=paste('recall_precision_', data, '.xlsx', sep=''), sheetName=paste('Simulation',simulation,'_bucket_', n.buck, sep=''), row.names=T, append=T, showNA=T)
    return(rec_prec_byBucket)
}

getRec_prec_bucket <- function(data, n.bucket,infile, kernel){
    path <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, sep='')
    pathOutput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", data, sep='')
    if(!file.exists(paste(path, data, sep='/'))){
        dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
        setwd(pathOutput)
        
    }else{
        setwd(pathOutput)
        
    }
    pathInput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", sep='')
    #predScoreVl <- read.table(paste(pathInput, 'scoreVl.csv', sep=''), head=T, sep=',')[, -1]
    predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep=''), head=T, sep=',')[, -1]
    predScoreAllSim <- getScore_allSimu_v2(predScoreTs)
    idxAllSim <- getIdx_v2()
    predScoreAddIdx <- cbind(idxAllSim, predScoreAllSim)
    rec_prec_byBucket <- recPrecBucket_v2(predScoreAddIdx, n.bucket)
    #bucket_rbind <- numeric()
    write.csv(rec_prec_byBucket, paste(kernel, '_', infile, "_", data, '_bucket', n.bucket, '_setScore.txt',sep=''), row.names=F, quote=F)
    return(rec_prec_byBucket)
}
rec_prec_byBucket1 <- getRec_prec_bucket('Ts', 10, 'setting1', 'lin')
rec_prec_byBucket2 <- getRec_prec_bucket('Ts', 10, 'setting3', 'lin')
rec_prec_byBucket3 <- getRec_prec_bucket('Ts', 10, 'setting1', 'rbf')
rec_prec_byBucket1 <- getRec_prec_bucket('Ts', 100, 'setting1', 'lin')
rec_prec_byBucket2 <- getRec_prec_bucket('Ts', 100, 'setting3', 'lin')
rec_prec_byBucket3 <- getRec_prec_bucket('Ts', 100, 'setting1', 'rbf')


scoreLabel1 <- scoreLabel[1:100,]
cutoff<- scoreLabel1[, 2]
TP <- unlist(lapply(X=1:nrow(scoreLabel1), function(X){sum(scoreLabel1[, 1]==1 & scoreLabel1[, 2]>= cutoff[X])}))
FP <- unlist(lapply(X=1:nrow(scoreLabel1), function(X){sum(scoreLabel1[, 1]==-1 & scoreLabel1[, 2]>= cutoff[X])}))
recall <- TP/sum(scoreLabel1[, 1]==1)
precision <- TP/(TP+FP)

curve <- cbind(recall, precision)
curve_order <- curve[order(recall),]

scoreByIdx <- scoreLabel1
predobj <- prediction(scoreByIdx[, 2], scoreByIdx[, 1])
perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
recall <- perf@x.values[[1]]
precision <- perf@y.values[[1]]
rec_prec <- cbind(recall, precision)
rec_prec_order <- rec_prec[order(rec_prec[, 1]),]


cbind(curve_order, rec_prec_order)






  library("snowfall")
  sfLibrary("snow",character.only = TRUE)
  sfLibrary("snowfall", character.only = TRUE)
  sfLibrary("MASS", character.only = TRUE)
  sfLibrary("e1071", character.only = TRUE)
  sfLibrary("ROCR",character.only = TRUE)
  sfLibrary("pracma",character.only = TRUE)
  sfLibrary("gtools",character.only = TRUE)
library("ROCR")

getIdx <- function(){
    pathInd <- "D:\\RareDisease_TSC\\Data\\Dict_Ind_2_update\\2n\\"
    fileInd <- "TestIndices"
    fileInd1 <- "TSCTestIndices"
    Ext <- '.csv'
    indAllSim <- numeric()
    for(i in 1:10){
        indAllSplit <- numeric()
        ind1 <- read.table(paste(pathInd, fileInd1, i, Ext, sep=''))[, 1]
        
        for(j in 1:8){
            
            ind <- read.table(paste(pathInd, fileInd, i,j, Ext, sep=''))[, 1]
            indAllSplit <- c(indAllSplit, ind)
        }
        indAllSplitAddTsc <- c(ind1, indAllSplit)
        indAllSim <- cbind(indAllSim, indAllSplitAddTsc)
    }
    return(indAllSim)
}


recPrecBucket <- function(rec_prec, n.bucket, consolid, data infile){
    if(consolid==T){
        obsPred <- aggregate(rec_prec[, -1], by=list(as.factor(rec_prec[, 1])), function(i)mean(i, na.rm=T))[, -1]
    }else{
        obsPred <- rec_prec    
    }
    pdf(file=paste(kernel,infile, data,"onAllSimuByPtIdx_setRecPrec.pdf", sep='_'), width=11, height=9)
    #par(mfrow=c(5,2))
    par(cex.main=1.3, cex.lab=1.2, cex.axis=1)
    plot(obsPred, main=paste(kernel,infile, data, "onAllSimuByPtIdx_setPrecPrec", sep='_'))
    dev.off()
    
    #idx_uniq <- ifelse(as.numeric(as.vector(obsPred[, 1])) %in% as.numeric(as.vector(data[1:50,1])), 1, -1)
    recall <- obsPred[,1]
    
    #predCut <- cut2(recall, g=n.buck)
    predCut<-cut(recall,breaks=n.bucket,include.lowest=T,right=T,labels=c(1:n.bucket))
    
    curve_byBucket <- aggregate(obsPred[,], by=list(predCut), function(i)round(mean(i, na.rm=T), 2))
    #num_buck <- table(predCut)
    #num_neg <- aggregate(1-obsPred[, 1], by=list(predCut), sum)[, 2]
    #perc_pos <- num_pos/num_buck
    #write.xlsx(num_pos, file=paste('recall_precision_', data, '.xlsx', sep=''), sheetName=paste('Simulation',simulation,'_bucket_', n.buck, sep=''), row.names=T, append=T, showNA=T)
    return(curve_byBucket)
}

getBucket <- function(i){
    #predobj <- prediction(score[,i],c(rep(1, 50), rep(-1, nonTSCNum)))
    #add plot
    #perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
    #plot(perf, main=paste('simulation:', i, sep=''))
    #recall <- perf@x.values[[1]]
    #precision <- perf@y.values[[1]]
    n <- 50+nonTSCNum
    cutoff <- score[, i]
    TP <- unlist(lapply(X=1:n, function(X){sum(score[1:50, i] >= cutoff[X])}))
    FP <- unlist(lapply(X=1:n, function(X){sum(score[-(1:50), i] >= cutoff[X])}))
    
    recall <- TP/50
    precision <- TP/(TP+FP)
    
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
    #bucket <- recPrecBucket(rec_prec, n.bucket, consolid)
    #bucketi <- as.data.frame(cbind(Simulation=i, bucket))
    #bucket_rbind <- rbind(bucket_rbind, bucketi)
    #write.table(bucketi, paste("Simu_", i,  '_consolid=', "T", '.txt',sep=''), row.names=F, quote=F)
    return(rec_prec)
}
rec_prec_bucket <- function(data, n.bucket, trim, infile, kernel, consolid){
    path <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "/Rs/", kernel, sep='')
    pathOutput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "/Rs/", kernel, "/", data, sep='')
    
    if(!file.exists(paste(path, data, sep='/'))){
        dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
        setwd(pathOutput)
        
    }else{
        setwd(pathOutput)
        
    }
    pathInput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", sep='')
    predScoreVl <- read.table(paste(pathInput, 'scoreVl.csv', sep=''), head=T, sep=',')[, -1]
    predScoreTs <- read.table(paste(pathInput, 'scoreTs.csv', sep=''), head=T, sep=',')[, -1]
    if(data=='Vl'){
        score <- predScoreVl
        
    }else if(data=='Ts'){
        score <- predScoreTs
        
    }else{
        stop('\n the wrong input string!\n')
    }
    nonTSCNum <- nrow(score)-50

    indAllSim <- getIdx()
	
    num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = paste(pathInput,"recall_precision_consolid_parOnSimAll.txt", sep=''))
    sfLibrary(snowfall)
    sfExport('nonTSCNum', 'score', 'indAllSim', 'trim', 'consolid', 'getBucket', 'recPrecBucket', 'n.bucket')
    #sfClusterEval(library("e1071"))
    rec_prec_idx_allSimu <- sfClusterApplyLB(1:10,getBucket)
    
    sfStop()
    
    #library('plyr')
    #rec_prec_idx.df <- ldply(rec_prec_idx_allSimu, quickdf)
    rec_prec_idx_set <- numeric()
    for(i in 1:10){
        rec_prec_idx_set <- rbind(rec_prec_idx_set, rec_prec_idx_allSimu[[i]])
    }
    
    bucketResult_allSimu <- recPrecBucket(rec_prec_idx_set, n.bucket, consolid, data, infile)
    write.csv(bucketResult_allSimu, paste(kernel, '_', infile, "_", data, '_bucket', n.bucket, '_setRecPrec.txt',sep=''), row.names=F, quote=F)
    return(bucketResult_allSimu)
    
}
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting1')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting1')
bucket_result_1 <- rec_prec_bucket('Ts', 10, trim=F, infile='setting1', kernel='lin', consolid=T)
bucket_result_3 <- rec_prec_bucket('Ts', 10, trim=F, infile='setting3', kernel='lin', consolid=T)
bucket_result_21 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting1', kernel='lin', consolid=T)
bucket_result_22 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting1', kernel='rbf', consolid=T)
bucket_result_2 <- rec_prec_bucket('Ts', 10, trim=F, infile='setting1', kernel='rbf', consolid=T)

bucket_result_23 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting3', kernel='lin', consolid=T)

bucket_result_16 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting6', kernel='lin', consolid=T)
bucket_result_19 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting9', kernel='lin', consolid=T)
bucket_result_112 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting12', kernel='lin', consolid=T)
bucket_result_23 <- rec_prec_bucket('Ts', 100, trim=F, infile='setting3', kernel='rbf', consolid=T)
