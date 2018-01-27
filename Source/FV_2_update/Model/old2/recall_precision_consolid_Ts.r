
pathInd <- "D:\\RareDisease_TSC\\Data\\Dict_Ind_2_update\\2n\\"
fileInd <- "TestIndices"
Ext <- '.csv'
indAllSim <- numeric()
for(i in 1:10){
    indAllSplit <- numeric()
    for(j in 1:8){
        ind <- read.table(paste(pathInd, fileInd, i,j, Ext, sep=''))
        indAllSplit <- rbind(indAllSplit, ind)
    }
    indAllSim <- c(indAllSim, indAllSplit[, 1])
}


recPrecBucket <- function(data, n.buck, consolid){
    if(consolid==T){
        obsPred <- aggregate(data[, -1], by=list(as.factor(data[, 1])), function(i)mean(i, na.rm=T))[, -1]
    }else{
        obsPred <- data    
    }
    recall <- obsPred[,1]
    
    #predCut <- cut2(recall, g=n.buck)
    predCut<-cut(recall,breaks=10,include.lowest=T,right=T,labels=c(1:10))
    
    num_pos <- aggregate(obsPred[,], by=list(predCut), function(i)round(mean(i, na.rm=T), 2))
    num_buck <- table(predCut)
    #num_neg <- aggregate(1-obsPred[, 1], by=list(predCut), sum)[, 2]
    #perc_pos <- num_pos/num_buck
    #write.xlsx(num_pos, file=paste('recall_precision_', data, '.xlsx', sep=''), sheetName=paste('Simulation',simulation,'_bucket_', n.buck, sep=''), row.names=T, append=T, showNA=T)
    return(num_pos)
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
    write.table(bucketi, paste("Simu_", i,  '_consolid=', "T", '.txt',sep=''), row.names=F, quote=F)
    return(bucketi)
}
rec_prec_bucket <- function(data, n.bucket, trim, infile, kernel, consolid){
    path <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, sep='')
    dir.create(paste(path, data, sep='/'), recursive=T, showWarnings=T)
    pathOutput <- paste("D:/RareDisease_TSC/Results/FV_2_update/2n_", kernel, "_", infile, "_maxPPV/Rs/", kernel, "/", data, sep='')
    
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
    #bucket_rbind <- numeric()
    #write.csv(bucket_rbind, paste(infile, '_', data, '.txt',sep=''), row.names=F, quote=F)
    num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
    sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = paste(pathInput,"recall_precision_consolid_parOnSimAll.txt", sep=''))
    sfLibrary(snowfall)
    sfExport('nonTSCNum', 'score', 'indAllSim', 'trim', 'consolid', 'getBucket', 'recPrecBucket', 'n.bucket')
    #sfClusterEval(library("e1071"))
    bucketResult <- sfClusterApplyLB(1:10,getBucket)
    sfStop()
    return(bucketResult)
}
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting2')
#bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=T, infile='setting1')
#bucket_result_Ts <- rec_prec_bucket('Vl', 10, trim=T, infile='setting1')
bucket_result_Ts <- rec_prec_bucket('Ts', 10, trim=F, infile='setting1', kernel='lin', consolid=T)
library('plyr')
bucket_result_Ts.df <- ldply(bucket_result_Ts, quickdf)
