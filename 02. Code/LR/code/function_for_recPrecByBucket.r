get_recPrec_perSim <- function(infile, input_path){
    dt <- read.table(paste(input_path, infile, sep=''), header=T, sep=',')
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

get_bucket <- function(n.bucket, wt){
    idx_recPrecAllSim <- numeric()
    for(i in 1:10){
        idx_recPrecAllSim <- rbind(idx_recPrecAllSim, rec_prec_idx_lst[[i]])
    }
    recPrec_byIdx <- aggregate(idx_recPrecAllSim[, -1], by=list(as.factor(idx_recPrecAllSim[, 1])), mean)
    recall <- recPrec_byIdx[, 2]
    recallCut<-cut(recall,breaks=n.bucket,include.lowest=T,right=T,labels=c(1:n.bucket))
    recPrec_byBucket <- aggregate(recPrec_byIdx[,-1], by=list(recallCut), function(i)round(mean(i, na.rm=T), 2))
    write.csv(recPrec_byBucket, paste("recall_precision_byBucket_wt", wt, '_bucket', n.bucket, ".csv", sep=''), row.names=F, quote=F)
    return(recPrec_byBucket)
    
}
