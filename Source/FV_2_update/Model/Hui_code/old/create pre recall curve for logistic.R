# # create precision-recall curve for logistic model

library(ROCR)
library(snowfall)

path <- 'D:\\yxue\\TSC\\LR\\work\\PredTest\\'
file1 <- 'PredTest_wt1_Simulation'
file2 <- 'PredTest_wt2_Simulation'
ext <- '.csv'

create_curve <- function(i , wt){
  
  rec_prec <- numeric()
  file <- ifelse(wt==1, file1,file2)
  #reading in data    
    pred_list<- read.csv(paste(path, file,i,ext,sep=''), header=T)
    pred_list$label <- 0
    pred_list$label[1:50] <- 1
    
    cutoff <- pred_list[, 2]
    TP <- unlist(lapply(X=1:nrow(pred_list), function(X){sum(pred_list[1:50, 2] >= cutoff[X])}))
    FP <- unlist(lapply(X=1:nrow(pred_list), function(X){sum(pred_list[-(1:50), 2] >= cutoff[X])}))
    
    recall <- TP/50
    precision <- TP/(TP+FP)
    
    
    rec_prec <- cbind(pred_list[, 1], recall, precision)

    return(rec_prec)
   
}
sfInit(parallel=TRUE, cpus=10)
sfLibrary(snowfall)
sfExport('path', 'file1', 'file2', 'ext')
rec_prec_all2 <- sfClusterApplyLB(1:10,create_curve,2)

sfStop()

rec_prec_idx_set1 <- numeric()
rec_prec_idx_set2 <- numeric()

for(i in 1:10){
  rec_prec_idx_set1 <- rbind(rec_prec_idx_set1, rec_prec_all[[i]])
  rec_prec_idx_set2 <- rbind(rec_prec_idx_set2, rec_prec_all2[[i]])
}

obspred1 <- aggregate(rec_prec_idx_set1[, -1], by=list(as.factor(rec_prec_idx_set1[, 1])), function(i)mean(i, na.rm=T))[, -1]
obspred2 <- aggregate(rec_prec_idx_set2[, -1], by=list(as.factor(rec_prec_idx_set2[, 1])), function(i)mean(i, na.rm=T))[, -1]

recall1 <- obspred1[,1]
recall2 <- obspred2[,1]

predCut1<-cut(recall1,breaks=100,include.lowest=T,right=F,labels=c(1:100))
predCut2<-cut(recall2,breaks=100,include.lowest=T,right=F,labels=c(1:100))

curve_byBucket1 <- aggregate(obspred1[,], by=list(predCut1), function(i)round(mean(i, na.rm=T), 2))
curve_byBucket2 <- aggregate(obspred2[,], by=list(predCut2), function(i)round(mean(i, na.rm=T), 2))

write.csv(curve_byBucket1, 'D:\\Hui\\TSC project\\Results\\precision_recall_curve\\LR_model1_prec_rec_curve.csv', quote=F, row.names=F )
write.csv(curve_byBucket2, 'D:\\Hui\\TSC project\\Results\\precision_recall_curve\\LR_model2_prec_rec_curve.csv', quote=F, row.names=F )




