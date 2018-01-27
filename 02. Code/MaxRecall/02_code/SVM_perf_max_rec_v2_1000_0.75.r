

# Maximizing Recall at targer level of precision
# Date: 2015/06/09
# Developed by : Hui Jin, Zhiyu Wang, Jie Zhao

# Load required packages 
rm(list=ls())
library("snowfall")
library(e1071)
library(gtools)
library(ROCR)

# Some Parameters to set
NumSplit <- 8         # Number of splits for test data (for parallel processing)
ThreshLim <- 50       # Uncomment thisline for binary classification (when ONE is set to FALSE, set ThresLim to 50)
NumSimulation <- 10   # Number of simulationsline
Ext <- '.csv'

#some path need to set
cygwin_path <- 'C:/cygwin64'
log_path <- 'D:/jzhao/RareDisease_TSC/Max_recall/02_code'
input_path <- 'D:/RareDisease_TSC/Data/FV_2/2n'
svmperf_path <- 'D:/jzhao/RareDisease_TSC/Max_recall/02_code/svm_perf_1000_0.25_maxRecall'
root_path <- 'D:/jzhao/RareDisease_TSC/Max_recall/03_model'

output_path_mod <- paste(root_path, '/Model', sep='')
#dir.create(output_path_mod)
output_path_rs <- paste(root_path, '/Rs', sep='')
#dir.create(output_path_rs)
output_path_ind <- paste(root_path, '/Indices', sep='')
#dir.create(output_path_ind)


##################################################################################################
# !!!! Need to be decided, if changed need to change the svm_struct_api.c line 1811 by hand !!!!!#
Target_ppv <- '0.75/0.25 '                                                                              #
##################################################################################################

######################################################
# !!!!Input Number of your Trail and Cost!!!!!#      #
# !!! need to change every time ####                 #
Trail <- "2_0.25_maxRecall"                                           #
cvec <- seq(10,100, 10)
svm_option <- ' -l 3 -w 0 --b 0 '
Cost <- 'from 10 to 100 by 10 ' 
crit <- ' prbep loss'
######################################################
eval(parse(text=paste("out_path <- paste(output_path_rs,'/result_trail_", Trail, "',sep='')",sep='')))


# Run this after set Trail and Cost to output model log
log <- cbind(Trail, Cost, svm_option,crit)
write.table(log, paste(log_path, '/Model_log_linear_c.csv', sep=''), append=T, quote=F, row.names=F, sep=',')

# Train SVM-perf model
Max_Rec <- function(counter, Trail, cvec) {
    
    # Some constance to be set
    threshint <- c(1,0,ThreshLim)
    selROCAUC <- 0.0
    selTP <- -1
    selwt <- NULL
    sval <- counter * 590700
    
    # Input data
    TrFile1 <- "/trainv"
    TrFile <-  "/train"
    VlFile <-  "/valid"
    TsFilePos <- "/testpos"
    TsFileNeg <- "/testneg"
    TrFile1 <- paste(input_path, TrFile1,counter,Ext,sep="")
    TrFile <- paste(input_path,TrFile,counter,Ext,sep="")
    VlFile <- paste(input_path,VlFile,counter,Ext,sep="") 
    TsFilePos <- paste(input_path,TsFilePos,counter,Ext,sep="")
    
    # Output file (Index and result files)
    # Create model folder by simulation
    eval(parse(text=paste("model_path <- paste(output_path_mod,'/svm_model_trail_", Trail, "',sep='')",sep='')))     
    dir.create(model_path) 
    tr_model_path <- paste(model_path, '/tr_model', sep='')
    trvl_model_path <- paste(model_path, '/trvl_model', sep='')
    dir.create(tr_model_path)
    dir.create(trvl_model_path)
    eval(parse(text=paste("tr_model_sim_path <- paste(tr_model_path,'/model_simulation_", counter, "',sep='')",sep='')))     
    dir.create(tr_model_sim_path) 
    eval(parse(text=paste("trvl_model_sim_path <- paste(trvl_model_path,'/model_simulation_", counter, "',sep='')",sep='')))     
    dir.create(trvl_model_sim_path) 
    
    # Create results folder by simulation
    eval(parse(text=paste("result_path <- paste(output_path_rs,'/result_trail_", Trail, "',sep='')",sep='')))     
    dir.create(result_path) #create a new result folder
    vl_result_path <- paste(result_path, '/vl_rs', sep='')
    ts_result_path <- paste(result_path, '/ts_rs', sep='')
    dir.create(vl_result_path)
    dir.create(ts_result_path)
    eval(parse(text=paste("vl_result_sim_path <- paste(vl_result_path,'/vl_result_simulation_", counter, "',sep='')",sep='')))     
    dir.create(vl_result_sim_path) #create a new result folder
    eval(parse(text=paste("ts_result_sim_path <- paste(ts_result_path,'/ts_result_simulation_", counter, "',sep='')",sep='')))     
    dir.create(ts_result_sim_path) #create a new result folder
    
    # Create index folder by simulation
    TSCIndexFile  <- "/ProspectiveTSC"
    ConTSCIndexFile  <- "/ConfirmedTSC" 
    eval(parse(text=paste("ind_path <- paste(output_path_ind,'/Indices_trail_", Trail, "',sep='')",sep='')))     
    dir.create(ind_path)
    TSCIndexFile <- paste(ind_path, TSCIndexFile,counter,sep="")
    ConTSCIndexFile <- paste(ind_path, ConTSCIndexFile,counter,Ext,sep="")
    
    # Create script folder
    eval(parse(text=paste("scrips_path <- paste(svmperf_path,'/script_trail_", Trail, "',sep='')",sep='')))     
    dir.create(scrips_path) 
    eval(parse(text=paste("sim_path <- paste(scrips_path,'/simulation_", counter, "',sep='')",sep=''))) 
    dir.create(sim_path)
    unix_path <- paste(strsplit(sim_path,":")[[1]][1],strsplit(sim_path,":")[[1]][2],sep='')
    
    validation <- read.matrix.csr(VlFile,fac=TRUE)
    train <- read.matrix.csr(TrFile1,fac=TRUE)
    
    validResMat <- numeric()
    trainResMat <- numeric()
    # Invoke cygwin from R, training SVM-perf on training set and apply on validation set
    for (c in cvec ){
        
        # Create script files for validation
        eval(parse(text=paste('out<- file("',sim_path,'/learn_model_C_',c,'.txt", "w")',sep='')))
        eval(parse(text=paste('writeLines("cd ',svmperf_path,'",out)',sep='')))
        eval(parse(text=paste('writeLines("./svm_perf_learn -c ',c, svm_option , TrFile1, ' ',tr_model_sim_path,'/model_C_',c,'",out)',sep='')))
        eval(parse(text=paste('writeLines("./svm_perf_classify ', VlFile ,' ',tr_model_sim_path,'/model_C_',c,' ',vl_result_sim_path,'/pred_C_',c,'.csv",out)',sep='')))
        close(out)
        
        # Create script files for training
        eval(parse(text=paste('out<- file("',sim_path,'/learn_model_tr_C_',c,'.txt", "w")',sep='')))
        eval(parse(text=paste('writeLines("cd ',svmperf_path,'",out)',sep='')))
        eval(parse(text=paste('writeLines("./svm_perf_classify ', TrFile1 ,' ',tr_model_sim_path,'/model_C_',c,' ',vl_result_sim_path,'/pred_tr_C_',c,'.csv",out)',sep='')))
        close(out)
        
        # Train the model
        eval(parse(text=paste('shell("',cygwin_path,'/bin/dos2unix.exe /cygdrive/',unix_path,'/learn_model_C_',c,'.txt",intern=T)',sep='')))
        eval(parse(text=paste('pred <- shell("',cygwin_path,'/bin/bash.exe --login -i /cygdrive/',unix_path,'/learn_model_C_',c,'.txt",intern=T)',sep='')))
        eval(parse(text=paste('shell("',cygwin_path,'/bin/dos2unix.exe /cygdrive/',unix_path,'/learn_model_tr_C_',c,'.txt",intern=T)',sep='')))
        eval(parse(text=paste('pred_tr <- shell("',cygwin_path,'/bin/bash.exe --login -i /cygdrive/',unix_path,'/learn_model_tr_C_',c,'.txt",intern=T)',sep='')))
        
        # Read the Predictive Score
        eval(parse(text=paste('predscore <- read.csv("',vl_result_sim_path,'/pred_C_',c,'.csv",header=F)',sep='')))
        eval(parse(text=paste('predscore_tr <- read.csv("',vl_result_sim_path,'/pred_tr_C_',c,'.csv",header=F)',sep='')))
        
        # Performance measures for validation
        n<- length(pred)
        iAUC <- as.numeric(strsplit(pred[n-1],":")[[1]][2])
        iFP <- sum(predscore > 0 & validation$y == -1)#false positive number
        iTP <- sum(predscore > 0 & validation$y == 1)#true postive number
        iPPV <- as.numeric(strsplit(pred[n-5],":")[[1]][2])
        iRecall <- as.numeric(strsplit(pred[n-4],":")[[1]][2])
        
        temp <- c(iAUC, iFP, iTP,iRecall, iPPV, c )
        validResMat <- rbind(validResMat, temp) 
        
        # Performance measures for training
        n<- length(pred_tr)
        iAUC_tr <- as.numeric(strsplit(pred_tr[n-1],":")[[1]][2])
        iFP_tr <- sum(predscore_tr > 0 & train$y == -1)#false positive number
        iTP_tr <- sum(predscore_tr > 0 & train$y == 1)#true postive number
        iPPV_tr <- as.numeric(strsplit(pred_tr[n-5],":")[[1]][2])
        iRecall_tr <- as.numeric(strsplit(pred_tr[n-4],":")[[1]][2])
        
        temp_tr <- c(counter,iAUC_tr, iFP_tr, iTP_tr,iRecall_tr, iPPV_tr, c )
        trainResMat <- rbind(trainResMat, temp_tr)
        
        
    }
    
    # select FP<=50 or min, maximize TP
    ifelse( length(which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[3] ))) == 0, FPIndex <- which((validResMat[,2] >= threshint[2]) & (validResMat[,2] <= threshint[3] )), FPIndex <- which((validResMat[,2] >= threshint[1]) & (validResMat[,2] <= threshint[3] )) )
    
    ifelse(invalid(FPIndex), FPIndex <- which(validResMat[, 2]==min(validResMat[, 2])), FPIndex) 
    TPIndex <- FPIndex[which(validResMat[FPIndex,3] > selTP)]
    
    MaxIndex <- FPIndex[which( validResMat[TPIndex,3] == max(validResMat[TPIndex,3]))] 
    
    if (length(MaxIndex) > 1) {
        selIndex_all <- MaxIndex[which(validResMat[MaxIndex,4]==max(validResMat[MaxIndex,4]))]
        set.seed(sval) 
        selIndex <- sample(selIndex_all,1)
    }else{
        selIndex <- MaxIndex[which(validResMat[MaxIndex,4]==max(validResMat[MaxIndex,4]))]
    }
    
    if (length(MaxIndex) == 1) {
        selIndex <- MaxIndex
    }
    
    # Performance on validation set
    valid_perf <- c( counter, validResMat[selIndex,])
    
    # Selected Cost
    selC <- validResMat[selIndex, 6]
    
    # Train a new SVM-perf on training + validation set, apply on postive data
    # Create script files for training + validation
    eval(parse(text=paste('out<- file("',sim_path,'/final_model_pos.txt", "w")',sep='')))
    eval(parse(text=paste('writeLines("cd ',svmperf_path,'",out)',sep='')))
    eval(parse(text=paste('writeLines("./svm_perf_learn -c ',selC, svm_option , TrFile, ' ',trvl_model_sim_path,'/final_model",out)',sep='')))
    eval(parse(text=paste('writeLines("./svm_perf_classify ', TsFilePos ,' ',trvl_model_sim_path,'/final_model ',ts_result_sim_path,'/pred_pos.csv",out)',sep='')))
    close(out)
    
    
    # Train the model and apply on positive data
    eval(parse(text=paste('shell("',cygwin_path,'/bin/dos2unix.exe /cygdrive/',unix_path,'/final_model_pos.txt",intern=T)',sep='')))
    eval(parse(text=paste('pred <- shell("',cygwin_path,'/bin/bash.exe --login -i /cygdrive/',unix_path,'/final_model_pos.txt",intern=T)',sep='')))
    
    # Read the Predictive Score
    eval(parse(text=paste('predscore_pos <- read.csv("',ts_result_sim_path,'/pred_pos.csv",header=F)',sep='')))
    # Save scores in a file
    write.csv(predscore_pos,file = ConTSCIndexFile ,quote=FALSE,row.names = FALSE)
    
    # Using the same model apply on negative data
    predscore_neg_all <- numeric()
    for (i in 1:NumSplit){
        
        TsFileNeg1 <-  paste(input_path,TsFileNeg,counter,i,Ext,sep="")
        TSCIndexFile1 <- paste(TSCIndexFile,i,Ext,sep="") 
        
        eval(parse(text=paste('out<- file("',sim_path,'/final_model_neg',i,'.txt", "w")',sep='')))
        eval(parse(text=paste('writeLines("cd ',svmperf_path,'",out)',sep='')))
        #eval(parse(text=paste('writeLines("./svm_perf_learn -c ',selC,' -l 3 -w 4 ', TrFile, ' ',trvl_model_sim_path,'/model_C_',c,'",out)',sep='')))
        eval(parse(text=paste('writeLines("./svm_perf_classify ', TsFileNeg1 ,' ',trvl_model_sim_path,'/final_model ',ts_result_sim_path,'/pred_neg',i,'.csv",out)',sep='')))
        close(out)
        
        # Predict the model
        eval(parse(text=paste('shell("',cygwin_path,'/bin/dos2unix.exe /cygdrive/',unix_path,'/final_model_neg',i,'.txt",intern=T)',sep='')))
        eval(parse(text=paste('pred <- shell("',cygwin_path,'/bin/bash.exe --login -i /cygdrive/',unix_path,'/final_model_neg',i,'.txt",intern=T)',sep='')))
        
        # Read the Predictive Score
        eval(parse(text=paste('predscore_neg <- read.csv("',ts_result_sim_path,'/pred_neg',i,'.csv",header=F)',sep='')))
        UnTSCIndex <- which(predscore_neg > 0)
        selscores <- predscore_neg[UnTSCIndex,]  
        IndValMat <- cbind(UnTSCIndex,formatC(selscores,digits=3,format="f"))
        colnames(IndValMat)<-c('Indices', 'Score')
        
        # Output prospective TSC patients
        if(invalid(UnTSCIndex)){
            UnTSCIndex <- ''
        }
        
        if (length(UnTSCIndex) > 0) {
            write.csv(IndValMat,file = TSCIndexFile1,quote=FALSE,row.names = FALSE)
        }
        
        predscore_neg_all <- rbind(predscore_neg_all, predscore_neg)
        
    }
    
    NumDiag <- nrow(predscore_pos)
    TsPosTarget <- rep(1, NumDiag)
    NumUnDiag <- nrow(predscore_neg_all)
    TsNegTarget <- rep(0,NumUnDiag) 
    
    predscore_ts <- rbind(predscore_pos,predscore_neg_all)
    TsTarget  <- c(TsPosTarget,TsNegTarget)
    predobj <- prediction(predscore_ts,TsTarget)
    
    ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
    
    FP <- sum(predscore_ts > 0 & TsTarget == 0) # False Positive
    TP <- sum(predscore_ts > 0 & TsTarget == 1) # True Positive
    Recall <- TP/50
    PPV <- TP/(TP+FP)
    
    ResMat <- c(counter, round(ROCAUC*100, 2), FP, TP, round(Recall*100, 2),round(PPV*100, 2))
    result <- list(Ts_ResMat=ResMat, Vl_ResMat= valid_perf, Tr_ResMat = trainResMat)  
    return(result)
}

# Parallel run
num_cpus <-as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
sfInit(parallel=T,cpus=num_cpus) 

sfExport("NumSplit","ThreshLim","NumSimulation","cygwin_path","Ext","input_path","svmperf_path","svm_option","output_path_mod", "output_path_rs","output_path_ind")
sfClusterEval(library("e1071"))
sfClusterEval(library("ROCR"))
sfClusterEval(library("pracma"))
sfClusterEval(library("gtools"))
sfClusterEval(library("MASS"))
start=Sys.time()  
Output<-sfClusterApplyLB(1:NumSimulation,Max_Rec,Trail, cvec)
end=Sys.time() 
end-start
sfStop()

Ts_ResMat <- numeric()
Vl_ResMat <- numeric()
Tr_ResMat <- numeric()
for (i in 1: NumSimulation){
    
    Ts_ResMat <- rbind(Ts_ResMat, Output[[i]]$Ts_ResMat)
    Vl_ResMat <- rbind(Vl_ResMat, Output[[i]]$Vl_ResMat)
    Tr_ResMat <- rbind(Tr_ResMat, Output[[i]]$Tr_ResMat)
}

mean_ts <- round(apply(Ts_ResMat, 2, mean), 2)
mean_vl <- round(apply(Vl_ResMat, 2, mean), 2)
Tr_sim <- rep(1:10,each=3)

Ts_ResMat <- rbind(Ts_ResMat, mean_ts)
Vl_ResMat <- rbind(Vl_ResMat, mean_vl)

colnames(Ts_ResMat) <- c('Simulation', 'AUC', 'FP', 'TP', 'Recall', 'PPV')
colnames(Vl_ResMat) <- c('Simulation', 'AUC', 'FP','TP', 'Recall','PPV', 'selected C')
colnames(Tr_ResMat) <- c('Simulation', 'AUC', 'FP','TP', 'Recall','PPV', 'selected C')

sel_tr <- numeric()
for (i in 1:10){
    
    temp <- Tr_ResMat[Tr_ResMat[,1]==i & Tr_ResMat[,7] == Vl_ResMat[Vl_ResMat[,1]==i,7],]
    sel_tr <- rbind(sel_tr, temp)
}

mean_sel_tr <- round(apply(sel_tr, 2, mean), 2)
sel_tr  <- rbind(sel_tr, mean_sel_tr)
colnames(sel_tr) <- c('Simulation', 'AUC', 'FP','TP', 'Recall','PPV', 'selected C')

write.csv(Ts_ResMat,paste(out_path, '/TS_ResMat_trail_',Trail,'.csv', sep=''), quote=F, row.names=F)
write.csv(Vl_ResMat,paste(out_path, '/Vl_ResMat_trail_',Trail,'.csv', sep=''), quote=F, row.names=F)
write.csv(Tr_ResMat,paste(out_path, '/Tr_ResMat_trail_',Trail,'.csv', sep=''), quote=F, row.names=F)
write.csv(sel_tr,paste(out_path, '/sel_Tr_ResMat_trail_',Trail,'.csv', sep=''), quote=F, row.names=F)


# pre-rec curve
Trail <- "2_0.25_maxRecall"
score_path <- paste('D:\\jzhao\\RareDisease_TSC\\Max_recall\\03_model\\Rs\\result_trail_', Trail, "\\ts_rs", sep='')
outpath <- paste(score_path, '\\curve', sep='')

if(!file.exists(outpath)){
    dir.create(outpath, recursive=T, showWarnings=T)
    setwd(outpath)
    
}else{
    setwd(outpath)
    
}

getBucket <- function(n.bucket, score, nonTSCNum, target){
    predobj <- prediction(score,c(rep(1, 50), rep(-1, nonTSCNum)))
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
    rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
    irecall_atTgt <- unlist(lapply(target, function(Tgt){
        idx <- which(abs(Tgt-rec_prec_byBucket[,3])==min(abs(Tgt-rec_prec_byBucket[,3]), na.rm=T))
        recall_atTgt <- rec_prec_byBucket[idx, 2]
        return(recall_atTgt)
    }))
    
    return(irecall_atTgt)
}

find_perfMeasure <-  function(n.bucket, target){
    bkt_list <- lapply(1:10, function(i){
        score_path <- paste('D:\\jzhao\\RareDisease_TSC\\Max_recall\\03_model\\Rs\\result_trail_', Trail, "\\ts_rs\\ts_result_simulation_", i, sep='')
        file_list <-  list.files(path=score_path, all.files=F, full.names=T, recursive=F)
        neg_file_list <- grep("neg", file_list, value=T, perl=T)
        pos_file_list <- grep('pos', file_list, value=T, perl=T)
        neg_score <- unlist(lapply(neg_file_list, function(Fn){
            read.csv(Fn, header=FALSE)[,1]
        }))
        pos_score <- read.csv(pos_file_list, header=FALSE)[, 1]
        score <- c(pos_score, neg_score)
        nonTSCNum <- length(neg_score)
        bkt <-getBucket(n.bucket, score, nonTSCNum)
        return(bkt)
    })
    
    library(plyr)
    bkt_df <- ldply(bkt_list, quickdf)
    bkt_final <- aggregate(bkt_df[, -1], by=list(bkt_df[, 1]), mean)
    write.csv(bkt_final, "Precision-Recall Curve.csv", row.names=F)
    names(bkt_final)
    recall_atTgt <- unlist(lapply(target, function(Tgt){
        idx <- which(abs(Tgt-bkt_final[,3])==min(abs(Tgt-bkt_final[,3]), na.rm=T))
        recall_atTgt <- bkt_final[idx, 2]
        return(recall_atTgt)
    }))
    return(list(Points=recall_atTgt, curve=bkt_final))
}
target=c(0.25, 0.65, 0.75)

measure <- find_perfMeasure(100, target)








