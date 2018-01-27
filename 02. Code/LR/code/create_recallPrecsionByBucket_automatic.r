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

sfSource("D:\\jzhao\\RareDisease_TSC\\LR\\function_for_recPrecByBucket.R")


input_path <- "D:\\yxue\\TSC\\LR\\work\\PredTest\\"
#input_path <- "D:\\yxue\\TSC\\LR\\Jun18\\Output\\"
output_path <- "D:\\jzhao\\RareDisease_TSC\\LR"


#infile <- paste('PredTest_wt', wt, '_Simulation', sim, sep='')


get_recPrec_byBucket <- function(input_path, output_path, n.bucket){
    if(!file.exists(output_path)){
        dir.create(output_path, recursive=T, showWarnings=T)
        setwd(output_path)
        
    }else{
        setwd(output_path)
        
    }
    
    infileAll <- list.files(path=input_path, all.files=F, full.names=F, recursive=F)
    #PredTest_wt1_Simulation1
    wt <- gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[1], perl=T)
    wt_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+)(\\d)(\\D\\w+\\W\\w+$)", "\\2", infileAll[X], perl=T)))
    wt_level <- sort(as.numeric(levels(as.factor(wt_lst))))
    sim_lst <- unlist(lapply(X=1:length(infileAll),  function(X)gsub("(^\\w+\\D)(\\d+)(\\W\\w+$)", "\\2", infileAll[X], perl=T)))
    sim_level <- sort(as.numeric(levels(as.factor(sim_lst))))
    wt_level <- '1'
    for (w in wt_level){
        #files <- eval(parse(text=paste("grep('\\\\w+", w, "\\\\w', infileAll, perl=T, value=T)", sep='')))
        #files <- eval(parse(text=paste('grep("\\\\w+', w, '\\\\w", infileAll, perl=T, value=T)', sep='')))
		files <- eval(parse(text=paste("grep('\\\\w+_\\\\w{2}", w, "_\\\\w+\\\\d', infileAll, perl=T, value=T)", sep='')))
        
        #num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
        #sfInit(parallel=TRUE, cpus=num_pros, type="SOCK")
        #sfLibrary(snowfall)
        #sfExport('get_recPrec_perSim', "input_path", "files")
        #sfClusterEval(library("e1071"))
        #rec_prec_idx_lst <- sfClusterApplyLB(files,get_recPrec_perSim,input_path)
        
        #sfStop()
		f <- files[1]
		#score <- lapply(files, function(X){read.table(paste(input_path, X, sep=''), sep=',', header=T)[, 2]})
		#score_allSim <-as.data.frame(score)
		rec_prec_byBucket_rbind <- numeric()
		recPrec <- lapply(files, function(f){
											score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
										    predobj <- prediction(score,c(rep(1, 50), rep(-1, length(score)-50)))
											#add plot
											perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
											#plot(perf, main=paste('simulation:', i, sep=''))
											recall <- perf@x.values[[1]]
											precision <- perf@y.values[[1]]
											rePrec <- cbind(recall, precision)
											bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
											rec_prec <- cbind(recall, precision)
											rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
											#rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, rec_prec_byBucket)
											return(rec_prec_byBucket)
											})
											
		for(i in 1: length(recPrec)){
			rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, recPrec[[i]])
		}
		
        rec_prec_result <- aggregate(rec_prec_byBucket_rbind[, -1], by=list(rec_prec_byBucket_rbind[, 1]), function(i)mean(i, na.rm=T))
		#plot
		pdf(file=paste('recall-precision curve by bucket', n.bucket, '_wt', w, '.pdf', sep=''))
		recall <- rec_prec_result[, 2]
		precision <- rec_prec_result[, 3]
		plot(recall, precision, type='l', main=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
		dev.off()
		
		write.csv(rec_prec_result, paste('LR_rec_prec_byBucket_wt', w, '.csv', sep=''), row.names=F, quote=F)
		return(rec_prec_result)

        
    }
}

#input_path <- 'D:\\yxue\\TSC\\LR\\Jun18\\Output Jul24 Final\\'
input_path <- 'D:\\yxue\\TSC\\LR\\Jun18\\Output Jul07\\' #combination prediction set
#input_path <- 'D:\\yxue\\TSC\\LR\\Jun18\\Output Jul07\\Forth LR\\' #Firth prediction set


#output_path <- "D:\\jzhao\\RareDisease_TSC\\LR\\Jul24"
output_path <- "D:\\jzhao\\RareDisease_TSC\\LR\\Jul29" #combination prediciton set
output_path <- "D:\\jzhao\\RareDisease_TSC\\LR\\Nov05"

    if(!file.exists(output_path)){
        dir.create(output_path, recursive=T, showWarnings=T)
        setwd(output_path)
        
    }else{
        setwd(output_path)
        
    }
# part1: AUC+recall on precision(0.05 0.1 0.25 0.5)
create_recallPrecision <- function(dataset){
    #wt_list <- c(1, 2, 10, 20)
    wt_list <- 1
    auc_allWt <- numeric()
    recall_allWt <- numeric()
    for(wt in wt_list){
        
        if(dataset=='comb'){
            score <- read.table(paste(input_path, '\\LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else if(dataset=='sep'){
            score <- read.table(paste('D:\\yxue\\TSC\\LR\\Jun18\\Output Jul24 Final', '\\LR_AgeManif_Sep_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')
        }else if(dataset=='firth'){
            score <- read.table(paste(input_path, 'Forth LR\\Firth_LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else{
            stop('please check the parameter!\n')
        }
        
        
        recPrec <- lapply(1:ncol(score), function(X){
            #score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
            predobj <- prediction(score[, X],c(rep(1, 50), rep(-1, nrow(score)-50)))
            #add plot
            perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
            #plot(perf, main=paste('simulation:', i, sep=''))
            recall <- perf@x.values[[1]]
            precision <- perf@y.values[[1]]
            rePrec <- cbind(recall, precision)
            bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
            rec_prec <- cbind(recall, precision)
            rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
            #rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, rec_prec_byBucket)
            ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
            return(list(rec_prec_byBucket, ROCAUC))
        })
        auc_all <- unlist(lapply(1:length(recPrec), function(X){recPrec[[X]][[2]]}))
        auc_mean <- mean(auc_all)
        auc_allWt <- c(auc_allWt, auc_mean)
        rec_prec_byBucket_rbind <- numeric()									
        for(i in 1: length(recPrec)){
            rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, recPrec[[i]][[1]])
        }
        
        rec_prec_result <- aggregate(rec_prec_byBucket_rbind[, -1], by=list(rec_prec_byBucket_rbind[, 1]), function(i)mean(i, na.rm=T))
        #plot
        #pdf(file=paste('recall-precision curve by bucket', n.bucket, '_wt', w, '.pdf', sep=''))
        recall <- rec_prec_result[, 2]
        precision <- rec_prec_result[, 3]
        #plot(recall, precision, type='l', main=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
        #dev.off()
        
        write.csv(rec_prec_result, paste(toupper(dataset),'_LR_rec_prec_byBucket_wt', wt, '.csv', sep=''), row.names=F, quote=F)
        tar_recall_all <- numeric()
        for (target_prec in c(0.05, 0.10, 0.25, 0.50)){
            diff <- abs(precision-target_prec)
            target_recall <- recall[which(diff==min(diff, na.rm=T))]
            tar_recall_all <- c(tar_recall_all, target_recall)
        }
        #recall1 <- cbind(target_prec=c(5,10, 25, 50), target_recall = tar_recall_all)
        recall1 <- tar_recall_all
        recall_allWt <- rbind(recall_allWt,recall1)
        
    }
    auc_allWt_final <- cbind(wt_list, auc_allWt)
		
		return(list(recall=recall_allWt, auc=auc_allWt_final))

}
par1_result <- create_recallPrecision('sep')
par1 <- cbind(par1_result[[2]], par1_result[[1]])
colnames(par1) <- c( 'Wt', 'AUC', 'Recall at 5% precision',     'Recall at 10% precision',	'Recall at 25% precision',	'Recall at 50% precision')

# part2: recall  specialty  precsion
create_rate <- function(dataset, cutoff){
    rate_mean_all <- numeric()
    for(wt in c(1, 2, 10, 20)){
        
        if(dataset=='comb'){
            score <- read.table(paste(input_path, '\\LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else if(dataset=='sep'){
            score <- read.table(paste('D:\\yxue\\TSC\\LR\\Jun18\\Output Jul24 Final', '\\LR_AgeManif_Sep_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')
        }else if(dataset=='firth'){
            score <- read.table(paste(input_path, 'Forth LR\\Firth_LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else{
            stop('please check the parameter!\n')
        }
        cutoff <- cutoff
        rate <- lapply(1:ncol(score), function(X){
            #score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
            TP <- sum(score[1:50, X]>=cutoff)
            FP <- sum(score[-(1:50), X] >= cutoff)
            TN <- sum(score[-(1:50), X] < cutoff)
            
            Recall <- TP/50
            Precision <- TP/(TP+FP)
            Specificity <- TN/(FP+TN)
            rate <- c(Recall, Specificity, Precision)
            return(rate)
        })
        rate_all <- as.data.frame(rate)
        rate_mean <- apply(rate_all, 1, mean)
        rate_mean_all <- rbind(rate_mean_all, rate_mean)
        
    }
    rate_final <- cbind(c(1, 2, 10, 20), rate_mean_all)
    
    return(rate_final)
    
}
rate_final <- create_rate('firth', 0.5)
colnames(rate_final) <- c('Wt', 'Recall for pos / neg (0.5 for LR) cut-off',    'Specifivity for pos / neg (0.5 for LR) cut-off',	'Precision for pos / neg (0.5 for LR)  cut-off')
par2 <- rate_final[, -1]
# part3: extra 3 columns (precision pos_num F1)
create_recallPrecision_addFlaggedPatient <- function(dataset, tar_recall){
    wt_list <- c(1, 2, 10, 20)
    target_recall_list <- tar_recall
    
    pos_num_allWt <- numeric()
    auc_allWt <- numeric()
    for(wt in wt_list){
        if(dataset=='comb'){
            score <- read.table(paste(input_path, '\\LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else if(dataset=='sep'){
            score <- read.table(paste('D:\\yxue\\TSC\\LR\\Jun18\\Output Jul24 Final', '\\LR_AgeManif_Sep_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')
        }else if(dataset=='firth'){
            score <- read.table(paste(input_path, 'Forth LR\\Firth_LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else{
            stop('please check the parameter!\n')
        }
        
        recPrec <- lapply(1:ncol(score), function(X){
            #score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
            predobj <- prediction(score[, X],c(rep(1, 50), rep(-1, nrow(score)-50)))
            #add plot
            perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
            #plot(perf, main=paste('simulation:', i, sep=''))
            recall <- perf@x.values[[1]]
            precision <- perf@y.values[[1]]
            rePrec <- cbind(recall, precision)
            bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
            rec_prec <- cbind(recall, precision)
            rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
            #rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, rec_prec_byBucket)
            ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
            write.csv(rec_prec_byBucket, paste(toupper(dataset),'_LR_rec_prec_byBucket_wt', wt, '_sim', X, '.txt', sep=''), row.names=F, quote=F)
            return(list(rec_prec_byBucket, ROCAUC))
        })
        auc_all <- unlist(lapply(1:length(recPrec), function(X){recPrec[[X]][[2]]}))
        auc_mean <- mean(auc_all)
        auc_allWt <- c(auc_allWt, auc_mean)
        rec_prec_byBucket_rbind <- numeric()    								
        for(i in 1: length(recPrec)){
            rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, recPrec[[i]][[1]])
        }
        
        rec_prec_result <- aggregate(rec_prec_byBucket_rbind[, -1], by=list(rec_prec_byBucket_rbind[, 1]), function(i)mean(i, na.rm=T))
        #plot
        #pdf(file=paste('recall-precision curve by bucket', n.bucket, '_wt', w, '.pdf', sep=''))
        #recall <- rec_prec_result[, 2]
        #precision <- rec_prec_result[, 3]
        #plot(recall, precision, type='l', main=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
        #dev.off()
        
        #write.csv(rec_prec_result, paste('LR_rec_prec_byBucket_wt', wt, '.csv', sep=''), row.names=F, quote=F)
        rec <- rec_prec_result[, 2]
        temp1 <- numeric()
        for (target_recall in target_recall_list){
            tar_idx <- which(abs(rec-target_recall)==min(abs(rec-target_recall)))
            prec <- rec_prec_result[tar_idx, 3]
            pos_num <- 50*target_recall/prec
            F_score <- 2*target_recall*prec/(target_recall+prec)
            temp1 <- rbind(temp1, c(prec, pos_num, F_score))
        }
        pos_num_allWt <- rbind(pos_num_allWt, temp1)
    }
    pos_num_allWt_table <- cbind(wt_list, pos_num_allWt)
    
    write.csv(pos_num_allWt_table, paste('extra_3columnsForTable4_LR_', toupper(dataset), '.csv', sep=''), row.names=F)
    
    return(pos_num_allWt_table)
    
}
extra_table4_LR_comb <- create_recallPrecision_addFlaggedPatient('comb', c(0.5))
extra_table4_LR_sep <- create_recallPrecision_addFlaggedPatient('sep',c(0.5))
extra_table4_LR_firth <- create_recallPrecision_addFlaggedPatient('firth',c(0.5))
colnames(extra_table4_LR_firth) <- c('Wt', 'Precision associated with 50% recall',     'Number of patients flagged to identify 25 TSC patients',	'F1')
par3 <- extra_table4_LR_firth[, -1]


summaryTable4_LR_Firth <- cbind(par1, par2, par3)

write.csv(summaryTable4_LR_Firth, 'summaryTable4_LR_Firth.csv', row.names=F)







#generate the precision curve for each simulation for comb LR
create_recallPrecision <- function(dataset){
    #wt_list <- c(1, 2, 10, 20)
    wt_list <- 1
    auc_allWt <- numeric()
    recall_allWt <- numeric()
    for(wt in wt_list){
        
        if(dataset=='comb'){
            score <- read.table(paste(input_path, '\\LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else if(dataset=='sep'){
            score <- read.table(paste('D:\\yxue\\TSC\\LR\\Jun18\\Output Jul24 Final', '\\LR_AgeManif_Sep_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')
        }else if(dataset=='firth'){
            score <- read.table(paste(input_path, 'Forth LR\\Firth_LR_AgeManif_Comb_Pred_on_Test_wt(', wt, ').csv', sep=''), header=T, sep=',')            
        }else{
            stop('please check the parameter!\n')
        }
        
        
        recPrec <- lapply(1:ncol(score), function(X){
            #score <- read.table(paste(input_path, f, sep=''), sep=',', header=T)[, 2]
            predobj <- prediction(score[, X],c(rep(1, 50), rep(-1, nrow(score)-50)))
            #add plot
            perf <- performance(predobj, 'prec', 'rec') # added by jie for recall-precision plot.
            #plot(perf, main=paste('simulation:', i, sep=''))
            recall <- perf@x.values[[1]]
            precision <- perf@y.values[[1]]
            rePrec <- cbind(recall, precision)
            bucket <- cut(recall, breaks=seq(0, 1, 0.01), include.lowest=T,right=F)
            rec_prec <- cbind(recall, precision)
            rec_prec_byBucket <- aggregate(rec_prec, by=list(bucket), function(i)mean(i, na.rm=T))
            #rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, rec_prec_byBucket)
            ROCAUC <- performance(predobj, measure = "auc")@y.values[[1]]
            return(list(rec_prec_byBucket, ROCAUC))
        })
        auc_all <- unlist(lapply(1:length(recPrec), function(X){recPrec[[X]][[2]]}))
        auc_mean <- mean(auc_all)
        auc_allWt <- c(auc_allWt, auc_mean)
        rec_prec_byBucket_rbind <- numeric()    								
        for(i in 1: length(recPrec)){
            rec_prec_byBucket_rbind <- rbind(rec_prec_byBucket_rbind, recPrec[[i]][[1]])
        }
        
        rec_prec_result <- aggregate(rec_prec_byBucket_rbind[, -1], by=list(rec_prec_byBucket_rbind[, 1]), function(i)mean(i, na.rm=T))
        #plot
        #pdf(file=paste('recall-precision curve by bucket', n.bucket, '_wt', w, '.pdf', sep=''))
        recall <- rec_prec_result[, 2]
        precision <- rec_prec_result[, 3]
        #plot(recall, precision, type='l', main=paste('recall-precision curve by bucket', n.bucket, '.pdf', sep=''))
        #dev.off()
        
        write.csv(rec_prec_result, paste(toupper(dataset),'_LR_rec_prec_byBucket_wt', wt, '.csv', sep=''), row.names=F, quote=F)
        tar_recall_all <- numeric()
        for (target_prec in c(0.05, 0.10, 0.25, 0.50)){
            diff <- abs(precision-target_prec)
            target_recall <- recall[which(diff==min(diff, na.rm=T))]
            tar_recall_all <- c(tar_recall_all, target_recall)
        }
        #recall1 <- cbind(target_prec=c(5,10, 25, 50), target_recall = tar_recall_all)
        recall1 <- tar_recall_all
        recall_allWt <- rbind(recall_allWt,recall1)
        
    }
    auc_allWt_final <- cbind(wt_list, auc_allWt)
    
    return(list(recall=recall_allWt, auc=auc_allWt_final))
    
}




