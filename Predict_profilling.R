library(xlsx)
library(dplyr)
library(plyr)
inPath <- "D:\\yxue\\Brillinta\\prediction for Jie\\"
outPath <- "D:\\yxue\\Brillinta\\prediction for Jie\\predict_result\\"
setwd(outPath)

#get frequency of mosaic variable
fileN <- c('RF_switch_model_prediction_Oct22_S497', 
          # 'RF_nospillover_model_prediction_Oct22_S7856', 
           "RF_nospillover_model_prediction_Oct28_Seed7856")
f=2
raw_data <- read.table(paste(inPath, fileN[f], '.csv', sep=''), sep=',', header=T, stringsAsFactors=F) #[1] 4157   75 for switch
names(raw_data) <- tolower(names(raw_data))
freq <- table(raw_data$mosaic)
leraw_data_tarngth(table(raw_data$mosaic)) #20
perc <- table(raw_data$mosaic)/nrow(raw_data)
freq_perc1 <- cbind(Freq=freq, Perc=perc)
freq_perc <- rbind(freq_perc1, Total=c(nrow(raw_data), 1))
sheetN <- gsub('(RF_)(\\w+)(_model\\w+)', '\\2', fileN, perl=T)
write.xlsx(freq_perc, 'freq_for_mosaic_oct28.xlsx', sheetName=sheetN[f], row.names=T,append=T)


#generate the profiling of patients for both the 2 markets 
temp1 <- list.files(path=paste(inPath, 'perf\\', sep=''), full.names=T)
#impFList <- grep('import', temp1, ignore.case=T, value=T)
impFList <- grep('\\\\RF\\w+oct28', temp1, ignore.case=T, value=T)

impDt <- read.table(grep(sheetN[f], impFList, perl=T, value=T), sep=',', header=T, stringsAsFactors=F)
tar_covar <- tolower(impDt[1:10, 1])

raw_data_tar <- raw_data[, match(tar_covar, names(raw_data))]
sapply(raw_data_tar, function(x)length(table(x)))
num_flag <- sapply(raw_data_tar, is.numeric)
conti_var <- tar_covar[num_flag]
cat_var <- tar_covar[!num_flag]
#[1] "mosaic"          "patient_age"     "income_grp"      "education_level" for spillover
#generate dummy variables for catigorical variables

if(f==1){
    dummy <- model.matrix(~ mosaic + specialty, data=raw_data_tar, 
                          contrasts.arg=list(mosaic=contrasts(as.factor(raw_data_tar$mosaic), contrasts=F), 
                                             specialty=contrasts(as.factor(raw_data_tar$specialty), contrasts=F)))[, -1]
    
}
if(f==2){
    dummy <- model.matrix(~ mosaic + patient_age + income_grp + education_level, data=, 
                          contrasts.arg=list(mosaic=contrasts(as.factor(raw_data_tar$mosaic), contrasts=F), 
                                             patient_age=contrasts(as.factor(raw_data_tar$patient_age), contrasts=F),
                                             income_grp=contrasts(as.factor(raw_data_tar$income_grp), contrasts=F),
                                             education_level=contrasts(as.factor(raw_data_tar$education_level), contrasts=F)
                          ))[, -1] #[1] 4157   38 for spillover
    
}

#test the dummy creation ---YES!!!!!!!!
data_forDummy <- raw_data_tar[, cat_var]
sapply(as.data.frame(data_forDummy),  class)
data_forDummy_fct <- sapply(as.data.frame(data_forDummy),  as.factor) #YES!!!!!!!!!
data_forDummy_fct <- sapply(as.data.frame(data_forDummy),  type.convert(.)) #YES!!!!!!!!!

sapply(as.data.frame(data_forDummy_fct),  class)
contrasts.arg=lapply(cat_var, function(v){
    assign(paste("var_", v, sep=''), contrasts(as.factor(data_forDummy[, v]), contrasts=F))
})
#or
contrasts.arg=lapply(data_forDummy, contrasts, contrasts=F)
names(contrasts.arg)<- cat_var
dummy_test <- model.matrix(~., data=data_forDummy, 
                           contrasts.arg=contrasts.arg)[, -1]

raw_data_forProfile <- cbind(raw_data_tar[, conti_var], dummy) #[1] 4157   78 for switch
#raw_data_forProfile=raw_data_tar[, conti_var]
#get the response and predict probability on test data
raw_data_forProfile <- cbind(raw_data_tar[, conti_var], dummy) #[1] 4157   78 for switch
test_flag <- raw_data$train_test==0
qtl <- quantile(raw_data$prediction[test_flag], probs=seq(0, 1, length.out=4))
bucket <- cut(raw_data$prediction[test_flag], breaks=c(qtl), include.lowest=T,right=F, labels=1:3)
tarCovarProfile <- aggregate(raw_data_forProfile[test_flag, ], by=list(bucket), mean, rm.na=T)
respPredProfile <- aggregate(raw_data[test_flag, c('response', 'prediction')], by=list(bucket), mean, rm.na=T)
n_pat <- table(as.numeric(as.character(bucket)))
profile <- as.data.frame(t(cbind(cbind(unname(n_pat), respPredProfile)[, -c(1, 3)], tarCovarProfile[, -1])))
names(profile) <- c('Low', 'Medium', 'High')
rownames(profile)[1] <- 'Number of Patients'
write.xlsx(profile, 'Profile_result_Oct28.xlsx', sheetName=sheetN[f], append=T, row.names=T )

#update the grouping stat method
profile_result_2 <- cbind(bucket=bucket, raw_data_forProfile[test_flag, ]) %>% group_by(bucket) %>% summarize_each(funs(mean))
names(raw_data_forProfile) <- gsub('\\s+', '_', names(raw_data_forProfile))
profile_result_3 <- cbind(bucket=bucket, raw_data_forProfile[test_flag, ]) %>% group_by(bucket) %>% summarize_each_(funs(mean), vars=names(raw_data_forProfile))





#            bucket
[1,] 0.6500      3
[2,] 0.1028      1
[3,] 0.4104      2
[4,] 0.6172      3
[5,] 0.8044      3
[6,] 0.8084      3



