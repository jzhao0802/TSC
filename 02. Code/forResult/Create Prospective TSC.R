

setwd("D:/yxue/TSC/Create Prospective TSC")

#Path to Prospective TSC
path_pros_tsc<- "D:/RareDisease_TSC/Results/FV_2_update/2n_lin_setting1_Yan/Indices/lin"

#Path to Test 
path_test_indice<- "D:\\RareDisease_TSC\\Data\\Dict_ind_2_update\\2n_Yan'sData"


Num_Sim <- 10
Num_splits <- 8

score_threshold<- 0.3

for(i in 1:Num_Sim){

  #initialization 
  if(i==1){
    
    pros_tsc_out<-NULL
    qc_length<-c()
    
  }
  
  for(j in 1:Num_splits){
  
  temp_pros_tsc_fn <- paste("ProspectiveTSC", i,  j, '.csv', sep="")
  temp_test_indice_fn <- paste("TestIndices", i, j, '.csv', sep="")
  
  temp_pros_tsc<-read.table(paste(path_pros_tsc,'/', temp_pros_tsc_fn, sep=""), header=FALSE, sep='\t')
  colnames(temp_pros_tsc)<-c("row.index","pred.score")
  
  temp_test_indice<-read.table(paste(path_test_indice,'/', temp_test_indice_fn, sep=""), header=FALSE, sep=',')
  colnames(temp_test_indice)<- "Patient_ID"
  
  temp_matched<- cbind(Sim=rep(i,times=nrow(temp_pros_tsc)), Split= rep(j,times=nrow(temp_pros_tsc)),patient_id= temp_test_indice[ temp_pros_tsc[,"row.index"] , ], temp_pros_tsc)
  
  pros_tsc_out<-rbind(pros_tsc_out,temp_matched)
  qc_length<- c(qc_length, nrow(temp_pros_tsc))
  
  } #end of loops thru Num_splits
  
  pros_tsc_out<-as.data.frame(pros_tsc_out)
  
  
}#end of loops thru Num_Sim

# colnames(pros_tsc_out)

# length(unique(pros_tsc_out[,"patient_id"]))

# nrow(pros_tsc_out)
# length(qc_length)
# sum(qc_length)
# table(qc_length)

# head(pros_tsc_out)
pros_tsc_remain<- pros_tsc_out[pros_tsc_out[,"pred.score"]>=score_threshold, c("patient_id","pred.score")]
# nrow(pros_tsc_remain)

# is.numeric(pros_tsc_remain[,1])

agg_1<-tapply(pros_tsc_remain[,2],pros_tsc_remain[,1],mean, simplify = TRUE)

deduped_pat_score<-data.frame(patient_id=as.numeric(names(agg_1)), score=agg_1)
head(deduped_pat_score)

#optional way
# library(plyr)
# agg_2<-aggregate(pros_tsc_remain[,2],by=list(pros_tsc_remain[,1]),mean)
# head(agg_2)
# colnames(agg_2)<- c("patient_id", "score")
# table(deduped_pat_score[,1]-agg_2[,1])
# table(deduped_pat_score[,2]-agg_2[,2])

write.csv(deduped_pat_score,"Prospective_TSC_Patients.csv",row.names=FALSE)

top_20_pros_TSC<- deduped_pat_score[order(deduped_pat_score[,2],na.last = TRUE, decreasing = TRUE)[1:20],]

write.csv(top_20_pros_TSC, "Prospective_TSC_Top20.csv",row.names=FALSE)


