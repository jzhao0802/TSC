
################################################################################################## 
# Prstst.R                                                                                       #      
# Description: It generates feature vectors for non-TSC patients in test set. Data can be        #
#              processed in parallel or in sequential mode                                       #
#                                                                                                #
# Input variables :                                                                              #
#            innerloop : Iterator                                                                #
#                                                                                                #         
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################

#innerloop<- 1

PrsTst <- function(innerloop) {
  
  cat(paste("Iteration ", innerloop), sep="\n")
  
  subfindex <- innerloop
  
  TsIndexFile <- paste(TsIndexFile,subfindex,Ext,sep="") 

  CurIdx <- unlist(IdxAry[[subfindex]])
  
  write.table(CurIdx,file=TsIndexFile,quote=FALSE,col.names=FALSE,row.names=FALSE,append=TRUE) # write indices to a file
  
  TsNonTSCAgeGender <- lapply(X=1:1:length(CurIdx), function(X) NonTSCAgeGender[ which(NonTSCAgeGender$Id %in% CurIdx[X]), ] )
  
  TsNonTSCCodeDrug <- lapply(X=1:1:length(CurIdx), function(X) NonTSCCodeDrug[ which(NonTSCCodeDrug$V1 %in% CurIdx[X]), ] )
  
  
##################################################################################################
  
  # Normalise Age for non-TSC patients in test set
  
  TsNonTSCAgeMat <- ldply(TsNonTSCAgeGender,quickdf)
  
  TsNonTSCAgeMat <- NormAge(TsNonTSCAgeMat,MeanAgeFirst,MeanAgeSecond)  
  

##################################################################################################

# Feature Vectors for non-TSC patients in test set
  
  TsNonTSClabel <- rep(-1,length(CurIdx)) # Label vector for non-TSC patients
  
  Fname <- paste(TsFileNeg,filenum,subfindex,Ext,sep="")

  MkFtVec(TsNonTSCAgeMat,TsNonTSCCodeDrug,Dict,TsNonTSClabel,Fname)
  
  rm(list=c('TsNonTSCAgeMat','TsNonTSCCodeDrug','TsNonTSClabel'))  #modify by jzhao
}