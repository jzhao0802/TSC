
################################################################################################## 
# MkDictEnvt.R                                                                                   #      
# Description: It creates a create a dictionary of unique codes appearing in training set        #                   
#                                                                                                #
# Input variables :                                                                              #
#            PtsCodeDrug : Codes and drugs                                                       #
#            dictFile_ : File to store dictionary                                                #
# Other variables  :                                                                             #
#            tempCodeDrug, CodeDrugDict, DictElem,  Dict, Indices, tIndices: Dictionary related  #                                   
#            variables                                                                           #
#                                                                                                #         
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################

#PtsCodeDrug <- TrPtsCodeDrug
MkDictEnv <- function(PtsCodeDrug,dictFile_) {
    
  tempCodeDrug <- lapply(X=1:length(PtsCodeDrug),function(X) PtsCodeDrug[[X]][-1])
  
  CodeDrugDict <- unique((unlist(tempCodeDrug)))  #5207
  
  bindex <- which(CodeDrugDict == "-")
  if(bindex !=0){
    CodeDrugDict <- CodeDrugDict[-bindex]  #5206
    
  }
    DictElem <- length(CodeDrugDict)
    Indices <- c(1:DictElem)   #1:5206
    tIndices <- c(6:(DictElem+5))   #6:5211
    
  
  write.table(data.frame(tIndices,CodeDrugDict),file=dictFile_)
  
  Dict <- new.env(hash=TRUE)
  
  Dict$Indices <- CodeDrugDict

  rm(CodeDrugDict)
  
  return(Dict)
  
}

