
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


MkDictEnv <- function(PtsCodeDrug,dictFile_) {
    
  tempCodeDrug <- lapply(X=1:length(PtsCodeDrug),function(X) PtsCodeDrug[[X]][-1])
  
  CodeDrugDict <- unique((unlist(tempCodeDrug)))
  
  bindex <- which(CodeDrugDict == "-")
  CodeDrugDict <- CodeDrugDict[-bindex]
    
  DictElem <- length(CodeDrugDict)
  Indices <- c(1:DictElem)  
  tIndices <- c(6:(DictElem+5)) 
  
  write.table(data.frame(tIndices,CodeDrugDict),file=dictFile_)
  
  Dict <- new.env(hash=TRUE)
  
  Dict$Indices <- CodeDrugDict

  rm(CodeDrugDict)
  
  return(Dict)
  
}

