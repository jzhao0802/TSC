
################################################################################################## 
# MkFtVec.R                                                                                      #      
# Description: It generates feature vectors for patients                                         #                   
#                                                                                                #
# Input variables :                                                                              #
#            PtsAgeGender : Age and gender data                                                  #
#            PtsCodeDrug : Codes and drugs data                                                  #
#            Dict : Codes and drugs dictionary                                                   #
#            LabelVec : Label vector                                                             #
#            Fname : File to store vectors                                                       #
#                                                                                                #         
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################
 

MkFtVec <- function(PtsAgeGender,PtsCodeDrug,Dict,LabelVec,Fname) {
  
  PtId <- unique(PtsAgeGender$Id) 
  
  NumPts <- length(PtId)

  Dim <- length(Dict$Indices)
  
  lastDim <- Dim + 1 + 5
  
  zero <- ":0"
  agindex <- c(1,2,3,4,5)
  coln <- ":"
  
  lastDimSt <- noquote(paste(lastDim,zero,sep=""))
  
  PtbyCodeDrugLst <- lapply(X=1:NumPts,function(X) which(Dict[["Indices"]] %in%  PtsCodeDrug[[X]][2:length(PtsCodeDrug[[X]])]))
  
  for (Y in 1:NumPts) {    
    sfCat(".")
    
    cat(file=Fname,append=TRUE,LabelVec[Y])
    cat(file=Fname,append=TRUE," ")
    
    temp1 <- PtsAgeGender[Y,2:6]
    
    AgeSum <- sum(PtsAgeGender[Y,2:6]^2)

    CodeDrugSum <- length(PtbyCodeDrugLst[[Y]])
    
    Pnorm <- sqrt(AgeSum + CodeDrugSum)
    
    if (norm2) {
      temp1 <-  (temp1/Pnorm)
    }
        
    temp2 <- noquote(paste(agindex,coln,temp1,sep=""))
    
    cat(temp2,file=Fname,append=TRUE) # write age and gender features

    if (norm2) {
      Val <- 1.0/Pnorm
    }
    
    if (!norm2) {
      Val <- 1.00
    }
      
    if (length(PtbyCodeDrugLst[[Y]]) > 0) {  
      
      cat(file=Fname,append=TRUE," ")
    
      temp3 <- PtbyCodeDrugLst[[Y]]
      temp3 <- temp3+5
     
      temp4 <- noquote(paste(temp3,coln,Val,sep=""))
      
      cat(temp4,file=Fname,append=TRUE) # write codes and drugs features
      
    }
    
   cat(file=Fname,append=TRUE," ")    
   cat(lastDimSt,"\n",file=Fname,append=TRUE)
   
  }
  
  rm(list=c('PtbyCodeDrugLst'))
  return()
  
}

