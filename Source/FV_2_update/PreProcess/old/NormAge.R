
################################################################################################## 
# NormAge.R                                                                                      #      
# Description: It performs scaling for age variables                                             #                   
#                                                                                                #
# Input variables :                                                                              #
#            AgeMat : Age matrix                                                                 #
#            MeanAgeFirst : Mean age at first record                                             #
#            MeanAgeFirst : Mean age at last record                                              #
#                                                                                                #         
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################

NormAge <- function(AgeMat,MeanAgeFirst, MeanAgeSecond) {
                
  AgeMat[,2] <- AgeMat[,2] / MeanAgeFirst
  AgeMat[,3] <- AgeMat[,3] / MeanAgeSecond
  
  return(AgeMat)
}