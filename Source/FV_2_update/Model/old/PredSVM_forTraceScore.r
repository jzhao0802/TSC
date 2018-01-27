
################################################################################################## 
# PredSVM.R                                                                                      #
# Description: Evaluates model on test data                                                      #
#                                                                                                #
# Input Variables:                                                                               #
#               innerloop: Iterator                                                              #
#               classifier is trained                                                            #
#               ThreshLim: Upper limit for undiagnosed patients to be identified as TSC patients,#
#               default: 50                                                                      #
#               kertype: Kernel function to be used in conjunction with SVMs                     #
#               TrFile1, VlFile, TrFile : Input files for training (1), validation,              #
#               training(training TSC + validation TSC) sets                                    #
#               TrRsFile : Output file for saving results on training data                       #
#              
#                                                                                                # 
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################


PredSVM <- function(innerloop, TsFileNeg, TSCIndexFile, Ext) {
  
  subfindex <- innerloop
  
  TSCIndexFile <- paste(TSCIndexFile,subfindex,Ext,sep="")
  
  Fname1 <- paste(TsFileNeg,subfindex,Ext,sep="")

  testneg <- read.matrix.csr(Fname1,fac=TRUE)
  
  pred <- predict(svm_model,testneg$x,sclae=FALSE,decision.values = TRUE)
  upredscore <- attr(pred, "decision.values")
  #UnTSCIndex <- which(upredscore > 0)
  #selscores <- upredscore[UnTSCIndex]  
  IndValMat <- formatC(upredscore,digits=3,format="f")

  #if (length(UnTSCIndex) > 0) {
    #write.table(IndValMat,file = TSCIndexFile, sep = "\t",quote=FALSE,row.names = FALSE, col.names = FALSE)
  #}
  
  return(IndValMat)

}
  