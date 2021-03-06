
################################################################################################## 
# main.R                                                                                         #
# Description: Main program for pre-processing library.                                          #
#              It declares and initialises variables, sets flag for performing parallel or       #
#              sequential processing, and invokes dependent functions                            #
# Variables:                                                                                     #
#               inFile1TSC: Input file containg age and gender information for TSC patients      #
#               inFile2TSC: Input file containg READ, ICD-10, OPCS codes and drugs               #
#               information for TSC patients                                                     #
#               inFile1NonTSC: Input file containg age and gender information for non-TSC        #
#               patients                                                                         #
#               inFile2NonTSC: Input file containg READ, ICD-10, OPCS codes and drugs            #
#               information for non-TSC patients                                                 #
#               dictFile: Ouput file for storing dictionary                                      #
#               TrIndexFile,VlIndexFile, TsIndexFile: Output files for storing training, test    #
#               and validation  indices for non-TSC patients                                     #
#               TrFile1, VlFile, TrFile : Output files for saving training (1), validation,      #
#               training(trainging TSC + validation TSC) sets                                    #
#               TsFilePos, TsFileNeg : Output files for saving test TSC and test non-TSC         #
#                patients respectively                                                           #      
#               Ext : Files extension                                                            #
#               norm2: Boolean variable for nomalisation, default: TRUE                          #
#               ONE : Boolean variable for one / binary classification data                      #
#               PERC : Percentage of non-TSC patients to be included in training set             #
#               default : 0.1                                                                    #
#               NumSimulation : Total number of simulations to be performed, default : 10        #  
#               constNumVlPtNonTSC : Number of non-TSC patients in validation set, default: 20000#                                                                                               
#               SeedV : A seed vetcor. Use different seed values to generate different datasets. #
#               Use same seed values to reproduce results.                                       #  
#                                                                                                # 
#                                                                                                # 
# Author : Huma Lodhi                                                                            #
# Date :  2014 --                                                                                #
#                                                                                                #
##################################################################################################


rm(list=ls())

library("snowfall")
sfLibrary("snow",character.only = TRUE)
sfLibrary("MASS", character.only = TRUE)
sfLibrary("snowfall", character.only = TRUE)
sfLibrary("plyr",character.only = TRUE)


#sfSource("D:/RareDisease_TSC/source/PreProcess/SampAndRes.R")
#sfSource("D:/RareDisease_TSC/source/PreProcess/NormAge.R")
#sfSource("D:/RareDisease_TSC/source/PreProcess/MkDictEnvt.R")
#sfSource("D:/RareDisease_TSC/source/PreProcess/PrsTst.R")
#sfSource("D:/RareDisease_TSC/source/PreProcess/MkFtVec.R")
sfSource('D:/RareDisease_TSC/source/FV_2_update/PreProcess/SampAndRes_forParOnSimu_comb.R')
sfSource('D:/RareDisease_TSC/source/FV_2_update/PreProcess/NormAge.R')
sfSource('D:/RareDisease_TSC/source/FV_2_update/PreProcess/MkDictEnvt_comb.R')
sfSource('D:/RareDisease_TSC/source/FV_2_update/PreProcess/PrsTst_forTest.R')
sfSource('D:/RareDisease_TSC/source/FV_2_update/PreProcess/MkFtVec_comb.R')

inFile1TSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\TSCAgeGender_May06.txt"
inFile2TSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\TSCCodeDrug_May13.txt"

#using huma's input file
inFile1NonTSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\NonTSCAgeGender_huma.txt"
inFile2NonTSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\NonTSCCodeDrug_huma.txt"

#Manifest data for Logistic regression
#inFile1TSC_manifest <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\TSC_model_data_v2.txt"
#inFile1NonTSC_manifest <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\NonTSC_model_data_v2.txt"
#correct the LR input data according to Yan's suggestion
inFile1TSC_manifest <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\TSC_model_data_Jun04.txt"
inFile1NonTSC_manifest <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\NonTSC_model_data_Jun04.txt"


#using yan's input file
#inFile1NonTSC <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\NonTSCAgeGender_May06.txt"
#inFile2NonTSC <- "D:\\jzhao\\RareDisease_TSC\\01. Data\\input data\\NonTSCCodeDrug_May06.txt"

#using yan's second input file
#inFile1NonTSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\NonTSCAgeGender_yan_2nd.txt"
#inFile2NonTSC <- "D:\\RareDisease_TSC\\Data\\TobeFV\\TobeFV_update\\NonTSCCodeDrug_yan_2nd.txt"

#create new directory
output_path1 <- 'D:\\RareDisease_TSC\\Data\\Dict_ind_2\\2n_comb'
output_path2 <- 'D:\\RareDisease_TSC\\Data\\FV_2\\2n_comb'
if(!file.exists(output_path1)){
    dir.create(output_path1, recursive=T, showWarnings=T)
    setwd(output_path1)
}
setwd(output_path1)

if(!file.exists(output_path2)){
    dir.create(output_path2, recursive=T, showWarnings=T)
    setwd(output_path2)
}
setwd(output_path2)


dictFile <- paste(output_path1, "\\CodesDrugsDict", sep='')

TSCTsIndexFile <- paste(output_path1, "\\TSCTestIndices", sep='')
TSCTrIndexFile <- paste(output_path1, "\\TSCTrIndices", sep='')
TSCVlIndexFile <- paste(output_path1, "\\TSCVlIndices", sep='')

TsIndexFile <- paste(output_path1, "\\TestIndices", sep='')
TrIndexFile <- paste(output_path1, "\\TrIndices", sep='')
VlIndexFile <- paste(output_path1, "\\VlIndices", sep='')

TrFile1 <- paste(output_path2, "\\trainv", sep='')
VlFile <- paste(output_path2, "\\valid", sep='')
TrFile <- paste(output_path2, "\\train", sep='')
TsFilePos <- paste(output_path2, "\\testpos", sep='')
TsFileNeg <- paste(output_path2, "\\testneg", sep='')
TimeLog <- paste(output_path2, "", sep='')
Ext <- ".csv"

NumSimulation <- 10

PERC <- 0.1

ONE <- FALSE

constNumVlPtNonTSC <- 20000

norm2 <- TRUE
#norm2 <- FALSE

#SeedV <- c(1000000,9000000,18000000,27000000,36000000,45000000,54000000,63000000,72000000,81000000) # For first (SAS) sample

SeedV <- c(173514,8912020,64276204,84048,9863684,480,4033848,874190486,1334165,1565796911) # For second (SAS) sample

##################################################################################################
#                           Read raw TSC & Non-TSC data files                                    #
##################################################################################################


tm <- proc.time()

cat("\n Reading and age and gender for TSC and non-TSC patients\n")   

# Read age and gender for TSC patients
TSCAgeGender1 <- read.table(inFile1TSC,sep="\t",comment.char="") 
#TSCAgeGender1 <- cbind(TSCAgeGender1, rep(0, nrow(TSCAgeGender1)))   #[334, 6]
TSC_manifest <- read.table(inFile1TSC_manifest, sep='\t', comment.char="")
TSCAgeGender <- cbind(TSCAgeGender1, TSC_manifest[, -(1:5)])
manifest_age_combo<-sort(paste(paste("C",7:11,sep=""),rep(1:2,each=5),sep="_"))[c(5:10,1:4)]
colnames(TSCAgeGender)<-c("Id","FirstAge", "LastAge", "Male","Female",paste("C",1:11,sep=""),manifest_age_combo)


# Read age and gender for non-TSC patients
NonTSCAgeGender1 <- read.table(inFile1NonTSC,sep="\t",comment.char="") #[100000, 6]
NonTSC_manifest <- read.table(inFile1NonTSC_manifest, sep='\t', comment.char="")
NonTSCAgeGender <- cbind(NonTSCAgeGender1[, -6], NonTSC_manifest[, -(1:6)])
colnames(NonTSCAgeGender)<-c("Id","FirstAge", "LastAge", "Male","Female",paste("C",1:11,sep=""),manifest_age_combo)



cat("\n Reading ICD, READ, OPCS codes and drugs for TSC and non-TSC patients\n")

# Read codes and drugs for TSC patients
TSCCodeDrug <- read.table(inFile2TSC,sep="\t",comment.char="",colClasses="character")  #[1] 334 365

# Read codes and drugs for non-TSC patiemts
NonTSCCodeDrug <- read.table(inFile2NonTSC,sep="\t",comment.char="",colClasses="character")  #[1] 100000    638

cat("Reading data time: ", proc.time()- tm,"\n")   #42s

##################################################################################################
#                     Preprocess data in parallel or sequential mode 
#                     Modified by Jie
##################################################################################################

num_pros <- Sys.getenv('NUMBER_OF_PROCESSORS')
sfInit(parallel=TRUE, cpus=num_pros, type="SOCK",slaveOutfile = "D:/RareDisease_TSC/Data/parOnSimu.log")
sfLibrary(snowfall)
timeStart <- proc.time()
sfExport("TSCAgeGender","TSCCodeDrug","NonTSCAgeGender","NonTSCCodeDrug","SeedV","constNumVlPtNonTSC","norm2", "ONE")
sfExport("dictFile","TrFile1","VlFile","TrFile","PERC","TrIndexFile","VlIndexFile","TsIndexFile","TSCTrIndexFile","TSCVlIndexFile","TSCTsIndexFile", 'TsFilePos', 'TsFileNeg', "Ext")
sfExport("SampAndRes","NormAge","MkDictEnv","MkFtVec","PrsTst")
sfExport("ldply", namespace = "plyr")
sfExport("quickdf", namespace = "plyr")
cat('time used for sfExport: ', proc.time()-timeStart, '\n')
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_v3:oneClass=', ONE, 'time used for sfExport: ', proc.time()-timeStart, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

#for (counter in 1:NumSimulation) {
# tm <- proc.time()

#SampAndRes(counter)
#cat("Simulation ", counter, " time: ", proc.time()- tm,"\n")
#}

##modified by jzhao#######
runBySimu <- function(counter){
  tm <- proc.time()
  SampAndRes(counter)
  cat("Simulation ", counter, " time: ", proc.time()- tm,"\n")
  
}

timeStart <- proc.time()
result <- sfClusterApplyLB( 1:10, runBySimu)
timeEnd <- proc.time()
timeUsed <- timeEnd-timeStart
cat(file=paste(TimeLog, '/timeLogComp', Ext, sep=''),append=TRUE,'main_v3:oneClass=', ONE, ' time used for simulation parallel: ', timeUsed, 'and the time is ', format(Sys.time(), "%a %b %d %X %Y"), '\n')

sfStop()







