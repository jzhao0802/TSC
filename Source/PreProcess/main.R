
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


  library("snowfall")
  sfLibrary("snow",character.only = TRUE)
  sfLibrary("MASS", character.only = TRUE)
  sfLibrary("snowfall", character.only = TRUE)
  sfLibrary("plyr",character.only = TRUE)
  
  sfSource("D:/RareDisease_TSC/source/PreProcess/SampAndRes.R")
  sfSource("D:/RareDisease_TSC/source/PreProcess/NormAge.R")
  sfSource("D:/RareDisease_TSC/source/PreProcess/MkDictEnvt.R")
  sfSource("D:/RareDisease_TSC/source/PreProcess/PrsTst.R")
  sfSource("D:/RareDisease_TSC/source/PreProcess/MkFtVec.R")

  inFile1TSC <- "D:/RareDisease_TSC/Data/TobeFV/TSCAgeGender.txt"
  inFile2TSC <- "D:/RareDisease_TSC/Data/TobeFV/TSCCodeDrug.txt"
  
  inFile1NonTSC <- "D:/RareDisease_TSC/Data/TobeFV/NonTSCAgeGender_2.txt"
  inFile2NonTSC <- "D:/RareDisease_TSC/Data/TobeFV/NonTSCCodeDrug_2.txt"
  

  dictFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/CodesDrugsDict"

  TSCTsIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/TSCTestIndices"
  TSCTrIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/TSCTrIndices"
  TSCVlIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/TSCVlIndices"

  TsIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/TestIndices"
  TrIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/TrIndices"
  VlIndexFile <- "D:/RareDisease_TSC/Data/Dict_Ind_2/1/VlIndices"
  
  TrFile1 <- "D:/RareDisease_TSC/Data/FV_2/1/trainv"
  VlFile <- "D:/RareDisease_TSC/Data/FV_2/1/valid"
  TrFile <- "D:/RareDisease_TSC/Data/FV_2/1/train"
  TsFilePos <- "D:/RareDisease_TSC/Data/FV_2/1/testpos"
  TsFileNeg <- "D:/RareDisease_TSC/Data/FV_2/1/testneg"
  Ext <- ".csv"
  
  NumSimulation <- 10
  
  PERC <- 0.1
  
  ONE <- TRUE

  constNumVlPtNonTSC <- 20000
  
  norm2 <- FALSE

  #SeedV <- c(1000000,9000000,18000000,27000000,36000000,45000000,54000000,63000000,72000000,81000000) # For first (SAS) sample

  SeedV <- c(173514,8912020,64276204,84048,9863684,480,4033848,874190486,1334165,1565796911) # For second (SAS) sample
  
##################################################################################################
#                           Read raw TSC & Non-TSC data files                                    #
##################################################################################################

  
  tm <- proc.time()

  cat("\n Reading and age and gender for TSC and non-TSC patients\n")   

  # Read age and gender for TSC patients
  TSCAgeGender <- read.table(inFile1TSC,sep="\t",comment.char="")  
  colnames(TSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")

  # Read age and gender for non-TSC patients
  NonTSCAgeGender <- read.table(inFile1NonTSC,sep="\t",comment.char="")
  colnames(NonTSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")
  
  cat("\n Reading ICD, READ, OPCS codes and drugs for TSC and non-TSC patients\n")
  
  # Read codes and drugs for TSC patients
  TSCCodeDrug <- read.table(inFile2TSC,sep="\t",comment.char="",colClasses="character")  

  # Read codes and drugs for non-TSC patiemts
  NonTSCCodeDrug <- read.table(inFile2NonTSC,sep="\t",comment.char="",colClasses="character")


  cat("Reading data time: ", proc.time()- tm,"\n")
  
##################################################################################################
#                     Preprocess data in parallel or sequential mode                             #
##################################################################################################

  
  sfExport("TSCAgeGender","TSCCodeDrug","NonTSCAgeGender","NonTSCCodeDrug","SeedV","constNumVlPtNonTSC","norm2")
  sfExport("dictFile","TrFile1","VlFile","TrFile","PERC","TrIndexFile","VlIndexFile","TsIndexFile","TSCTrIndexFile","TSCVlIndexFile","TSCTsIndexFile","Ext")

  for (counter in 1:NumSimulation) {
    tm <- proc.time()

    SampAndRes(counter)
    cat("Simulation ", counter, " time: ", proc.time()- tm,"\n")
  }
  

  
 

  