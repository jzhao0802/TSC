
#library(xlsx)
rm(list=ls())
inFile1TSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/TSCAgeGender_may06.txt "
inFile2TSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/TSCCodeDrug_may13.txt"

inFile1NonTSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/NonTSCAgeGender_huma.txt"
inFile2NonTSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/NonTSCAgeGender_huma.txt"
#inFile1NonTSC2 <- "D:/jzhao/RareDisease_TSC/01. Data/input data/NonTSCAgeGender_May06.txt"


tm <- proc.time()

cat("\n Reading and age and gender for TSC and non-TSC patients\n")   

# Read age and gender for TSC patients
TSCAgeGender <- read.table(inFile1TSC,sep="\t",comment.char="") 
TSCAgeGender <- cbind(TSCAgeGender, rep(0, nrow(TSCAgeGender)))
colnames(TSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")

# Read age and gender for non-TSC patients
NonTSCAgeGender <- read.table(inFile1NonTSC,sep="\t",comment.char="")
#NonTSCAgeGender2 <- read.table(inFile1NonTSC2,sep="\t",comment.char="")
Ind_huma <- NonTSCAgeGender[, 1]
Ind_May06 <- NonTSCAgeGender2[, 1]
Ind_yan2 <- NonTSCAgeGender[, 1]
#num_matched_huma <- sum(!is.na(match(Ind_huma, Ind_May06)))
#num_matched_May06 <- sum(!is.na(match(Ind_May06, Ind_huma)))

#pat_id <- read.csv('D:\\Hui\\TSC_project\\Huma_100k_Id.csv', header=T)
#pat_id2 <- pat_id[,2]

#num_match <- sum(!is.na(match(Ind_May06, pat_id2)))

#table(Ind_May06 %in% pat_id2)

#write.csv(Ind_huma, 'D:\\Hui\\TSC_project\\Huma.csv', quote=F, row.names=F)

#write.csv(Ind_May06, 'D:\\Hui\\TSC_project\\may06.csv', quote=F, row.names=F)

colnames(NonTSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")

cat("\n Reading ICD, READ, OPCS codes and drugs for TSC and non-TSC patients\n")

# Read codes and drugs for TSC patients
TSCCodeDrug <- read.table(inFile2TSC,sep="\t",comment.char="",colClasses="character")  
dict_all_TSC <- unlist(lapply(1:nrow(TSCCodeDrug), function(X){TSCCodeDrug[X, ]}))
dict_all_TSC_uniq <- unique(dict_all_TSC)
# Read codes and drugs for non-TSC patiemts
NonTSCCodeDrug <- read.table(inFile2NonTSC,sep="\t",comment.char="",colClasses="character")
dict_all_nonTSC <- unlist(lapply(1:nrow(NonTSCCodeDrug), function(X){NonTSCCodeDrug[X, ]}))
dic_all_uniq <- unique(c(dict_all_TSC_uniq, dict_all_nonTSC_uniq))

cat("Reading data time: ", proc.time()- tm,"\n")

outpath <- 'D:/jzhao/RareDisease_TSC/03. Output/feature correlation/Jul07'
if(!file.exists(outpath)){
    dir.create(outpath, recursive=T, showWarnings=T)
    setwd(outpath)
}else{
    setwd(outpath)    
}

dictFile_TSC <- paste(outpath, 'CodesDrugsDict_TSC', sep="")
#dictFile_TSC <- "D:/jzhao/RareDisease_TSC/03. Output/feature correlation/May06/CodesDrugsDict_TSC_0508"
dictFile_TSC_train <- paste(outpath,'CodesDrugsDict_TSC_train', sep='')
dictFile_NonTSC3w <- paste(outpath, 'CodesDrugsDict_NonTSC3w', sep='')
dictFile_NonTSC10w <- paste(outpath, 'CodesDrugsDict_NonTSC10w', sep='')

dictFile3w <- paste(outpath, 'CodesDrugsDict3w', sep='')
dictFile10w <- paste(outpath, 'CodesDrugsDict10w', sep='')

#dictFile_TSC <- "D:/jzhao/RareDisease_TSC/03. Output/feature correlation/May06/CodesDrugsDict_TSC_0513"
#dictFile_TSC <- "D:/jzhao/RareDisease_TSC/03. Output/feature correlation/May06/CodesDrugsDict_TSC_0617"
Ext <- '.csv'
dictFile_TSC_<- paste(dictFile_TSC,Ext,sep="")
dictFile_TSC_train_<- paste(dictFile_TSC_train,Ext,sep="")
dictFile_NonTSC3w_<- paste(dictFile_NonTSC3w,Ext,sep="")
dictFile_NonTSC10w_<- paste(dictFile_NonTSC10w,Ext,sep="")

dictFile_<- paste(dictFile,Ext,sep="")

#extract the 234 trainv and 50 valid TSC patient, but not the whole 334 TSC patient, for the dictionary built and correlation calculation
path_indices<- 'D:\\RareDisease_TSC\\Data\\Dict_Ind_2_update\\2n\\'
TSCTrIndicesF <- paste(path_indices, "TSCTrIndices2", Ext, sep='')
TSCVlIndicesF <- paste(path_indices, "TSCVlIndices2", Ext, sep='')
TrIndicesF <- paste(path_indices, "TrIndices2", Ext, sep='')
#VlIndicesF <- paste(path_indices, "VlIndices2", Ext, sep='')
TSCTrIndices <- read.table(TSCTrIndicesF, sep='')
TSCVlIndices <- read.table(TSCVlIndicesF, sep='')
TSCTrainIndices <- c(TSCTrIndices[, 1], TSCVlIndices[, 1]) # the target 284TSC patient indices

TrIndices <- read.table(TrIndicesF, sep='')
#VlIndices <- read.table(VlIndicesF, sep='')
NonTSCIndices_3w <- c(TrIndices[, 1], VlIndices[, 1]) # the target30000 NonTSC patient indices
NonTSCIndices_1w <- TrIndices[, 1]

TSCCodeDrug_train <- TSCCodeDrug[!is.na(match(as.numeric(TSCCodeDrug[, 1]), TSCTrainIndices)), ]
TSCAgeGender_train <- TSCAgeGender[!is.na(match(as.numeric(TSCAgeGender[, 1]), TSCTrainIndices)), ]
NonTSCCodeDrug_3w <- NonTSCCodeDrug[!is.na(match(as.numeric(NonTSCCodeDrug[, 1]), NonTSCIndices_3w)), ]
NonTSCAgeGender_3w <- NonTSCAgeGender[!is.na(match(as.numeric(NonTSCAgeGender[, 1]), NonTSCIndices_3w)), ]
NonTSCCodeDrug_1w <- NonTSCCodeDrug[!is.na(match(as.numeric(NonTSCCodeDrug[, 1]), NonTSCIndices_1w)), ]
NonTSCAgeGender_1w <- NonTSCAgeGender[!is.na(match(as.numeric(NonTSCAgeGender[, 1]), NonTSCIndices_1w)), ]

inFile <- "D:/jzhao/RareDisease_TSC/03. Output/feature correlation/May06/TSCNotMatchedInNonTSC_10w.csv "

tar <- as.character(read.table(inFile, header=T, sep=',')[, 2])
retain <- tar[-grep('^Q85.1|Q851|PK5', tar, ignore.case=T)]

#for initial input data from SAS QC 
MkDictEnv <- function(PtsCodeDrug,dictFile_) {
  tempCodeDrug <- lapply(X=1:nrow(PtsCodeDrug), function(X) PtsCodeDrug[, -1][X,])

  CodeDrugDict <- unique((unlist(tempCodeDrug)))
  
  bindex <- which(CodeDrugDict == "-")
  CodeDrugDict <- CodeDrugDict[-bindex]
  
  # find unique drug substance
 # index_drug <- which(is.na(as.numeric(gsub("[^0-9]", "", CodeDrugDict)))==TRUE)
  #drug <- CodeDrugDict[index_drug] 
  
  DictElem <- length(CodeDrugDict)
  Indices <- c(1:DictElem)  
  tIndices <- c(6:(DictElem+5)) 
  
  write.table(data.frame(tIndices,CodeDrugDict),file=dictFile_)
 df <- data.frame(tIndices,CodeDrugDict)
  Dict <- new.env(hash=TRUE)
  
  Dict$Indices <- CodeDrugDict
  
  rm(CodeDrugDict)
  
  return(df)
  
}

tm <- Sys.time()
Dict <- MkDictEnv(TSCCodeDrug_train,dictFile_TSC_train_)
Dict <- MkDictEnv(NonTSCCodeDrug_3w,dictFile_NonTSC3w_)

Dict1 <- MkDictEnv(TSCCodeDrug,dictFile_TSC_)
Dict2<- MkDictEnv(NonTSCCodeDrug,dictFile_NonTSC10w_)
#Dict <- MkDictEnv(PtsCodeDrug,dictFile_)
cat('\n running time :', Sys.time() - tm, '\n')
nomatch <- retain[!(retain %in% dict)]
lapply(X=1:length(nomatch), function(X)grep(nomatch[X], dict, ignore.case=T, value=T))


pathf <-"D:\\jzhao\\RareDisease_TSC\\03. Output\\feature correlation\\May06\\"

CodesDrugsDict_TSC <- read.table(paste(pathf, 'CodesDrugsDict_TSC',Ext, sep=''), header=T, sep=' ')
#CodesDrugsDict_TSC_train <- read.table(paste(pathf, 'CodesDrugsDict_TSC_train',Ext, sep=''), header=T, sep=' ')

#CodesDrugsDict_NonTSC3w <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC3w',Ext, sep=''), header=T, sep=' ')
CodesDrugsDict_NonTSC10w <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC10w',Ext, sep=''), header=T, sep=' ')

#CodesDrugDict3w <- unique(c(as.character(CodesDrugsDict_TSC_train[, 2]), as.character(CodesDrugsDict_NonTSC3w[, 2])))
CodesDrugDict10w <- unique(c(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC10w[, 2])))

diaMatch_check <- function(dataBased){
    if(dataBased=='3w'){
        CodesDrugsDict_TSC <- read.table(paste(pathf, 'CodesDrugsDict_TSC_train',Ext, sep=''), header=T, sep=' ') 
        CodesDrugsDict_NonTSC <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC3w',Ext, sep=''), header=T, sep=' ')
        noMatchF <- 'TSCNotMatchedInNonTSC_3w.csv'
        dictF <- 'CodeDrugDict_3w.csv'
    }else if(dataBased=='10w'){
        CodesDrugsDict_TSC <- read.table(paste(pathf, 'CodesDrugsDict_TSC',Ext, sep=''), header=T, sep=' ') 
        CodesDrugsDict_NonTSC <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC10w',Ext, sep=''), header=T, sep=' ')
        noMatchF <- 'TSCNotMatchedInNonTSC_10w.csv'
        dictF <- 'CodeDrugDict_10w.csv'
        
    }else{
        stop('\nthe wrong input!\n')
    }
    CodesDrugDict <- unique(c(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC[, 2])))
    write.csv(CodesDrugDict, file=paste(pathf, dictF, sep=''), row.names=F)
    
    dictMatch <- match(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC[, 2]))
    length(dictMatch[is.na(dictMatch)])
    TSCcodenoMatch <- as.character(CodesDrugsDict_TSC[, 2])[which(is.na(dictMatch))]
    TSCcodenoMatch_df <- data.frame(DrugCode=TSCcodenoMatch)
    write.csv(TSCcodenoMatch_df, file=paste(pathf, noMatchF, sep=''))
    
    dictMatch_nonTSC <- match(as.character(CodesDrugsDict_NonTSC[, 2]), as.character(CodesDrugsDict_TSC[, 2]))
    length(dictMatch_nonTSC[is.na(dictMatch_nonTSC)])
    NonTSCcodenoMatch <- as.character(CodesDrugsDict_NonTSC[, 2])[which(is.na(dictMatch_nonTSC))]
    nomatchedCodeInNonTSC <- lapply(X=1:length(TSCcodenoMatch), function(X){grep(paste('^',TSCcodenoMatch[X], '$', sep=''), as.character(CodesDrugsDict_NonTSC[, 2]), ignore.case=T, value=T)})
    length(lapply(NonTSCcodenoMatch, function(x){length(x)==0}))
    ForTSC_notfindInNonTSC <- unlist(lapply(nomatchedCodeInNonTSC, function(x){length(x)==0}))
    TSCcodenoMatch[ForTSC_notfindInNonTSC]
    caseIssue<- TSCcodenoMatch[!ForTSC_notfindInNonTSC]
    return(caseIssue)
}
caseIssue1 <- diaMatch_check('3w')
caseIssue2 <- diaMatch_check('10w')


#CodeDrug <- rbind(TSCCodeDrug, NonTSCCodeDrug)
#CodesDrugsDict_TSC_train <- read.table(paste(pathf, 'CodesDrugsDict_TSC_train',Ext, sep=''), header=T, sep=' ')
#CodesDrugsDict_NonTSC3w <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC3w',Ext, sep=''), header=T, sep=' ')
#CodesDrugDict3w <- unique(c(as.character(CodesDrugsDict_TSC_train[, 2]), as.character(CodesDrugsDict_NonTSC3w[, 2])))  #[1] 35322
#CodesDrugDict10w <- unique(c(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC10w[, 2])))
CodesDrugDict1w <- read.table(paste(path_indices, 'CodesDrugsDict_forValidation2', Ext, sep=''), header=T, sep=' ') [, 2]
data <- '10w'
TSC<- F

creatCorMatrix <- function(data, TSC=T){
    
    if(TSC){
        if(data=="1w"){
            CodeDrug <- TSCCodeDrug_train
            AgeGender <- TSCAgeGender_train
            
        }else{
            CodeDrug <- TSCCodeDrug
            AgeGender <- TSCAgeGender
            
        }
        #CodesDrugDict <- as.character(read.table(paste(pathf, 'CodesDrugsDict_TSC_train',Ext, sep=''), header=T)[, 2]) #5896
        label<- 1
    }else{
        if(data=="1w"){
            CodeDrug <- NonTSCCodeDrug_1w
            AgeGender <- NonTSCAgeGender_1w
            
        }else{
            CodeDrug <- NonTSCCodeDrug
            AgeGender <- NonTSCAgeGender
            
        }
        #CodesDrugDict <- as.character(read.table(paste(pathf, 'CodesDrugsDict_NonTSC3w',Ext, sep=''), header=T)[, 2])
        label <- -1
    }
    matrix <- eval(parse(text=paste("matrix(0, nr=nrow(CodeDrug), nc=length(CodesDrugDict", data, "))", sep='')))
    #matrix <- matrix(0, nr=100, nc=length(CodesDrugDict3w))
    for(i in 1:nrow(matrix)){
        codeNm <- CodeDrug[i,][-1]
        codeIdx <- eval(parse(text=paste("match(as.vector(t(codeNm)), CodesDrugDict", data, ')', sep='')))
        codeIdx1 <- codeIdx[!is.na(codeIdx)]
        #value <- ifelse(is.na(codeIdx), 0, 1)
        matrix[i, codeIdx1] <- 1
        #matrix[i, 2:6] <- AgeGender[i, 2:6]
    }
    codeDemo_mtx<- cbind(Response=rep(label, nrow(matrix)), AgeGender[,-1], matrix)
}
matrix_TSC <- creatCorMatrix(data="1w", TSC=T)
tm <- format(Sys.time(), "%a %b %d %X %Y")
matrix_NonTSC <- creatCorMatrix(data='1w', TSC=F)
cat(tm, '\t', format(Sys.time(), "%a %b %d %X %Y"), '\n')
codeDemo_mtx <- rbind(matrix_TSC, matrix_NonTSC)
codeDemo <- as.data.frame(codeDemo_mtx)
colnames(codeDemo_mtx) <- c('Response', names(TSCAgeGender)[-1], as.character(CodesDrugDict1w))

rm.featureIdx <- which(apply(codeDemo_mtx, 2, sum)==0)
#codeDemo[, as.vector(rm.featureIdx)] <- NULL
codeDemo_mtx2 <- codeDemo_mtx[,-rm.featureIdx]
save(codeDemo_mtx2, file=paste(pathf, 'codeDemoMatrix_baseOnHuma10284_17Jun.RData', sep=''))

cor <- cor(codeDemo_mtx2[, 1], codeDemo_mtx2[, -1])
correlation <- cbind(colnames(cor)[rev(order(abs(cor)))], cor[rev(order(abs(cor)))])
colnames(correlation) <- c('Feature', 'Correlation')
write.csv(correlation, paste(pathf, 'correlation_10284.csv', sep=''))
correlation_top15 <- correlation[1:15,]
write.csv(correlation_top15, paste(pathf, 'correlation_top15_10284.csv', sep=''))


