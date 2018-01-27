
#library(xlsx)

inFile1TSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/TSCAgeGender_may06.txt "
inFile2TSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/TSCCodeDrug_may06.txt"

inFile1NonTSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/NonTSCAgeGender_Huma.txt"
inFile2NonTSC <- "D:/jzhao/RareDisease_TSC/01. Data/input data/NonTSCCodeDrug_Huma.txt"


tm <- proc.time()

cat("\n Reading and age and gender for TSC and non-TSC patients\n")   

# Read age and gender for TSC patients
TSCAgeGender <- read.table(inFile1TSC,sep="\t",comment.char="")  
TSCAgeGender <- cbind(TSCAgeGender, rep(0, nrow(TSCAgeGender)))
colnames(TSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")
TSCCodeDrug <- read.table(inFile2TSC,sep="\t",comment.char="") 
# Read age and gender for non-TSC patients
NonTSCAgeGender <- read.table(inFile1NonTSC,sep="\t",comment.char="")
colnames(NonTSCAgeGender) <- c("Id","FirstAge", "LastAge", "Male","Female","NE")
NonTSCCodeDrug <- read.table(inFile2NonTSC,sep="\t",comment.char="") 

cat("\n Reading ICD, READ, OPCS codes and drugs for TSC and non-TSC patients\n")


datapath <- 'D:\\jzhao\\RareDisease_TSC\\01. Data\\Dict_Ind_2\\2n\\'
TSCTrIndices <- 'TSCTrIndices2'
TSCVlIndices <- 'TSCVlIndices2'
TrIndices <- 'TrIndices2'
VlIndices <- 'VlIndices2'
CodeDrugDict <- 'CodesDrugsDict2'
path <- 'D:\\jzhao\\RareDisease_TSC\\01. Data\\FV_2\\2n\\'
trainf <- 'train2'
validf <- 'valid2'
Ext <- '.csv'

TSCTrIndices <- read.csv(paste(datapath, TSCTrIndices,Ext, sep=''), header=F  )
TSCVlIndices <- read.csv(paste(datapath, TSCVlIndices,Ext, sep=''), header=F  )
TrIndices <- read.csv(paste(datapath, TrIndices,Ext, sep=''), header=F  )
VlIndices <- read.csv(paste(datapath, VlIndices,Ext, sep=''), header=F  )
CodeDict <- read.table(paste(datapath, CodeDrugDict,Ext, sep=''), header=T, sep=' ')
train <- read.table(paste(path, trainf,Ext, sep=''), header=F, sep='\t')
valid <- read.table(paste(path, validf,Ext, sep=''), header=F, sep='\t')
TSC_total_Indices <- rbind(TSCTrIndices, TSCVlIndices)
NonTSC_total_Indices <- rbind(TrIndices, VlIndices)

#TSC_demo <- TSCAgeGender[TSCAgeGender[,1] %in% TSC_total_Indices[,1] ,]
TSC_demo <- TSCAgeGender[is.na(match(TSCAgeGender[, 1], TSC_total_Indices[, 1])), ]
#TSC_demo <- TSC_demo[!is.na(TSC_demo[,1]), ]
#Non_TSC_demo <- NonTSCAgeGender[NonTSCAgeGender[,1] %in% NonTSC_total_Indices[,1], ]
Non_TSC_demo <- NonTSCAgeGender[match(NonTSCAgeGender[, 1], NonTSC_total_Indices[, 1]), ]
Non_TSC_demo <- Non_TSC_demo[!is.na(Non_TSC_demo[,1]), ]


TSC_matrix <- matrix(0, nrow= nrow(TSC_demo), ncol=nrow(CodeDict)+5)
Non_TSC_matrix <- matrix(0, nrow= nrow(Non_TSC_demo), ncol=nrow(CodeDict)+5)

tsc_patnum <- length(unique(TSC_demo[,1])) 
non_tsc_patnum <- length(unique(Non_TSC_demo[,1]))

trainSplit <- strsplit(apply(train, 1, as.character), '\\s', perl=T)
label <- unlist(lapply(trainSplit, function(x)x[1]))
#trainTsc <- lapply(1:length(trainSplit), function(x)trainSplit[[x]]=="1")
trainTsc <- train[label=="1", ]
trainNonTsc <- train[label=="-1", ]

validSplit <- strsplit(apply(valid, 1, as.character), '\\s', perl=T)
label_v <- unlist(lapply(validSplit, function(x)x[1]))
#trainTsc <- lapply(1:length(trainSplit), function(x)trainSplit[[x]]=="1")
validTsc <- valid[label_v=="1", ]
validNonTsc <- valid[label_v=="-1", ]
tsc <- c(trainTsc)
nonTsc <- c(trainNonTsc, validNonTsc)

i<- 1
train <- tsc
for(i in tsc_patnum){
	line <- as.character(train[i, ])
	linei <- strsplit(line, split='\\s', perl=T)[[1]][-1] #drop the diagnosis label
	lineii <- strsplit(linei, split=':')
	score <- as.numeric(unlist(lapply(lineii, function(x)x[[2]])))
	idx <- as.numeric(unlist(lapply(score, function(x)x[[1]])))
	TSC_matrix[i ,idx+5] <- score
}
TSC_matrix[, 1:5] <- TSC_demo[, -1]
 
train <- nonTsc
for(i in tsc_patnum){
	line <- as.character(train[i, ])
	linei <- strsplit(line, split='\\s', perl=T)[[1]][-1] #drop the diagnosis label
	lineii <- strsplit(linei, split=':')
	score <- as.numeric(unlist(lapply(lineii, function(x)x[[2]])))
	idx <- as.numeric(unlist(lapply(score, function(x)x[[1]])))
	Non_TSC_matrix[i ,idx+5] <- score
}
Non_TSC_matrix[, 1:5] <- Non_TSC_demo[, -1]

matrix.df <- as.data.frame(rbind(TSC_matrix, Non_TSC_matrix))
colnames(matrix.df)<- CodeDict[, 2]
 
#the second data manipulation method
	trainTSC_demo <- TSCAgeGender[!is.na(match(TSCAgeGender[, 1], TSCTrIndices[, 1])), ]
	validTSC_demo <- TSCAgeGender[!is.na(match(TSCAgeGender[, 1], TSCVlIndices[, 1])), ]
	trainNonTSC_demo <- NonTSCAgeGender[!is.na(match(NonTSCAgeGender[, 1], TrIndices[, 1])), ]
	validNonTSC_demo <- NonTSCAgeGender[!is.na(match(NonTSCAgeGender[, 1], VlIndices[, 1])), ]

	matrix_demo <- rbind(trainTSC_demo, trainNonTSC_demo, validTSC_demo, validNonTSC_demo)

	matrix <- matrix(0, nrow=nrow(matrix_demo), ncol=nrow(CodeDict)+6)
	#matrix <- matrix(0, nrow=10, ncol=nrow(CodeDict)+6)
	trVl <- rbind(train, valid)
	train <- trVl
lastScore <- lapply(train, function(x)strsplit(strsplit(x, split='\\s', perl=T)[[1]], split=':')[[2]])
#idxLast <- numeric()
#scoreLast <- numeric()
for(i in 1:nrow(matrix)){
	line <- as.character(train[i, ])
	label <- as.numeric(strsplit(line, split='\\s', perl=T)[[1]][1])
	linei <- strsplit(line, split='\\s', perl=T)[[1]][-c(1, length(strsplit(line, split='\\s', perl=T)[[1]]))] #drop the diagnosis label and the last idx_score(0)
	lineii <- strsplit(linei, split=':')
	score <- as.numeric(unlist(lapply(lineii, function(x)x[[2]])))
	#scoreLast <- rbind(scoreLast, score[length(score)])
	idx <- as.numeric(unlist(lapply(lineii, function(x)x[[1]])))
	#idxLast <- rbind(idxLast, idx[length(idx)])
	matrix[i ,c(1, idx+1)] <- c(label, score)

}
#matrix[, 1:6] <- matrix_demo[1:10, ] 
#matrix <- cbind(matrix_demo[, ], matrix[, -(1:6)])
matrix.df <- as.data.frame(matrix)
colnames(matrix.df)<- c(names(matrix_demo), as.character(CodeDict[, 2]))

cor <- cor(matrix.df[, 1], matrix.df[, -1])
correlation <- cbind(colnames(cor)[rev(order(cor))], cor[rev(order(cor))])

#the third data manipulation method for 100000 pt
#extract the 234 trainv and 50 valid TSC patient, but not the whole 334 TSC patient, for the dictionary built and correlation calculation
path_indices<- 'D:\\Hui\\TSC project\\Source\\Feature Selection\\data\\indices\\'
Ext <- '.csv'
TSCTrIndicesF <- paste(path_indices, "TSCTrIndices2", Ext, sep='')
TSCVlIndicesF <- paste(path_indices, "TSCVlIndices2", Ext, sep='')
TSCTrIndices <- read.table(TSCTrIndicesF, sep='')
TSCVlIndices <- read.table(TSCVlIndicesF, sep='')
TSCTrainIndices <- c(TSCTrIndices[, 1], TSCVlIndices[, 1]) # the target 284TSC patient indices
TSCCodeDrug_train <- TSCCodeDrug[!is.na(match(as.numeric(TSCCodeDrug[, 1]), TSCTrainIndices)), ]
TSCAgeGender_train <- TSCAgeGender[!is.na(match(as.numeric(TSCAgeGender[, 1]), TSCTrainIndices)), ]

pathf <-"D:\\jzhao\\RareDisease_TSC\\03. Output\\feature correlation\\May06\\"
CodesDrugsDict_TSC <- read.table(paste(pathf, 'CodesDrugsDict_TSC',Ext, sep=''), header=T, sep=' ')
CodesDrugsDict_TSC_train <- read.table(paste(pathf, 'CodesDrugsDict_TSC_train',Ext, sep=''), header=T, sep=' ')

CodesDrugsDict_NonTSC <- read.table(paste(pathf, 'CodesDrugsDict_NonTSC',Ext, sep=''), header=T, sep=' ')
CodesDrugDict <- unique(c(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC[, 2])))

CodesDrugsDict_TSC <- CodesDrugsDict_TSC_train 
dictMatch <- match(as.character(CodesDrugsDict_TSC[, 2]), as.character(CodesDrugsDict_NonTSC[, 2]))
length(dictMatch[is.na(dictMatch)])
TSCcodenoMatch <- as.character(CodesDrugsDict_TSC[, 2])[which(is.na(dictMatch))]
dictMatch_nonTSC <- match(as.character(CodesDrugsDict_NonTSC[, 2]), as.character(CodesDrugsDict_TSC[, 2]))
length(dictMatch_nonTSC[is.na(dictMatch_nonTSC)])
NonTSCcodenoMatch <- as.character(CodesDrugsDict_NonTSC[, 2])[which(is.na(dictMatch_nonTSC))]
nomatchedCodeInNonTSC <- lapply(X=1:length(TSCcodenoMatch), function(X){grep(paste('^',TSCcodenoMatch[X], '$', sep=''), as.character(CodesDrugsDict_NonTSC[, 2]), ignore.case=T, value=T)})
length(lapply(NonTSCcodenoMatch, function(x){length(x)==0}))
ForTSC_notfindInNonTSC <- unlist(lapply(nomatchedCodeInNonTSC, function(x){length(x)==0}))
TSCcodenoMatch[ForTSC_notfindInNonTSC]
TSCcodenoMatch[!ForTSC_notfindInNonTSC]
TSCcodenoMatch_df <- data.frame(DrugCode=TSCcodenoMatch)
write.csv(TSCcodenoMatch_df, file=paste(pathf, 'TSCNotMatchedInNonTSC.csv', sep=''))

#CodeDrug <- rbind(TSCCodeDrug, NonTSCCodeDrug)
TSCAgeGender <- TSCAgeGender_train
TSCCodeDrug <- TSCCodeDrug_train
AgeGender <- rbind(TSCAgeGender, NonTSCAgeGender)
tm <- Sys.time()
matrix_TSC <- matrix(0, nr=nrow(TSCCodeDrug), nc=length(CodesDrugDict))
matrix_NonTSC <- matrix(0, nr=nrow(NonTSCCodeDrug), nc=length(CodesDrugDict))
#matrix_TSC <- matrix(0, nr=10, nc=length(CodesDrugDict))
#matrix_NonTSC <- matrix(0, nr=500, nc=length(CodesDrugDict))

i <- 1
for(i in 1:nrow(matrix_TSC)){
    codeNm <- TSCCodeDrug[i,][-1]
    codeIdx <- match(as.vector(t(codeNm)), CodesDrugDict)
    codeIdx1 <- codeIdx[!is.na(codeIdx)]
    #value <- ifelse(is.na(codeIdx), 0, 1)
    matrix_TSC[i, codeIdx1] <- 1
    #matrix[i, 2:6] <- AgeGender[i, 2:6]
}
codeDemo_mtx_TSC<- cbind(Response=rep(1, nrow(matrix_TSC)), TSCAgeGender[,-1], matrix_TSC)

for(i in 1:nrow(matrix_NonTSC)){
    codeNm <- NonTSCCodeDrug[i,][-1]
    codeIdx <- match(as.vector(t(codeNm)), CodesDrugDict)
    codeIdx1 <- codeIdx[!is.na(codeIdx)]
    #value <- ifelse(is.na(codeIdx), 0, 1)
    matrix_NonTSC[i, codeIdx1] <- 1
    #matrix[i, 2:6] <- AgeGender[i, 2:6]
}
codeDemo_mtx_NonTSC<- cbind(Response=rep(-1, nrow(matrix_NonTSC)), NonTSCAgeGender[,-1], matrix_NonTSC)
codeDemo_mtx <- rbind(codeDemo_mtx_TSC, codeDemo_mtx_NonTSC)
codeDemo <- as.data.frame(codeDemo_mtx)
colnames(codeDemo) <- c('Response', names(TSCAgeGender)[-1], as.character(CodesDrugDict))
cat('\ntime used is: ', Sys.time()-tm, '\n')
rm.featureIdx <- which(apply(codeDemo, 2, sum)==0)
codeDemo[, as.vector(rm.featureIdx)] <- NULL
codeDemo2 <- codeDemo[,-rm.featureIdx]
cor <- cor(codeDemo2[, 1],codeDemo2[, -1])
cor[order(abs(cor))]
save.image(paste(pathf, 'find_feature_may06.RData'))

correlation <- cbind(colnames(cor)[rev(order(abs(cor)))], cor[rev(order(abs(cor)))])
load(paste(pathf,'find_feature_may06.RData'))
save(codeDemo, file=paste(pathf, 'codeDemo_baseOn334TSCDic.RData', sep=''))


























for ( i in 1: tsc_patnum){
  
  patv_temp <- TSC_drugcode[i,]
  bindex <- which(patv_temp=='-')
  patv <- patv_temp[-bindex]
  
  var_index <- CodeDict[CodeDict[,2] %in% patv[1,], 1]
  
  TSC_matrix[i, var_index] <- 1
    
}

tm<-Sys.time()
for ( i in 1: non_tsc_patnum){
  
  patv_temp <- Non_TSC_drugcode[i,]
  bindex <- which(patv_temp=='-')
  patv <- patv_temp[-bindex]
  
  var_index <- CodeDict[CodeDict[,2] %in% patv[1,], 1]
  
  Non_TSC_matrix[i, var_index] <- 1
  
    cat('iterition :',i,'\n')
}
Sys.time()-tm

#row.names(Non_TSC_demo[Non_TSC_demo$Id=='15077272',])
#head(Non_TSC_demo,10000)
#Non_TSC_demo[6104,]
#vec <- Non_TSC_drugcode[6104,]
#din<-which(vec=='-')
#vec2<-vec[-din]
#vin <- CodeDict[CodeDict[,2] %in% vec2[1,], 1]
#nn <- CodeDict[CodeDict[,2] %in% vec2[1,], 2]
#a<-rep(0,nrow(CodeDict)+6)
#a[vin] <-1


TSC_label <- as.matrix(rep(1, nrow(TSC_demo)))
TSC_matrix[,1:5]<-as.matrix(TSC_demo[,2:6])
TSC_pat <- cbind(TSC_label, TSC_matrix)

Non_TSC_label <- as.matrix(rep(-1, nrow(Non_TSC_demo)))
Non_TSC_matrix[,1:5] <- as.matrix(Non_TSC_demo[,2:6])
Non_TSC_pat <- cbind(Non_TSC_label, Non_TSC_matrix)

Total_pat <- rbind(TSC_pat, Non_TSC_pat)

t_pat <- data.frame(Total_pat)

sum <- apply(t_pat, 2, sum)
dindex <- which(sum==0)
# no variable ==0

# correlation index should plus 1  to get the feature index
new_rest_pat <- Total_pat
cor_pearson <- cor(new_rest_pat[,1], new_rest_pat[,-1], method='pearson' )
cor_sparman <- cor(new_rest_pat[,1], new_rest_pat[,-1], method='spearman' )

#non_tsc<- new_rest_pat[new_rest_pat[,1]==-1,]
#sum_nontsc <- apply(non_tsc[,8:26023],2,sum)


# positive correlation greater than 0.3
pos_cor_p_rank <- which(cor_pearson[1,] >= 0.252)
pos_cor_s_rank <- which(cor_sparman[1,] >=0.252)

# the least negative correlation 
neg<- sort(cor_pearson)[1:10]

neg_cor_p_rankd <- which(cor_pearson[1,]==min(cor_pearson[1,]))
neg_cor_s_rankd <- which(cor_sparman[1,]==min(cor_sparman[1,]))
# that is the patient id

pos_rank <- pos_cor_p_rank

feature_name_pos <- as.matrix(CodeDict[CodeDict[,1] %in% pos_rank, 2])
cor_p_0.3 <- as.matrix(cor_pearson[,pos_cor_p_rank])[-1,]
cor_p_ne <- as.matrix(cor_pearson[,pos_cor_p_rank])[1,]


final_fname_cor <- data.frame(cbind(feature_name_pos,round(cor_p_0.3,2)))
colnames(final_fname_cor) <- c('Feature_Name', 'Pearson_Correlation')

outpath <- 'D:\\Hui\\TSC project\\Results\\feature correlation\\'

write.csv(final_fname_cor, paste(outpath,'Feature_Name_Correlation_v5.csv',sep=''), quote=F, row.names=F )

# reading in description
all_desc <- read.csv('D:\\Hui\\TSC project\\Results\\feature correlation\\feature_desc_v5.csv',header=T)

#read_desc <- read.csv('D:\\Hui\\TSC project\\Results\\feature correlation\\Read_feature_desc.csv',header=T)
#drugsub_desc <- read.csv('D:\\Hui\\TSC project\\Results\\feature correlation\\Drugsubstance_feature_desc.csv',header=T)
#icd_desc <- read.csv('D:\\Hui\\TSC project\\Results\\feature correlation\\ICD_feature_desc.csv',header=T)

#distribution
OUT<- file(paste(outpath, 'Distribution_v5.txt',sep='') , 'w')

for(i in c(pos_cor_p_rank)){
    
  re<- table(new_rest_pat[,i+1],new_rest_pat[,1])
  
  #rownames(re)<-c('neg', 'pos')
  nm <- rownames(re)
  
  fn <- all_desc[which(all_desc[,1]==as.vector(CodeDict[CodeDict[,1]==i, 2])),1]
  fn_desc <- all_desc[which(all_desc[,1]==as.vector(CodeDict[CodeDict[,1]==i, 2])),2]
  
  writeLines(paste(fn, fn_desc,  sep='\t\t\t\t') , OUT)
  writeLines(paste('distribution', 'Non TSC', 'TSC' ,  sep='\t\t') , OUT)
  writeLines(paste(rep('-' , 150) , collapse='') , OUT)
    
    for(j in 1:nrow(re)){
      writeLines(paste(nm[j] , re[j,1]  ,re[j,2],sep='\t\t') , OUT)
    }
    writeLines('' , OUT)
 
 cat('iteration ', i, 'name',fn,'\n')
  }
  close(OUT) 

# find the description
102
262
103
511
table(new_rest_pat[,103],new_rest_pat[,1])
table(new_rest_pat[,263],new_rest_pat[,1])
table(new_rest_pat[,104],new_rest_pat[,1])
table(new_rest_pat[,512],new_rest_pat[,1])





