#for feature number of TSC used in SVM (train+validataion)

inPath <- 'D:\\RareDisease_TSC\\Data\\Dict_Ind_2\\2n'

infileAll <- list.files(path=inPath, all.files=F, full.names=F, recursive=F)

dictFile <- grep('^CodesDrugsDict\\d{1,2}\\W\\w{3}', infileAll, perl=T, value=T)

features <- unique(unlist(lapply(dictFile, function(X){read.table(paste(inPath, X, sep='\\'), header=T, sep=' ')[, 2]})))

