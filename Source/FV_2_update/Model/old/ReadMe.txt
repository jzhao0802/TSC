
Files and their brief description.

1. main.R
Main program for identification of TSC patients  and  undiagnosed patients who may have TSC.
It perform one-class and binary classification, builds model using training set, tunes
parameters using validation set, and performs evaluation on test set 


2. InvkSVM.R
Builds a model by using SVMs with linear / polynomial / RBF kernels


3. PredSVM.R
Evaluates model on test data                   


________________________________________________________________________________________


How to execute the code:

Set input and output file names, and other variables accordingly in function "main.R".
Execute this function to build and evaluate models. The function, "main.R" invokes required
functions.