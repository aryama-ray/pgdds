############################ Handwritten Digit Recognition Problem #######################################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation & EDA
# 4. Model Building 

# 5  Cross validation 
# 6. Checking overfitting - Non-Linear - SVM

###########################################################################################################################

# 1. Business Understanding: 

# We have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices.
# The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 

###########################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------#
# INSTALL PACKAGES AND LOAD REQUIRED LIBRARIES
#-------------------------------------------------------------------------------------------------------------------------# 

library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)

#-------------------------------------------------------------------------------------------------------------------------#
#**********************DATA SOURCING**************************************************************************************#
#-------------------------------------------------------------------------------------------------------------------------#

digit_recon_train <- as.matrix(fread("./mnist_train.csv",header = F))
digit_recon_test <- as.matrix(fread("./mnist_test.csv",header = F))

dim(digit_recon_train)
#digit_recon_train matrix has 60000 rows and 785 variables

#Since digit_recon_train data is very large dataset, for faster processing we are sampling out 15% of the dataset
set.seed(100)
digit_recon_indices = sample(1:nrow(digit_recon_train), 0.15*nrow(digit_recon_train))
digit_recon_input = digit_recon_train[digit_recon_indices,]                      

#------------------------------------------------------------------------------------------------------------------------#
# *********************DATA UNDERSTANDING *******************************************************************************#
#------------------------------------------------------------------------------------------------------------------------#

#MNIST database is of handwritten digits annd consists of pixel values of each digits along with their labels. 
dim(digit_recon_input) #Input dataset digit_recon_input now has 9000 rows and 785 variables

#Data set has 785 variables- first variable - V1 denotes the label of each digit,
# and rest of the variables denotes 784 pixels corresponding to those digits.

#Checking NA values in all the attributes
apply(digit_recon_input,2,function(x)sum(is.na(x)))
sum(is.na(digit_recon_input)) # There is no Na value in the dataset

#Chcking structure of the matrix
str(digit_recon_input)

#Checking unique values in all the attributes
uni_len <- apply(digit_recon_input,2,function(x) length(unique(x)))
# V1 which is intended outcome from 28*28 pixels values are having 10 unique values i.e 0-9 and 
# rest of the pixels are having different number of unique values  which signifies that there is  
# no unique identifier in this dataset

max(uni_len) # max value per pixel is 256

#Checking first few data elements from the input dataset
head(as.data.frame(digit_recon_input[,1:28]))

#------------------------------------------------------------------------------------------------------------------------#
# ******************** Data Preparation and EDA *************************************************************************#
#------------------------------------------------------------------------------------------------------------------------#
#
# 1. Identifying the digits from the 28*28 black and white pixels
symbol_1 <- matrix(as.numeric(digit_recon_input[11,-1]),nrow=28)
image(symbol_1,col=grey.colors(255)) #image looks like digit- '8'

# 2. Checking Intensity of each digit by calculating average pixel value per digit
digit_intensity <- apply(digit_recon_input[,-1], 1, mean)
digit_recon_input <- as.data.frame(digit_recon_input)
#We are adding a derived variable for digit image intesity in the dataset
digit_recon_input$digit_intensity <- digit_intensity
digit_avg_intensity <- aggregate(digit_intensity ~ V1,data=digit_recon_input,FUN = mean)

#Plot to check average intensity of each digit.
ggplot(digit_avg_intensity,aes(as.factor(V1),digit_intensity))+geom_col()

# Column chart shows that digit 0 has highest intesty whereas digit 1 has lowest intensity.

#3.Lots of pixel columns are having zero values only for their corresonding digit label, 
# but considering the fact that it might affect the pixel positions we are not removing those pixel columns.

digit_recon_input[,which(sapply(digit_recon_input,sum)==0)]

#4.Since the digit label which is the outcome of the pixel image is in numeric format, we need to convert it 
# to factor variable.
str(digit_recon_input)
digit_recon_input$V1 <- as.factor(digit_recon_input$V1)

##-----------------End of Data Preparation and EDA ---------------------------------------------------------------------##

#------------------------------------------------------------------------------------------------------------------------#
# *********************Model Building ***********************************************************************************#
#------------------------------------------------------------------------------------------------------------------------#
#
# 1. Training annd Test Data preparation
# Spliting the given train input dataset into 70:30 ratio of digit_train and digit_test datasets
set.seed(100)

svm_indices = sample.split(digit_recon_input$V1, SplitRatio = 0.7)

digit_train = digit_recon_input[svm_indices,]

digit_test = digit_recon_input[!(svm_indices),]

# 
# 2.Model Building - We first build a linear SVM model with Cost =1 

digit_linear_model <- ksvm(V1 ~ ., data = digit_train,scale = FALSE,C=1)

# We run the prediction on test data set - digit_test based on the first linear SVM model
pred_digit_linear <- predict(digit_linear_model, digit_test)

# Creating Confusion Matrix - to find accuracy, Sensitivity and specificity of the first linear SVM model
confusionMatrix(pred_digit_linear, digit_test$V1)

# Performing 5 fold cross validation for first linear SVM  model
# Train parameter preparation - grid of C values. 
linear_grid <- expand.grid(C=seq(1, 5, by=1))
# Train parameter preparation - trControl value for 5 fold cross validation and parallel processing
trainControl1 <- trainControl(method="cv", number=5,allowParallel=TRUE)
# Train parameter preparation - metric value
metric <- "Accuracy"
# Cross validation on digit_train dataset
fit.digi_svm_linear <- train(V1~., data=digit_train, method="svmLinear", metric=metric, 
                             tuneGrid=linear_grid, trControl=trainControl1)

# Printing cross validated linear SVM model
print(fit.digi_svm_linear)

# Results : first Linear SVM 
# C  Accuracy   Kappa    
# 1  0.9091956  0.8990802
# 2  0.9091956  0.8990802
# 3  0.9091956  0.8990802
# 4  0.9091956  0.8990802
# 5  0.9091956  0.8990802
# Hence the maximum accuracy is 90.92% with Linear SVm  model with cost =1.

# Plotting "fit.digi_svm_linear" results
plot(fit.digi_svm_linear)

# 
# 3.Model Building - We build another linear SVM model with Cost =10
digit_linear_model2 <- ksvm(V1 ~ ., data = digit_train,scale = FALSE,C=10)

# We run the prediction on test data set - digit_test based on the second linear SVM model
pred_digit_linear2 <- predict(digit_linear_model2, digit_test)

# Creating Confusion Matrix - to find accuracy, Sensitivity and specificity of the second linear SVM model
confusionMatrix(pred_digit_linear2, digit_test$V1)

# Performing 5 fold cross validation for second linear SVM  model
# Train parameter preparation - grid of C values. 
linear_grid2 <- expand.grid(C=seq(1, 5, by=1))
# Train parameter preparation - trControl value for 5 fold cross validation and parallel processing
trainControl2 <- trainControl(method="cv", number=5,allowParallel=TRUE)
# Train parameter preparation - metric value
metric <- "Accuracy"
# Cross validation on digit_train dataset
fit.digi_svm_linear2 <- train(V1~., data=digit_train, method="svmLinear", metric=metric, 
                              tuneGrid=linear_grid2, trControl=trainControl2)

# Printing the result for cross validated linear SVM model
print(fit.digi_svm_linear2)

# Results : Linear SVM
# C  Accuracy   Kappa    
# 1  0.9076077  0.8973151
# 2  0.9076077  0.8973151
# 3  0.9076077  0.8973151
# 4  0.9076077  0.8973151
# 5  0.9076077  0.8973151
# Hence the maximum accuracy is 90.76% with second Linear SVm  model with cost=10.
#Accuracy decreases here from first linear svm model.

# Plotting "fit.digi_svm_linear" results
plot(fit.digi_svm_linear2)


# 4.Model Building -Since there is still scope to achieve higher accuracy We build a radial basis SVM model 

digit_rbf_model1 <- ksvm(V1~.,data=digit_train,scale=FALSE,kernel='rbfdot')

print(digit_rbf_model1)
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  1.62536949984554e-07 
# Number of Support Vectors : 2790
# Training error : 0.019686 

# We run the prediction on test data set - digit_test based on the RBF SVM model
pred_digit_rbf <- predict(digit_rbf_model1,digit_test)
# Creating Confusion Matrix - to find accuracy, Sensitivity and specificity of the RBF SVM model
confusionMatrix(pred_digit_rbf,digit_test$V1)

# Confusion matrix: KSVM test result - Overall Statistics
# Accuracy : 0.9574          
# 95% CI : (0.9491, 0.9647)
# No Information Rate : 0.107           
# P-Value [Acc > NIR] : < 2.2e-16       
# Kappa : 0.9527          
# Mcnemar's Test P-Value : NA 

# Now RBF kernel is giving 95.74% accuracy which is better than previous linear svm models.

# Hyperparameter tuning and 5 fold Cross Validation - Non-Linear - SVM 

set.seed(1234)

# Train parameter preparation :
# tunegrid value: digit_rbf_model1 shows the sigma value as 1.62536949984554e-07. 
# Hence we select sigma value around it and select cost value ranging from 0.5 to 1.5
digi_grid <- expand.grid(.sigma=c(1.6275e-07,1.6281e-07,1.6290e-07,1.6300e-07), .C=c(0.5,1,1.5))

# trControl value :for 5 fold cross validation and parallel processing
trainControl <- trainControl(method="cv", number=5,allowParallel=TRUE)
# metric value :
metric <- "Accuracy"
# Cross validation on digit_train dataset
fit.digi_svm <- train(V1~.,data=digit_train, method="svmRadial",metric=metric,tuneGrid=digi_grid,
                      trControl=trainControl)

# Printing the result for cross validated non-linear RBF model
print(fit.digi_svm)

##Result of fit.digi_svm
# sigma     C    Accuracy   Kappa    
# 1.55e-07  0.1  0.9050642  0.8944958
# 1.55e-07  0.5  0.9342752  0.9269600
# 1.55e-07  1.0  0.9452276  0.9391312
# 1.60e-07  0.1  0.9056986  0.8952008
# 1.60e-07  0.5  0.9342752  0.9269600
# 1.60e-07  1.0  0.9461802  0.9401897
# 1.65e-07  0.1  0.9066510  0.8962593
# 1.65e-07  0.5  0.9349104  0.9276659
# 1.65e-07  1.0  0.9466572  0.9407199

#Accuracy was used to select the optimal model - fit.digi_svm using the largest value.
#The final values used for the model were sigma = 1.65e-07 and C = 1.
# Highest accuracy is 94.66% with sigma = 1.65e-07 and C = 1 and this is the highest among all the models.

# Plotting "fit.digi_svm" results
plot(fit.digi_svm)
#
##-----------------End of Model Building and Cross Validation-----------------------------------------------------------##

#------------------------------------------------------------------------------------------------------------------------#
# *****************Checking overfitting - Non-Linear - SVM **************************************************************#
#------------------------------------------------------------------------------------------------------------------------#
#
# We are validating the model results on the given digit_recon_test dataset which is completely untrained.
# Also, we use fit.digi_svm - radial basis cross validation model as we have highest accuracy with this model.

#Before running prediction on test data we need to prepare the test dataset.
#Converting the test dataset into dataframe.
digit_recon_test <- as.data.frame(digit_recon_test)
#Inserting derived variable - digit intensity to make the dataset alike train dataset
digit_intensity <- apply(digit_recon_test[,-1], 1, mean)
digit_recon_test$digit_intensity <- digit_intensity
#Factorising the outcome variable V1 for digit label
digit_recon_test$V1 <- as.factor(digit_recon_test$V1)

#Running prediction on whole digit_recon_test dataset.
predict_digit_non_linear<- predict(fit.digi_svm, digit_recon_test[,-1])
# Creating Confusion Matrix - to find accuracy, Sensitivity and specificity of the test dataset.
confusionMatrix(predict_digit_non_linear, digit_recon_test$V1)

#Test Result:Overall Statistics

#  Accuracy : 0.9555          
#  95% CI : (0.9513, 0.9595)
#  No Information Rate : 0.1135          
#  P-Value [Acc > NIR] : < 2.2e-16       

#  Kappa : 0.9505          
#  Mcnemar's Test P-Value : NA 

##-----------------End of Checking overfitting - Non-Linear - SVM ------------------------------------------------------##

##########################################################################################################################
# Model Interpretation
# 1. With non linear SVM kernel, we are able to predict the handwritten digits with 95.74% accuracy
# 2. After cross validation it gives us highest accuracy of 94.66% with sigma = 1.65e-07 and C = 1
# 3. The final model isused to predict test data which has been provided separately.The model has been able to 
#    recognize hand written  digits with 95.55% accuracy.

##########################################################################################################################

