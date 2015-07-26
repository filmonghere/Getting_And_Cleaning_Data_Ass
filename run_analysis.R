
#####################################################################

########   GETTING AND CLEANING DATA  ###############################

###  ASSIGNMENT NO. 1 

## The objective of this analysis is to make the raw data tidy enough for further analysis.

##################################################################

###  The data for this analysis was downloaded and saved in a local directory. 
###  The data was located on: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
###  The data was in a zipped folder and had to be unzipped
###  Description of the data is available in this link http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

###########################################################

# Set the directory for easy of reading the raw data, writing tidy data and other outputs

setwd("D:/Coursera courses/Getting and Cleaning Data/Assignment/UCI HAR Dataset")

getwd()   # confirms the directory set


############################
### Reading the data
#############################

# Importing the test data
X_Test <- read.table("test/X_test.txt")
Y_Test <- read.table("test/y_test.txt")
Subject_Test <- read.table("test/subject_test.txt")

# Importing the training data
X_Train <- read.table("train/X_train.txt")
Y_Train <- read.table("train/y_train.txt")
Subject_Train <- read.table("train/subject_train.txt")


###############################################
## TAST 1) Merge the training and test sets to create one data set.
#######################################

# binding together the data sets of X_Test with X_Train, Y_Test with Y_Test, and subject_test with subject_train to creat one test data set
Combined_Data <- rbind(X_Test, X_Train)
Combined_labels <- rbind(Y_Test, Y_Train)
All_Subjects <- rbind(Subject_Test, Subject_Train)

# The variables of the data sets appear to be not named. 
#The names of the variables is provided in a separate file and thus the column names of the binded data sets need to be renamed
Features <- read.table("features.txt")
colnames(Combined_Data) <- Features[,2]
names(Combined_Data)

###############################################
## TASK 2) Extracting only the measurements on the mean and standard deviation for each measurement
#######################################

# the variable names in the data sets containing mean and standard deviations are represented using mean() and std()
# the grepl function can identify the column names containing mean() and std()s
ColumnsOfInterest <-  grepl("mean()",colnames(Combined_Data)) | grepl("std()",colnames(Combined_Data))

# subseting the column names containg mean() and std()
Data_Mean_Std <- Combined_Data[,ColumnsOfInterest]


###############################################
## TASK 3) Using descriptive activity names to name the activities in the data set
#######################################

# Reading the activities 
Activities <- read.table("activity_labels.txt")

# the activity names are descriptive enough and thus no need of renaming the activity names provided with the raw data set


###############################################
## TASK 4) Appropriately labeling the data set with descriptive activity names. 
#######################################

# The labels in the data set Combined_labels should be replaced with the the activity names
str(Combined_labels) # this shows the labels are read as intiger, better to convert them into factors
Labels_as_factors <- as.factor(Combined_labels[,1])

# Replacing the factored lebels by the activity names
library(plyr)
Labels_as_factors <- mapvalues(Labels_as_factors,from = as.character(Activities[,1]), to = as.character(Activities[,2]))

# Verifying the length of the activity names labels with the number of rows of the data sets
length(Labels_as_factors)
dim(Data_Mean_Std)

# Combining the labels with the data sets using cbind function
Data_Mean_Std <- cbind(Labels_as_factors, Data_Mean_Std)  

# Renaming the column name of the column containing the labels
colnames(Data_Mean_Std)[1] <- "activity"

# The ID of subjects should also be included in the data sets as they might be needed for further analysis down the stream
Data_Mean_Std <- cbind(All_Subjects, Data_Mean_Std)
colnames(Data_Mean_Std)[1] <- "subject"  # rename the column name

# using descriptive variable names 
names(Data_Mean_Std)

# All variable names contain an upen and closed brackets, i.e., '()'. I think this is redundant and needs to be removed from the variable names
# Also, column names contain a hyphen, i.e., '-'. In my opinion, it is desirable to use underscore instead, i.e., '_'
VarNames <- names(Data_Mean_Std)
VarNames <- sub("[)]", "", VarNames)
VarNames <- sub("[(]", "", VarNames)
VarNames <- sub("-", "_", VarNames)
VarNames <- sub("-", "_", VarNames)

# changing the column names with the descriptive variable names
colnames(Data_Mean_Std) <- VarNames 

# Check for missing values 
sum(as.numeric(sapply(Data_Mean_Std, function (x) {sum(is.na(x))}))) # if the output is zero, then the data contains no missing data

# Now the data set X_mean is tidy enough for further analysis


###############################################
## TASK 5) Creating a second, independent tidy data set with the average of each variable for each activity and each subject 
#######################################

# The average of each variable for each subject can be obtained by reshaping the data
library(reshape2)

Melted_Data <-  melt(Data_Mean_Std,id.vars=c("subject","activity"))        # melt the data by the selected variables 
Average_For_Subjects <- dcast(Melted_Data, subject + activity ~ ..., mean)         # aggregate the data to find the average of each variable for each subject

# check for missing values
sum(as.numeric(sapply(Average_For_Subjects, function (x) {sum(is.na(x))})))

## Now the data set Average_For_Subjects is tidy enough for further analysis


###############################################
## Writing the tidy data set for submission as an answer to the assignment for the "course Getting and Cleaning Data"
#######################################

write.table(Average_For_Subjects, "Tidy_Data_Set.txt", row.names=FALSE)


#####################################################################
#  Some further analysis on the tidy data.
#  Let's try to apply some classification algorithm to determine whether the subjects are 
#  WALKING   WALKING_UPSTAIRS   WALKING_DOWNSTAIRS   SITTING   STANDING     LAYING 
#  For this purpose, I will apply Bayes Classification algorithm 
############################################
library(class) 
library(e1071) 

# performing the classification
classifier<-naiveBayes((Data_Mean_Std[,3:ncol(Data_Mean_Std)]), factor(Data_Mean_Std[,2])) 

# Missclasification Rates
BayesGroup <- predict(classifier, (Data_Mean_Std[,3:ncol(Data_Mean_Std)]))
ctNAIVE <- table(BayesGroup, factor(Data_Mean_Std[,2]))
ctNAIVE 
diag(prop.table(ctNAIVE , 1)) # returns the accuracy of the classification output

##    WALKING   WALKING_UPSTAIRS   WALKING_DOWNSTAIRS     SITTING      STANDING        LAYING 
##   0.9201365      0.7467249            0.7759336      0.8093246      0.8025144      0.9579534 

# The accuracy of the activity classification is very good, particularly for activities LAYING and WALKING

# total percent correct
sum(diag(prop.table(ctNAIVE)))
# 0.836489

# OVERALL, the accuracy of the classification output was found to be 84% which is pretty good

##########################################
##################################
#######################
###############
