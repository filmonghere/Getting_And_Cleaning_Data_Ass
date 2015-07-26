# Getting And Cleaning Data Assignment Submission

## Introduction

This repository contains a raw data and description of how the data was obtained and processed. This repository was created as a submission to assignment No. 1 for the course GETTING AND CLEANING DATA. The objective of assignment is to make the raw data tidy enough for further analysis. The specific tasks include:

•	Merging the training and the test sets to create one data set.
•	Extracting only the measurements on the mean and standard deviation for each measurement. 
•	Using descriptive activity names to name the activities in the data set
•	Appropriately labeling the data set with descriptive variable names. 
•	From the data set in TASK 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject.


## Project Name

Human Activity Recognition Using Smartphones Dataset [1]

## Data Source

The data sets used in this assignment were collected using accelerometers from the Samsung Galaxy S smartphones. The data sets are located on: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip The data was provided in a zipped folder and had to be unzipped. 
Description of the data is available in this link http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
For more information about the data collection, readers are referred to [1].

*One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained which is listed above. … The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity were captured at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data …  The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.*[1]

## Description of files containing the data

Source files contain the raw data were provided by the instructor of Getting and Cleaning Data. The source files obtained training and testing datasets as well the labels for activities that the data focused on. The source files include:
* `features_info.txt`: Shows information about the variables used on the feature vector.
* `features.txt`: List of all features.
* `activity_labels.txt`: Links the class labels with their activity name.
* `train/X_train.txt`: Training set.
* `train/y_train.txt`: Training labels.
* `test/X_test.txt`: Test set.
* `test/y_test.txt`: Test labels.
* `train/subject_train.txt`: Each row identifies the subject who performed the activity for each window sample. The entries in thee data sets are in the range from 1 to 30.

### Variables/Measurements in the data sets

The activity labels consist of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING while the subjects that the experiment was conducted were labeled 1 through 30. 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. The acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ). The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm. Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. For each measurement the mean, standard deviation, and mean frequency were calculated. [1]

After the data was cleaned and made tidy, the variable names include:

SubjectID, Class_Name, tBodyAcc_mean_X, tBodyAcc_mean_Y, tBodyAcc_mean_Z, tBodyAcc_std_X, tBodyAcc_std_Y, tBodyAcc_std_Z, tGravityAcc_mean_X, tGravityAcc_mean_Y, tGravityAcc_mean_Z, tGravityAcc_std_X, tGravityAcc_std_Y, tGravityAcc_std_Z, tBodyAccJerk_mean_X, tBodyAccJerk_mean_Y, tBodyAccJerk_mean_Z, tBodyAccJerk_std_X, tBodyAccJerk_std_Y, tBodyAccJerk_std_Z, tBodyGyro_mean_X, tBodyGyro_mean_Y, tBodyGyro_mean_Z, tBodyGyro_std_X, tBodyGyro_std_Y, tBodyGyro_std_Z, tBodyGyroJerk_mean_X, tBodyGyroJerk_mean_Y, tBodyGyroJerk_mean_Z, tBodyGyroJerk_std_X, tBodyGyroJerk_std_Y, tBodyGyroJerk_std_Z, tBodyAccMag_mean, tBodyAccMag_std, tGravityAccMag_mean, tGravityAccMag_std, tBodyAccJerkMag_mean, tBodyAccJerkMag_std, tBodyGyroMag_mean, tBodyGyroMag_std, tBodyGyroJerkMag_mean, tBodyGyroJerkMag_std, fBodyAcc_mean_X, fBodyAcc_mean_Y, fBodyAcc_mean_Z, fBodyAcc_std_X, fBodyAcc_std_Y, fBodyAcc_std_Z, fBodyAcc_meanFreq_X, fBodyAcc_meanFreq_Y, fBodyAcc_meanFreq_Z, fBodyAccJerk_mean_X, fBodyAccJerk_mean_Y, fBodyAccJerk_mean_Z, fBodyAccJerk_std_X, fBodyAccJerk_std_Y, fBodyAccJerk_std_Z, fBodyAccJerk_meanFreq_X, fBodyAccJerk_meanFreq_Y, fBodyAccJerk_meanFreq_Z, fBodyGyro_mean_X, fBodyGyro_mean_Y, fBodyGyro_mean_Z, fBodyGyro_std_X, fBodyGyro_std_Y, fBodyGyro_std_Z, fBodyGyro_meanFreq_X, fBodyGyro_meanFreq_Y, fBodyGyro_meanFreq_Z, fBodyAccMag_mean, fBodyAccMag_std, fBodyAccMag_meanFreq, fBodyBodyAccJerkMag_mean, fBodyBodyAccJerkMag_std, fBodyBodyAccJerkMag_meanFreq, fBodyBodyGyroMag_mean, fBodyBodyGyroMag_std, fBodyBodyGyroMag_meanFreq, fBodyBodyGyroJerkMag_mean, fBodyBodyGyroJerkMag_std, fBodyBodyGyroJerkMag_meanFreq

### Data cleaning and processing

The training and testing data sets were combined to create one big data set. The same was also done for the label names of the subjects. 

For the purpose of this assignment, measurements containing only the mean and the standard deviation were required. Therefore, from the combined data sets, variables containing the mean and standard deviations of measurements were subseted for further analysis.

Some of the variable names contained unnecessary characters, e.g., many of the column names contained the character ‘()’ which makes the variable names difficult to read for humans. This was removed from all the column names. Moreover, the column names consisted ‘-’ to separate words making the name. All these were changes into underscore ‘_’ for ease of reading the names by humans.

## Result

The result from this work is getting a tidy data set for further analysis. The tidy data was created and saved as a Tidy_Data_Set.txt.

## Further analysis / Classification of human activities from smartphones

Even though this was not required for this course, the author tried to classify activities based on the measurements obtained. Bayes classification was used for this purpose and the accuracy of the classification was found to be 84% - which is relatively good prediction accuracy.

### References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

