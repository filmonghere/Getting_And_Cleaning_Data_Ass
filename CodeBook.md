# CodeBook

This code book describes how the R scrip written performs the desired work and what libraries are required for the analysis to be successful. 
Moreover, the data set, its variables, transformations made and other related works that were performed to clean up the data and make it tidy.

## The data source

The data sets used in this assignment were collected using accelerometers from the Samsung Galaxy S smartphones. The data sets are located on: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip The data was provided in a zipped folder and had to be unzipped. 
Description of the data is available in this link http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
For more information about the data collection, readers are referred to [1].

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

## Variables/Measurements in the data sets

The activity labels consist of WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING while the subjects that the experiment was conducted were labeled 1 through 30. 

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. The acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ). The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm. Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. For each measurement the mean, standard deviation, and mean frequency were calculated. [1]

## Data cleaning procedures

The process of making the data tidy includes the following 5 procedures:

•	Merging the training and the test sets to create one data set.
•	Extracting only the measurements on the mean and standard deviation for each measurement. 
•	Using descriptive activity names to name the activities in the data set
•	Appropriately labeling the data set with descriptive variable names. 
•	From the data set in TASK 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject.

## How ```run_analysis.R``` implements the above steps:

* Required  librareis are ```plyr``` and ```reshapre2``` while the library  ```class``` is optional
* Load both the test data set and train data set
* Load the labels for features and activities
* Combine the test and training data sets as well as the labels of the features and activities
* Subset the columns containing the mean and standard deviation of the measurements
* Perform some transformation to the data to obtain the average of each activity for each subject.
* write the required tidy data sets in to a text file

## Reference

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012.

