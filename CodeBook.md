---
title: "CodeBook.md"
author: "Jessica Pitherin"
date: "May 22, 2015"
output: html_document
---

### run_analysis.R

If the zipped file from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip is in the R working directory, this R script will end with two tidy data sets:  
1. the combined training and testing data sets  
2. the average values of each variable for each activity and each subject from the first data set (this data set is printed to the working directory as 'SamsungSummary.txt')  
  
## UCI HAR Dataset relevant contents 
###(taken from the UCI HAR Data set README.txt)  

>"The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data."

**The dataset includes the following files:**  
* 'README.txt'  
* 'features_info.txt': Shows information about the variables used on the feature vector.  
* 'features.txt': List of all features.  
* 'activity_labels.txt': Links the class labels with their activity name.  
* 'train/X_train.txt': Training set.  
* 'train/y_train.txt': Training labels.  
* 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. (Ranges from 1 to 30.)  
* 'test/X_test.txt': Test set.  
* 'test/y_test.txt': Test labels.   
* 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. (Ranges from 1 to 30.)    

## The following is a list of variables in the 'SamsungSummary.txt' generated by 'run_analysis.R'
* subject: Test/training subject ID.  
* activity: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING  
* group: 1 = train, 2 = test  
* *The rest of the variables are the means and standard deviations extracted for each measurement described in 'features_info.txt'.*  

## Description of transformations and data cleaning  
* Extracted only the measurements on the mean & standard deviation for each measurement. The relevant columns/variables were determined by visually inspecting the 'features.txt' file.  
* Labelled the data frames with the descriptive variable names from 'features.txt' and 
```{r}
names(Y_test) <- "activity"
names(Y_train) <- "activity"
names(subject_test) <- "subject"
names(subject_train) <- "subject"
```
* Used descriptive activity names to name activities in the data frame by matching the numbers in 'Y_test' and 'Y_train' with activity names from 'activity_labels.txt'.  
* Used `cbind()` to merge all test data together into the data frame 'test', including a new column with the value "test".  
* Same as step 4, but with the train data.  
* Used `rbind()` to merge training and test sets to create one data frame named 'SamsungData'.  
* Used `group_by()` and `summarise_each()` from `library(dplyr)` to create a data frame of the averages of each variable for each activity and each subject (saved as 'summary').  
* Printed 'summary' to 'SamsungSummary.txt'.
```{r}
write.table(summary,file="SamsungSummary.txt", row.names=FALSE)
```
