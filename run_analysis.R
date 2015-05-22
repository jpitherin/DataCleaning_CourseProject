### Reading in testing and training files
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")


### 2) extracting only the measurements on the mean & standard deviation
#       for each measurement
### Trimming X_test and X_train to just mean and sd
mean_sd <- c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,
             253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)
X_test <- X_test[,mean_sd]
X_train <- X_train[,mean_sd]
rm(mean_sd)


### 4) appropriately labelling the data set with descriptive variable names
### Adding names to X_test, X_train, Y_test, Y_train, subject_test, subject_train
MSnames <- read.table("./UCI HAR Dataset/features_mean_sd.txt")
names(X_test) <- MSnames[,2]
names(X_train) <- MSnames[,2]
names(Y_test) <- "activity"
names(Y_train) <- "activity"
names(subject_test) <- "subject"
names(subject_train) <- "subject"
rm(MSnames)


### 3) using descriptive activity names to name activities in the data set
### matching the numbers with activity names in Y_test and Y_train
act_labels <- function(data,labels){
    for (i in seq_along(data[,1])){
        if(data[i,1]==1) {data[i,1] <- as.character(labels[1,2])}
        if(data[i,1]==2) {data[i,1] <- as.character(labels[2,2])}
        if(data[i,1]==3) {data[i,1] <- as.character(labels[3,2])}
        if(data[i,1]==4) {data[i,1] <- as.character(labels[4,2])}
        if(data[i,1]==5) {data[i,1] <- as.character(labels[5,2])}
        if(data[i,1]==6) {data[i,1] <- as.character(labels[6,2])}
    }
    data
}

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
Y_test <- act_labels(Y_test, activity_labels)
Y_train <- act_labels(Y_train, activity_labels)
rm(activity_labels,act_labels)


### 1) Merging training and test sets to create one data set (named SamsungData)
test <- cbind(group = rep_len("test",length(Y_test[,1])),subject_test,Y_test,X_test)
train <- cbind(group = rep_len("train",length(Y_train[,1])),subject_train,Y_train, X_train)
# clean up environment (to save space)
rm(subject_test,Y_test,X_test)
rm(subject_train,Y_train, X_train)

SamsungData <- rbind(test,train)
rm(test,train)


### 5) create another data set with the 
### average of each variable for each activity and each subject

library(dplyr)
summary <- group_by(SamsungData, subject, activity) %>% summarise_each(funs(mean))


### Printing SamsungData to a text file
write.table(summary,file="SamsungSummary.txt", row.names=FALSE)
