#############################################################################################################################
# Project : Getting and Cleaning Data Course Project
# Program by: Walter Kung
# Last Modified: 2014-07-13
# Descriptions:
#    The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to 
#    prepare tidy data that can be used for later analysis. Program should perform the following tasks:
#    1. Merges the training and the test sets to create one data set.
#    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#    3. Uses descriptive activity names to name the activities in the data set
#    4. Appropriately labels the data set with descriptive variable names. 
#    5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# Requirements:
#   The content of https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip is unzipped 
#   in .\\getdata_projectfiles_UCI HAR Dataset\\ directory
# Outputs:
#   The average of each variable for each activity and each subject will be stored as
#   .\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\activity_data.csv
#############################################################################################################################


#############################################################################################################################
# Initialization
library(data.table)
datadir <- '.\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\'
#############################################################################################################################


#############################################################################################################################
#### Background information Loading

##### Read in Activity labels
fileIn <- paste (datadir, 'activity_labels.txt', sep = '')
activityLbl <- read.table (file=fileIn, sep = " ")
colnames(activityLbl) <- c('activity_key', 'activity_lbl')
activityLbl$activity_lbl <- unlist(lapply(activityLbl$activity_lbl , as.character))

##### Read in feature labels
fileIn <- paste (datadir, 'features.txt', sep = '')
features <- read.table (file=fileIn, sep = " ")
colnames(features) <- c('features_key', 'features_lbl')
features$features_lbl <- unlist(lapply(features$features_lbl , as.character))

##### Select the relevant features (mean and std)
DT_features<-data.table(features)
DT_features$features_key <- paste("V",DT_features$features_key,sep='')
features_selected <- DT_features[DT_features[,grepl("-mean\\(",features_lbl)]|DT_features[,grepl("-std\\(",features_lbl)]]
features_selected$features_key
#############################################################################################################################


#############################################################################################################################
#### Read in training set
##### Read in the training set data
fileIn <- paste (datadir, 'train\\X_train.txt', sep = '')
dfTrain <- read.fwf(
  file=fileIn,
  widths=c(17,rep(16,560)))

##### Read in the training activity label
fileIn <- paste (datadir, 'train\\y_train.txt', sep = '')
y_train <- read.table (file=fileIn, sep = " ")
colnames(y_train) <- c('activity_key')

##### Read in the test subject label
fileIn <- paste (datadir, 'train\\subject_train.txt', sep = '')
subject_train <- read.table (file=fileIn, sep = " ")
colnames(subject_train) <- c('subject_key')

##### Combine information
dfTrain <- cbind(dfTrain, y_train)
dfTrain <- cbind(dfTrain, subject_train)

##### Extracts only the measurements on the mean and standard deviation for each measurement. 
dfTrain<-dfTrain[c('subject_key', 'activity_key', features_selected$features_key)]

#### Read in testing set
##### Read in the testing set data
fileIn <- paste (datadir, 'test\\X_test.txt', sep = '')
dftest <- read.fwf(
  file=fileIn,
  widths=c(17,rep(16,560)))

##### Read in the testing activity label
fileIn <- paste (datadir, 'test\\y_test.txt', sep = '')
y_test <- read.table (file=fileIn, sep = " ")
colnames(y_test) <- c('activity_key')

##### Read in the test subject label
fileIn <- paste (datadir, 'test\\subject_test.txt', sep = '')
subject_test <- read.table (file=fileIn, sep = " ")
colnames(subject_test) <- c('subject_key')

##### Combine information
dftest <- cbind(dftest, y_test)
dftest <- cbind(dftest, subject_test)

##### Extracts only the measurements on the mean and standard deviation for each measurement. 
dftest<-dftest[c('subject_key', 'activity_key', features_selected$features_key)]

#############################################################################################################################


#############################################################################################################################
# Merges the training and the test sets to create one data set.
DT<-data.table(rbind(dfTrain, dftest))
dfTrain<-NULL
dftest<-NULL

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
newDS<-DT[, j=list
    (        
          mean(V1,na.rm=T),
          mean(V2,na.rm=T),
          mean(V3,na.rm=T),
          mean(V4,na.rm=T),
          mean(V5,na.rm=T),
          mean(V6,na.rm=T),
          mean(V41,na.rm=T),
          mean(V42,na.rm=T),
          mean(V43,na.rm=T),          
          mean(V44,na.rm=T),
          mean(V45,na.rm=T),
          mean(V46,na.rm=T),
          mean(V81,na.rm=T),
          mean(V82,na.rm=T),
          mean(V83,na.rm=T),
          mean(V84,na.rm=T),
          mean(V85,na.rm=T),
          mean(V86,na.rm=T),
          mean(V121,na.rm=T),
          mean(V122,na.rm=T),
          mean(V123,na.rm=T),
          mean(V124,na.rm=T),
          mean(V125,na.rm=T),
          mean(V126,na.rm=T),
          mean(V161,na.rm=T),
          mean(V162,na.rm=T),
          mean(V163,na.rm=T),
          mean(V164,na.rm=T),
          mean(V165,na.rm=T),
          mean(V166,na.rm=T),
          mean(V201,na.rm=T),
          mean(V202,na.rm=T),
          mean(V214,na.rm=T),
          mean(V215,na.rm=T),
          mean(V227,na.rm=T),
          mean(V228,na.rm=T),
          mean(V240,na.rm=T),
          mean(V241,na.rm=T),
          mean(V253,na.rm=T),
          mean(V254,na.rm=T),
          mean(V266,na.rm=T),
          mean(V267,na.rm=T),
          mean(V268,na.rm=T),
          mean(V269,na.rm=T),
          mean(V270,na.rm=T),
          mean(V271,na.rm=T),
          mean(V345,na.rm=T),
          mean(V346,na.rm=T),
          mean(V347,na.rm=T),
          mean(V348,na.rm=T),
          mean(V349,na.rm=T),
          mean(V350,na.rm=T),
          mean(V424,na.rm=T),
          mean(V425,na.rm=T),
          mean(V426,na.rm=T),
          mean(V427,na.rm=T),
          mean(V428,na.rm=T),
          mean(V429,na.rm=T),
          mean(V503,na.rm=T),
          mean(V504,na.rm=T),
          mean(V516,na.rm=T),
          mean(V517,na.rm=T),
          mean(V529,na.rm=T),
          mean(V530,na.rm=T),
          mean(V542,na.rm=T),
          mean(V543,na.rm=T)
    ), by=list(activity_key, subject_key)]

### Appropriately labels the data set with descriptive variable names. 
features_lbl<-features_selected$features_lbl
features_lbl<-gsub("-", "_", gsub("\\)", "", gsub("\\(","",features_lbl)))
setnames(newDS, old=names(newDS),c(names(newDS)[c(1,2)], features_lbl))

# Uses descriptive activity names to name the activities in the data set

#keys
setkey(newDS,activity_key)
DT_activityLbl <- data.table(activityLbl)
setkey(DT_activityLbl,activity_key)

#joins
newDS<-merge(newDS, DT_activityLbl)
newDS[,activity_key:=NULL]
#############################################################################################################################


#############################################################################################################################
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
fileOut <- paste (datadir, 'activity_data.csv', sep = '')
write.table(newDS, file=fileOut,row.names = F, sep=",")
#############################################################################################################################


