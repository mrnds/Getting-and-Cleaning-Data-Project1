#-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-#
#GETTING AND CLEANING DATA COURSE PROJECT
#-##-##-##-##-##-##-##-##-##-##-##-##-##-##-##-#


#The purpose of this project is to demonstrate your ability to collect, 
#work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
#You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
########################################################################################################
#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



#Step-0. Set WD
setwd("D:/Coursera_Data_Science/Project_3/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset/")

# Step-1. Merges the training and the test sets to create one data set.

#1.1 #read data (train)
features          <- read.table("./features.txt",header=FALSE)
activity_labels   <- read.table("./activity_labels.txt",header=FALSE)
subject_train     <- read.table("./train/subject_train.txt", header=FALSE)
x_train           <- read.table("./train/X_train.txt", header=FALSE)
y_train           <- read.table("./train/y_train.txt", header=FALSE)

colnames(activity_labels)<-c("activityId","activityType")
colnames(subject_train) <- "subId"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityId"
# 1.2 Merge data (train)
data_train <- cbind(y_train,subject_train,x_train)

#1.3 #read data (test)

subject_test    <-read.table("./test/subject_test.txt", header=FALSE)
x_test          <- read.table("./test/X_test.txt", header=FALSE)
y_test          <- read.table("./test/y_test.txt", header=FALSE)

colnames(subject_test) <- "subId"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityId"

# 1.4 Merge data (test)
data_test <- cbind(y_test,subject_test,x_test)


# 1.5 merge train+test
data_all <- rbind(data_train,data_test)


#Step-2. Extracts only the measurements on the mean and standard deviation for each measurement.

#col_names <- colnames(data_all);
data_mean_std <-data_all[,grepl("mean|std|subject|activityId",colnames(data_all))]


#Step-3.Uses descriptive activity names to name the activities in the data set
library(plyr)
data_mean_std <- join(data_mean_std, activity_labels, by = "activityId", match = "first")
data_mean_std <-data_mean_std[,-1]


#step-4. Apropriately labels the data set with descriptive variable names.
names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)
names(data_mean_std) <- make.names(names(data_mean_std))
names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#Step-5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
data_tidy<- (data_mean_std %>%  select(-(activityType)) %>%  group_by(activityType) %>% summarise_each(funs( mean)))
write.table(data_tidy,file="data_tidy.txt")