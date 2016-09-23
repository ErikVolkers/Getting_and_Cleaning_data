################################################################################
###  Getting and Cleaning Data Course Project                               ####
################################################################################


################################################################################
### This script takes data collected from the accelerometers                ####
### from the Samsung Galaxy S smartphone.                                   ####
### the data are downloaded from:                                           ####
### http://archive.ics.uci.edu/ml/datasets/                                 ####
###     Human+Activity+Recognition+Using+Smartphones                        ####
### The script takes the test and train data and combines it into one file. ####
### It extracts only the measurements on the                                ####
### mean and standard deviation for each measurement.                       ####
### It then adds descriptive activity names to name the activities          ####
### and labels the data set with descriptive variable names.                ####
### From this dataset, it creates a second,                                 ####
### independent tidy data set with the average of each variable             ####
### for each activity and each subject.                                     ####
### The tidy dataset is written in csv format                               ####
################################################################################


## start with a clean environment
rm(list=ls())

# open libraries
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

## read all datasets
################################################################################

# read test data
test_set.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/test/X_test.txt")
test_labels.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/test/y_test.txt")
subject_test.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/test/subject_test.txt")

# read train data
train_set.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/train/X_train.txt")
train_labels.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/train/y_train.txt")
subject_train.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/train/subject_train.txt")

# read label names
features.R <- read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/features.txt")
activity_labels.R <-  read.table("c:/Users/ev00086/Downloads/UCI HAR Dataset/activity_labels.txt")


## create meaningful names
# set activity code to "activity" and activity labels to "activity_name"
colnames(activity_labels.R)[1] <- "activity"
colnames(activity_labels.R)[2] <- "activity.name"

# set subject names to "subject"
colnames(subject_test.R)[1] <- "subject"
colnames(subject_train.R)[1] <- "subject"

# set labels to "activity"
colnames(test_labels.R)[1] <- "activity"
colnames(train_labels.R)[1] <- "activity"

# set v2 in features to "variable_names"
colnames(features.R)[2] <- "variable.names"


## Part 1: merge the training and the test sets to create one data set.
################################################################################

## complete the test set

# add subject labels and activity to test set
test_set2.R <- cbind(subject_test.R, test_labels.R, test_set.R)

# label test set as "test"
test_set3.R <- cbind("test", test_set2.R)
# set name of first column to "set"
colnames(test_set3.R)[1] <- "set"


## complete the train set

# add subject labels and activity to train set
train_set2.R <- cbind(subject_train.R, train_labels.R, train_set.R)

# label train set as "train"
train_set3.R <- cbind("train", train_set2.R)
# set name of first column to "set"
colnames(train_set3.R)[1] <- "set"


## merge into one data set
mydata.R <- rbind(test_set3.R, train_set3.R)



## Part 2: Extract only the measurements on the
## mean and standard deviation for each measurement.
################################################################################

# which features must be kept: mean and std. NOT meanFreq
features_mean.R <- features.R[grep("mean", features.R$variable.names), ]
features_mean2.R <- features_mean.R[-grep("Freq", features_mean.R$variable.names), ]
features_std.R <- features.R[grep("std", features.R$variable.names), ]
features_to_keep.R <- rbind(features_mean2.R, features_std.R)%>%
    mutate(colnumber.to.keep = V1 + 3)%>% # because three columns have been added
    arrange(V1)

# keep the desired columns
my_clean_data.R <- mydata.R[c(1:3, features_to_keep.R$colnumber.to.keep)]


## Part 3: use descriptive activity names to name the activities in the data set
################################################################################

# join activity labels to my_clean_data
my_clean_data.R <- my_clean_data.R %>%
    left_join(., activity_labels.R, by = "activity")

# reorder for better understanding
my_clean_data.R <- my_clean_data.R[c(1,2,3, 70, 4:69)]


## Part 4: appropriately label the data set with descriptive variable names.
################################################################################

# change colnames to variable names.
# first four columns have names already
colnames(my_clean_data.R)[5:70] <- as.character(features_to_keep.R$variable.names)

# now we don't need column "activity" anymore
my_clean_data.R <- my_clean_data.R%>%
    select(-activity)

## just to clean up the environment, remove all old datafiles
rm(list=setdiff(ls(), "my_clean_data.R"))


## Part 5: from the data set in step 4, create a second, 
## independent tidy data set with the average of each variable 
## for each activity and each subject.
################################################################################

## reshape the data
tidy1.R <- gather(my_clean_data.R, measures, normalised.value, -c(set, subject, activity.name))


## split the variables. Each measure consists of seven different variables:
# 1. type of data (raw or Fast Fourier Transformed)
# 2. signal derived as "Jerk signal" or underived
# 3. measured by accelerometer or by gyroscope (device)
# 4. acceleration signal is divided in gravity, body motion and bodybody motion
# 5. direction is measured as X-, Y-, Z-  or no specific direction
# 6. quantity is either magnitude or direction
# 7. measure is either mean or std 
tidy2.R <- tidy1.R%>%
    mutate(type.of.data = ifelse(test = substr(measures, 1, 1)=="t",
                                 yes= "raw",
                                 no = "FFT"))%>%
    mutate(type.of.signal = ifelse(test = grepl("Jerk", measures),
                                   yes = "jerk",
                                   no = "underived"))%>%
    mutate(device = ifelse(test = grepl("Acc", measures),
                           yes = "accelerometer",
                           no = "gyroscope"))%>%
    mutate(component = ifelse(test = grepl("BodyBody", measures),
                              yes = "BodyBody_motion",
                              no = ifelse(test = grepl("Body", measures),
                                          yes = "Body_motion",
                                          no = "gravitation")))%>%
    mutate(direction = ifelse(test = grepl("X", measures)
                                    | grepl("Y", measures)
                                    | grepl("Z", measures),
                              yes = substr(measures, nchar(measures), nchar(measures)),
                              no = "no direction"))%>%
    mutate(quantity = ifelse(test = direction == "no direction",
                             yes = "magnitude",
                             no = "direction"))%>%
    mutate(measure = ifelse(test = grepl("mean", measures),
                          yes = "mean",
                          no = "std"))

## only the means are required, remove all std's
tidy2.R <- tidy2.R%>%
    filter(measure == "mean")

## this means all values are now means and the column should be named as such
setnames(tidy2.R, "normalised.value", "mean.normalised.value")

## re-order the columns and drop columns "measures" and "measure".
tidydata.R <- tidy2.R[c(1:3, 6:11, 5)]

 
## Part 6: create the tidydata file
################################################################################

## write the file
write.csv2(tidydata.R, file = "C:\\Users\\ev00086\\Documents\\tidydata.csv", 
           row.names = FALSE)


################################################################################
### End of script                                                           ####
################################################################################

