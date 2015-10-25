#Assignment Managing Data

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 
 
#You will be required to submit: 
#1) a tidy data set as described below, 
2) a link to a Github repository with your script for performing the analysis, and
3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
#You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
# A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


# R script called run_analysis.R does the following. 

#   Loading the data sets into R.
#********************************************************************
# 'train/X_train.txt': Training set.
# 'test/X_test.txt': Test set.

setwd("C:/Users/Tony Philip/Desktop/Working/Learn/GettCleanDataCourse/UCI HAR Dataset")
getwd()

xtestset <- read.table("./test/X_test.txt")
ytestset <- read.table("./test/y_test.txt")
subtestset <- read.table("./test/subject_test.txt")

xtrainset <- read.table("./train/X_train.txt")
ytrainset <- read.table("./train/y_train.txt")
subtrainset <- read.table("./train/subject_train.txt")

features <- read.table("./features.txt")
activities <- read.table("./activity_labels.txt")


# 1.   Merges the training and the test sets to create one data set.
#********************************************************************

head(xtrainset, n=1)
head(xtestset, n=1)

Combxdata <- rbind(xtrainset, xtestset)
Combydata <- rbind(ytrainset, ytestset)
Combsubdata <- rbind(subtrainset, subtestset)
Totaldata <- cbind(Combxdata,Combydata,Combsubdata )
remove(Combxdata, Combydata, Combsubdata)

##################
# 2.   Uses descriptive activity names to name the activities in the data set
#********************************************************************************************

FeatureOptions <- as.character(features[,2])
newCols <- c("subject", "activity", FeatureOptions)
colnames(TotalDataNew) <- newCols




#  3.  Extract only the measurements on the mean and standard deviation for each measurement.
#********************************************************************************************

DataMeans <- grep("mean()", colnames(Totaldata))
DataSDs <- grep("std()", colnames(Totaldata))

ColumnsRev <- c(DataMeans, DataSDs)
ColumnsNew <- sort(ColumnsRev)

TotalDataRev <- Totaldata[, c(1,2,ColumnsNew)]
TotalDataNew <- TotalDataRev[, !grepl("Freq", colnames(TotalDataRev))] 

remove(TotalDataRev, Totaldata) 
 

#  4.  Appropriately labels the data set with descriptive variable names.
#******************************************************************************************** 
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
 

# Relabel the columns 

levels(TotalDataNew[,2])<-c('walk','walk_upstairs','walk_downstairs', 'sit','stand', 'lay')

###########

# 5.  From the data set in step 4, creates a second, independent tidy data set 
#  with the average of each variable for each activity and each subject.
#********************************************************************************************************************************************

tidyds <- data.frame()
for (i in 1:30) {
        subjt<- subset(TotalDataNew,subject==i)
        for (j in 1:6){
                acty<- subset(subjt, activity==j)
                subactpair <-as.vector(apply(acty,2,mean))
                tidyds<-rbind(tidyds, subactpair) 
        }
        
}
colnames(tidyds)<-colnames(TotalDataNew) 

# output the data to "TidyData.txt"
write.table(tidyds, "TidyData.txt", sep = "",row.name=FALSE)

############

