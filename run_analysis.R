############################################################################################
## Getting and cleaning data
## Course Project
## This R script :
## 1/ Merges the training and the test sets to create one data set.
## 2/ Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3/ Uses descriptive activity names to name the activities in the data set
## 4/ Appropriately labels the data set with descriptive variable names. 
## 5/ From the data set in step 4, creates a second, independent tidy data set with the average 
##    of each variable for each activity and each subject.
##
## Author: O. Chuzel
## Date: 20 February 2015
############################################################################################

library("data.table")
library(dplyr)
tidy1sub1 <- function(fileUrlOrig, fileUrl)
{
cmde<-paste("cp ", fileUrlOrig, sep="")
cmde<-paste(cmde, " ", sep="")
cmde<-paste(cmde, fileUrl, sep="")
print(cmde)
system(cmde)
##sed
cmde<-paste("sed -i -e 's/  / /g' ", fileUrl, sep="")
print(cmde)
system(cmde)
}

ldf <- 
  function(fileUrl)
  {
    ds1<-read.table(fileUrl, sep=" ", header=FALSE) 
    ncol(ds1)
    colnames(ds1)[1]<-"DEL"
    drops<-c("DEL")
    ds1filt<-ds1[,!(names(ds1) %in% drops)]
  }

tidy1sub2<- function(fileTrainUrl, fileTestUrl)
{
ds1<-ldf(fileTrainUrl)
ds2<-ldf(fileTestUrl)
ds3<-rbind(ds1, ds2)
ds3
}

tidy2<- function(dirPath, ds3)
{
  fileFeaturesUrl<-paste(dirPath, "features.txt", sep="")
  dffeat2<-ldf(fileFeaturesUrl)
  hits<-grep("mean|std", dffeat2)
  df4<-ds3[, hits]
}

tidy3sub1<- function(dirPath)
{  
  trainLabelPath<-paste(dirPath, "/train/y_train.txt", sep="")
  testLabelPath<-paste(dirPath, "/test/y_test.txt", sep="")
  dstrain<-read.table(trainLabelPath, sep="\t", header=FALSE) 
  dstest<-read.table(testLabelPath, sep="\t", header=FALSE) 
  dsQ3<-rbind(dstrain, dstest)
}

tidy3sub2<- function(dirPath) #, dsQ2)
{  
  dsQ3sub1<-tidy3sub1(dirPath)
  activLabel="./UCI HAR Dataset/activity_labels.txt"
  activity_labels<-read.table(activLabel, sep=" ", header=FALSE) 
  dsQ3sub1$Activity <- activity_labels[match(dsQ3sub1$V1, activity_labels[,1]),2]
}

tidy3sub3<- function(dirPath, dsVariables)
{
  fileFeaturesUrl<-paste(dirPath, "/features.txt", sep="")
  dffeat3<-ldf(fileFeaturesUrl)
  hits<-grep("mean|std", dffeat3)
  ##change column name #n
  ind<-1
  for(i in hits)
  {
    head<-""
    head<-paste(head, dffeat3[i], sep="")
    #create correct variable name
    #no ()
    #no -
    head<-gsub("\\(\\)", "", head)
    head<-gsub("-", "", head)
    ##Some variables are incorrectly named
    head<-gsub("BodyBody", "Body", head)
    
    colnames(dsVariables)[ind] <- head
    ind<-ind + 1
  }
  dsVariables
}

tidy5<-function(dirPath)
{
  fileSubjectTest<-paste(dirPath, "test/subject_test.txt", sep="")
  fileSubjectTrain<-paste(dirPath, "train/subject_train.txt", sep="")
  
  dstrain<-read.table(fileSubjectTrain, sep="\t", header=FALSE) 
  dstest<-read.table(fileSubjectTest, sep="\t", header=FALSE) 
  dsQ4<-rbind(dstrain, dstest)
}

run_analysis<- function()
{  
  #File processing using sed on copied files
  fileTestUrlOrig="'./UCI HAR Dataset/test/X_test.txt'"
  fileTestUrl="'./UCI HAR Dataset/test/X_test-one.txt'"
  tidy1sub1(fileTestUrlOrig, fileTestUrl)
  
  fileTrainUrlOrig="'./UCI HAR Dataset/train/X_train.txt'"
  fileTrainUrl="'./UCI HAR Dataset/train/X_train-one.txt'"
  tidy1sub1(fileTrainUrlOrig, fileTrainUrl)
  
  fileTrainUrl="./UCI HAR Dataset/train/X_train-one.txt"
  fileTestUrl="./UCI HAR Dataset/test/X_test-one.txt"
  #Concatenation
  dsQ1<-tidy1sub2(fileTrainUrl, fileTestUrl)
  
  #Keep only columns having name containing mean or std
  #dsVariables has not yet the proper column names
  dirPath="./UCI HAR Dataset/"  
  dsVariables<-tidy2(dirPath, dsQ1)
  
  #List of subjects
  dsSubjects<-tidy5(dirPath)
  
  #List of activities
  dsActivities<-tidy3sub2(dirPath) 
  
  #List of renamed variables
  dsRenamedVar<-tidy3sub3(dirPath, dsVariables)
  
  #List of activities + subjects + variables
  dsSubVar<-cbind(dsSubjects, dsRenamedVar)
  dsQ5<-cbind(dsActivities, dsSubVar)
  colnames(dsQ5)[1] <- 'Activity'
  colnames(dsQ5)[2] <- 'Subject'
  
  #we want activity, subject, variables
  dsQ5 %>% group_by(Activity, Subject) %>% summarise_each(funs(mean))
  write.table(dsQ5, "tidy_dataset", row.name=FALSE)
  dsQ5
  }

#dsQ<-run_analysis()
#library(dplyr)
#dsQ %>% group_by(Activity, Subject) %>% summarise_each(funs(mean))
#write.table(dsQ, "tidy_dataset", row.name=FALSE)

#library(knitr)
#knit("run_analysis.Rmd")
