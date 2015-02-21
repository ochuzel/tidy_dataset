
---
title: "README"
author: "O. Chuzel"
date: "Friday, February 20, 2015"
output: html_document
---

#Introduction
This assignment consists in assembling various files. This assemblage requires to keep the row order
 of each file in order to keep parts of the tuples synchronized.
It will then not be possible to use functions which change the ordering of tuples ; setKey, for example, rearranges the set which makes then impossible to use later.
The approach chosen is to start the concatenation with data of train files followed by
 those of test files. Each concatenation carried out will have to fulfill this convention.

#Functional description
##ldf

* Load a table from a file,
* Remove the first empty column.

```r
ldf <- 
  function(fileUrl)
  {
    ds1<-read.table(fileUrl, sep=" ", header=FALSE) 
    ncol(ds1)
    colnames(ds1)[1]<-"DEL"
    drops<-c("DEL")
    ds1filt<-ds1[,!(names(ds1) %in% drops)]
  }
```

##tidy1sub1

* Works on a Copy of the file to be processed,
* Changes the formatting of this copy.
NB: The unix sed command has been used to substitue double spaces with single space.

```r
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
```

##tidy1sub2

* Calls ldf,
* Concatenates the 2 df by appending rows of a train df and a test df, in that order.

```r
tidy1sub2<- function(fileTrainUrl, fileTestUrl)
{
ds1<-ldf(fileTrainUrl)
ds2<-ldf(fileTestUrl)
ds3<-rbind(ds1, ds2)
ds3
}
```

##tidy2

* Builds a df containing only columns whose variables names contain either mean of std sub string.

```r
tidy2<- function(dirPath, ds3)
{
  fileFeaturesUrl<-paste(dirPath, "features.txt", sep="")
  dffeat2<-ldf(fileFeaturesUrl)
  hits<-grep("mean|std", dffeat2)
  df4<-ds3[, hits]
}
```

##tidy3sub1

* Create a df containing the union of validation subjects and test subjects.

```r
tidy3sub1<- function(dirPath)
{  
  trainLabelPath<-paste(dirPath, "/train/y_train.txt", sep="")
  testLabelPath<-paste(dirPath, "/test/y_test.txt", sep="")
  dstrain<-read.table(trainLabelPath, sep="\t", header=FALSE) 
  dstest<-read.table(testLabelPath, sep="\t", header=FALSE) 
  dsQ3<-rbind(dstrain, dstest)
}
```

#tidy3sub2

* Calls tidy3sub1,
* Creates a df with a new column made of the activity names derived from a column of activity labels.
use of:

```r
tidy3sub2<- function(dirPath) #, dsQ2)
{  
  dsQ3sub1<-tidy3sub1(dirPath)
  activLabel="./UCI HAR Dataset/activity_labels.txt"
  activity_labels<-read.table(activLabel, sep=" ", header=FALSE) 
  dsQ3sub1$Activity <- activity_labels[match(dsQ3sub1$V1, activity_labels[,1]),2]
}
```
  
##tidy3sub3

* Retrieves the variables names,
* Retains only those having mean or std in their names,
* Normalizes the variables by removing any forbidden char such as -, ( and ),
* Fixed the issue of incorrect variables naming in the original dataset (BodyBody)

```r
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
```

##tidy5

* aggreagates the traing subjects and test subjects in a single column.

```r
tidy5<-function(dirPath)
{
  fileSubjectTest<-paste(dirPath, "test/subject_test.txt", sep="")
  fileSubjectTrain<-paste(dirPath, "train/subject_train.txt", sep="")
  
  dstrain<-read.table(fileSubjectTrain, sep="\t", header=FALSE) 
  dstest<-read.table(fileSubjectTest, sep="\t", header=FALSE) 
  dsQ4<-rbind(dstrain, dstest)
}
```
##run_analysis

* This main function generates the tidy dataset.


```r
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
```

##Constraints
This function, uses sed and cp, runs only on a Unix or on Windows fitted which cygwin.
