---
title: "CodeBook"
author: "O. Chuzel"
date: "Friday, February 20, 2015"
output: html_document
---

The variables described in this document come from the selection of variables in the features.txt file.
Variables names were modified in order to be compliant with the r naming of variables and some wrong naming were corrected. This document presents the implementation of the selection criterium, the process of normalizing variable names and a short description of the variables.

#Selection criterium
Variables selected reflect either mean or standard deviation.
They have been selected by using a grep on the column name of the data set.

#Normalizing process
Raw names are not compliant with the r naming convention. That is why incorrect characters such has '(', ')' and '-' were stripped out. The code below retrieves the raw column names and converts them in standard ones. Moreover the code fixes a naming error where some variables name duplicated the word Body.


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
#List of selected variables

Mean and standard deviation variables are derived from the following variables:  
'XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

* tBodyAccXYZ
* tGravityAccXYZ
* tBodyAccJerkXYZ
* tBodyGyroXYZ
* tBodyGyroJerkXYZ
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag

* fBodyAccXYZ
* fBodyAccJerkXYZ
* fBodyGyroXYZ
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

Additional selected variables are:

* gravityMean
* tBodyAccMean
* tBodyAccJerkMean
* tBodyGyroMean
* tBodyGyroJerkMean

Consequently, mean variables are:

* tBodyAccmeanX
* tBodyAccmeanY
* tBodyAccmeanZ
* tGravityAccmeanX
* tGravityAccmeanY
* tGravityAccmeanZ
* tBodyAccJerkmeanX
* tBodyAccJerkmeanY
* tBodyAccJerkmeanZ
* tBodyGyromeanX
* tBodyGyromeanY
* tBodyGyromeanZ
* tBodyGyroJerkmeanX
* tBodyGyroJerkmeanY
* tBodyGyroJerkmeanZ
* tBodyAccMagmean
* tGravityAccMagmean
* tBodyAccJerkMagmean
* tBodyGyroMagmean
* tBodyGyroJerkMagmean
* fBodyAccmeanX
* fBodyAccmeanY
* fBodyAccmeanZ
* fBodyAccmeanFreqX
* fBodyAccmeanFreqY
* fBodyAccmeanFreqZ
* fBodyAccJerkmeanX
* fBodyAccJerkmeanY
* fBodyAccJerkmeanZ
* fBodyAccJerkmeanFreqX
* fBodyAccJerkmeanFreqY
* fBodyAccJerkmeanFreqZ
* fBodyGyromeanX
* fBodyGyromeanY
* fBodyGyromeanZ
* fBodyGyromeanFreqX
* fBodyGyromeanFreqY
* fBodyGyromeanFreqZ
* fBodyAccMagmean
* fBodyAccMagmeanFreq
* fBodyBodyAccJerkMagmean
* fBodyBodyAccJerkMagmeanFreq
* fBodyBodyGyroMagmean
* fBodyBodyGyroMagmeanFreq
* fBodyBodyGyroJerkMagmean
* fBodyBodyGyroJerkMagmeanFreq

Wherea standard deviation variables are:

* tBodyAccstdX
* tBodyAccstdY
* tBodyAccstdZ
* tGravityAccstdX
* tGravityAccstdY
* tGravityAccstdZ
* tBodyAccJerkstdX
* tBodyAccJerkstdY
* tBodyAccJerkstdZ
* tBodyGyrostdX
* tBodyGyrostdY
* tBodyGyrostdZ
* tBodyGyroJerkstdX
* tBodyGyroJerkstdY
* tBodyGyroJerkstdZ
* tBodyAccMagstd
* tGravityAccMagstd
* tBodyAccJerkMagstd
* tBodyGyroMagstd
* tBodyGyroJerkMagstd
* fBodyAccstdX
* fBodyAccstdY
* fBodyAccstdZ
* fBodyAccJerkstdX
* fBodyAccJerkstdY
* fBodyAccJerkstdZ
* fBodyGyrostdX
* fBodyGyrostdY
* fBodyGyrostdZ
* fBodyAccMagstd
* fBodyBodyAccJerkMagstd
* fBodyBodyGyroMagstd
* fBodyBodyGyroJerkMagstd

Correction
Double occurrences of the term 'Body' appear in few folowing variable names:

* fBodyBodyAccJerkMagmean
* fBodyBodyAccJerkMagmeanFreq
* fBodyBodyGyroMagmean
* fBodyBodyGyroMagmeanFreq
* fBodyBodyGyroJerkMagmean
* fBodyBodyGyroJerkMagmeanFreq

* fBodyBodyAccJerkMagstd
* fBodyBodyGyroMagstd
* fBodyBodyGyroJerkMagstd

This was fixed by the tidy code.

#Description of the variables

Units:
tAcc-XYZ and tGyroXYZ measures come from the accelerometer and gyroscope 3-axial raw signals. 

Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAccXYZ)

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerkXYZ and tBodyGyroJerkXYZ). 
Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm 
(tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, 
fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

Name             Unit      
---------------  ------------
tBodyAccXYZ      m/s2
tGravityAccXYZ   m/s2
tBodyAccJerkXYZ  m/s3
tBodyGyroXYZ     rd/s
tBodyGyroJerkXYZ rd/s2
tBodyAccMag      m/s2
tGravityAccMag   m/s2
tBodyAccJerkMag  m/s3
tBodyGyroMag     rd/s
tBodyGyroJerkMag rd/s2

Name             Unit      
---------------  ------------
fBodyAccXYZ      Hz
fBodyAccJerkXYZ  Hz
fBodyGyroXYZ     Hz
fBodyAccMag      Hz
fBodyAccJerkMag  Hz
fBodyGyroMag     Hz
fBodyGyroJerkMag Hz

Additional selected variables are:

Name              Unit      
----------------  ------------
gravityMean       m/s2
tBodyAccMean      m/s2
tBodyAccJerkMean  m/s3
tBodyGyroMean     rd/s
tBodyGyroJerkMean rd/s2


