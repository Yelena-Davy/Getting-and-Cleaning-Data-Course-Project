##  Developed by Yelena Davydova
##       as part of Clearing Data Course project

##  This script 
##
##	Merges the training and the test sets to create one data set.
##	Extracts only the measurements on the mean and standard deviation for each measurement. 
##	Uses descriptive activity names to name the activities in the data set
##	Appropriately labels the data set with descriptive variable names. 
##	From above data set  creates a second, independent tidy data set 
##  with the average of each variable for each activity and each subject.

#################################################################################################

## Read sets,subjects,labels
  setwd("~/DataScience/ClearingData/Assignment/UCI HAR Dataset/UCI HAR Dataset")
  y_data<-read.table("./test/y_test.txt")
  s_data<-read.table("./test/subject_test.txt")
  x_data<-read.table("./test/x_test.txt")
  y1_data<-read.table("./train/y_train.txt")
  s1_data<-read.table("./train/subject_train.txt")
  x1_data<-read.table("./train/x_train.txt")

  label<-read.table("activity_labels.txt",stringsAsFactors = F)
  
## read features

  features_data<-read.table("features.txt",stringsAsFactors = F)

## rename columns in s_data and y_data
  names(s_data)[1]<-"sid"
  names(y_data)[1]<-"id"
  names(s1_data)[1]<-"sid"
  names(y1_data)[1]<-"id"
  
## 	Uses descriptive activity names to name the activities in the data set
  ## rename columns in x,x1_data
  
  names(x_data)<-features_data[[2]]
  names(x1_data)<-features_data[[2]]
  
## add columns s(1)_data,y(1)_data,x(1)_data
  
  z_data<-cbind(s_data,y_data,x_data)
  z1_data<-cbind(s1_data,y1_data,x1_data)
  #################################################################################################
  
## 1.	Merges the training and the test sets to create one data set.

  w_data<-rbind(z_data,z1_data)
  
  ## rename labels column names
  
  names(label)<-c("id","activity")
  
  ## merge w_data with label
  
  m_data<-merge(label,w_data,by.x="id",by.y="id",all=F)
  
  
## 2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  tidy_data<-m_data[,grep("activity|sid|mean\\(|std\\(",names(m_data)) ]




## 4.	Appropriately labels the data set with descriptive variable names
##  - change variable names to lowercase
##  - removes '-','()'
  library("stringr")
  tidy_names<-names(tidy_data) %>% tolower()
  tidy_names<-gsub("-","",tidy_names)
  tidy_names<-gsub(")","",tidy_names)
  tidy_names<-gsub("\\(","",tidy_names)
  names(tidy_data)<-tidy_names
## 5.	From the data set in step 4, creates a second, independent tidy data set 
##  with the average of each variable for each activity and each subject.

  average_data<-aggregate(tidy_data[,3:68], list(tidy_data$activity,tidy_data$sid), mean)
  names(average_data)[1]<-"activity"
  names(average_data)[2]<-"subjectId"
  # Write tidy data to CSV MyData.csv
  write.table(average_data, file = "MyData.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
  










