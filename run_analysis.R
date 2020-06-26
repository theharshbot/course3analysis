library(dplyr)
library(data.table)

#create a directory for the data and download the file from given url and unzipping it
if(!file.exists("data")){dir.create("data")}
fileurl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile = "./data/rundata.zip",method = "curl")
unzip("./data/rundata.zip")

#reading all the variables names in features.txt
col_names <- read.table("./UCI HAR Dataset/features.txt")

#reading the X_train.txt which is values for activity,y_train which is activity,subject_train for subject
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",col.names = col_names[,2])
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt",col.names = as.factor(c("activity")))
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",col.names = as.factor(c("subject")))

#reading the X_test for values of test,y_test for the activity and subject_test for subject
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",col.names = col_names[,2])
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt",col.names = as.factor(c("activity")))
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",col.names = as.factor(c("subject")))

#merging the datasets of test and train into one dataset named all
test<- cbind(X_test,y_test,subject_test)
train <- cbind(X_train,y_train,subject_train)
all <- rbind(test,train)


#Extracts only the measurements on the mean and standard deviation for each measurement
mean_std <- select(all,contains("mean"),contains("std"),activity,subject)

#using the activity_labels.txt for naming the activities in the dataset
act <- function(x){
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
  activities[x,2]
}

mean_std <- mutate(mean_std,activity=act(mean_std$activity))

#appropriate labels to the dataset
names(mean_std) <- gsub("\\.","",tolower(names(mean_std))) 

#creating a another dataset from the above which calculates mean of all the variables and naming the dataset as tidy
mean_std_grouped <- group_by(mean_std,activity,subject)
tidy <- summarise_all(mean_std_grouped,mean)
write.table(tidy,"./tidy.txt",quote=FALSE,row.names = FALSE)


#to see the tidy.txt use --- read.table("./tidy.txt",header=TRUE)