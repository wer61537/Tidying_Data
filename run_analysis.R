
#Remove all previously built objects
rm(list=ls()) 


#====================================#
# Load the Files
# Features (column headings) are in “features.txt”
# Activity labels are in “activity_labels.txt”
# Activity is split into “Y_train.txt” and “Y_test.txt”
# Subject is split into “subject_train.txt” and subject_test.txt"
#====================================#:

# Download, extract dataset as need
if(!file.exists("UCI HAR Dataset")) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url,destfile = "Dataset.zip")
  unzip("Dataset.zip")
}

#test data
#extract each and then combine into 1 test dataframe
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
test <- cbind(subject_test, y_test, X_test)

#train data
#extract each and then combine into 1 train dataframe
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
train <- cbind(subject_train, y_train, X_train)

#Combine (merge) the train and test dataframes
merged <- rbind(test,train)
#names(merged) 

#get the names of the features
features <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
names(merged) 


#Rename the columns as subject, activity and features
names(merged) <- c("subject","activity", features$V2)

#get the labels for the activities
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)

#apply activity labels
merged$activity <- factor(merged$activity,levels = activity_labels$V1,labels = activity_labels$V2)

#inspect the data
str(merged)

#Pull out the mean and stnd dev
sub.merged <- merged[,grep("mean\\(\\)|std\\(\\)|subject|activity", names(merged))]

#Tidy names using gsub
#http://www.endmemo.com/program/R/gsub.php
#replace prepended t with Time_, f with Freq_, BodyBody with Body
names(sub.merged) <- gsub("^t", "Time_", names(sub.merged))
names(sub.merged) <- gsub("^f", "Freq_", names(sub.merged))
names(sub.merged) <- gsub("BodyBody", "Body", names(sub.merged))
#clean up mean in labels
names(sub.merged) <- gsub("-mean\\(\\)-", "_Mean_", names(sub.merged))
names(sub.merged) <- gsub("-mean\\(\\)", "_Mean", names(sub.merged))
#clean up std dev in labels
names(sub.merged) <- gsub("-std\\(\\)-", "_Std_", names(sub.merged))
names(sub.merged) <- gsub("-std\\(\\)", "_Std", names(sub.merged))
#clean up other fields
names(sub.merged) <- gsub("[(][)]", "", names(sub.merged))
names(sub.merged) <- gsub("Acc", "Accelerometer",names(sub.merged))
names(sub.merged) <- gsub("Gyro", "Gyroscope", names(sub.merged))
names(sub.merged) <- gsub("Mag", "Magnitude", names(sub.merged))
names(sub.merged) <- gsub("-", "_", names(sub.merged))


#calculate mean for each activity, subject combination
clean <- aggregate(. ~ activity + subject, data = sub.merged, FUN = mean)
names(clean)

# switch placement of subject and activity
cleaned <- cbind(clean[,c("subject","activity")], clean[,3:68])

#output to a csv file
write.table(cleaned, "tidied_data.csv", row.names=FALSE, sep=",")

#is the data tidy?
#check names
names(cleaned)