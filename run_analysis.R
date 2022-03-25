#run_analysis.R

library(dplyr)

#Download dataset

Dataset <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Dataset, "Dataset.zip")
unzip("Dataset.zip", exdir = "Dataset.zip")
dir.create("Dataset")

#Getting all dataframes from the dataset
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#STEP 1 : Merges the training and the test sets to create one data set.

x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subjects <- rbind(subject_train, subject_test)
merged_dataset <- cbind(subjects, y, x)

#STEP 2 : Extracts only the measurements on the mean and standard deviation for each measurement.

tidydata <- merged_dataset %>% 
select(subject, code, contains("mean"), contains("std"))

#STEP 3 : Uses descriptive activity names to name the activities in the data set

tidydata$code <- activities[tidydata$code, 2]

#STEP 4 : Appropriately labels the data set with descriptive variable names. 

names(tidydata)[2] = "activity"
names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
names(tidydata)<-gsub("^t", "Time", names(tidydata))
names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
names(tidydata)<-gsub("angle", "Angle", names(tidydata))
names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))

#STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata_independent <- tidydata %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(tidydata_independent, "tidydata_independent.txt", row.name=FALSE)

