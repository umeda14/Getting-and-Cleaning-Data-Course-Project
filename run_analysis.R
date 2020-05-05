library(dplyr)

# download the data
if(!file.exists("data.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip")  
}

# unzip the data
if(!file.exists("UCI HAR Dataset/"))
unzip("data.zip")

# read the features data
features <- read.table("./UCI HAR Dataset/features.txt")
features <- as.character(features[,2])

# read the train data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt") 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train_data <- data.frame(subject_train, y_train, x_train)

# read the test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test_data <- data.frame(subject_test, y_test, x_test)

#Step1: Merges the training and the test sets to create one data set.
merged_data <- rbind(train_data, test_data)
colnames(merged_data)<- c(c("subject", "activity"), features)

#Step2: Extracts only the measurements on the mean and standard deviation for each measurement.
valid_column_names <- make.names(names=names(merged_data), unique=TRUE, allow_ = TRUE)
names(merged_data) <- valid_column_names
extracted_data <- merged_data %>% select(subject, activity, contains("mean"), contains("std"))

#Step3: Uses descriptive activity names to name the activities in the data set.
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
extracted_data$activity <- activity_labels[extracted_data$activity, 2]

#Step4: Appropriately labels the data set with descriptive variable names.
appropriate_names <- names(extracted_data)
appropriate_names <- gsub("^t", "TimeDomain", appropriate_names)
appropriate_names <- gsub("^f", "FrequencyDomain", appropriate_names)
appropriate_names <- gsub("Acc", "Accelerometer", appropriate_names)
appropriate_names <- gsub("angle.t", "AngleTimeDomain", appropriate_names)
appropriate_names <- gsub("angle", "Angle", appropriate_names)
appropriate_names <- gsub("gravity", "Gravity", appropriate_names)
appropriate_names <- gsub("Gyro", "Gyroscope", appropriate_names)
appropriate_names <- gsub("Mag", "Magnitude", appropriate_names)
appropriate_names <- gsub("meanFreq", "MeanFrequency", appropriate_names)
appropriate_names <- gsub("mean", "Mean", appropriate_names)
appropriate_names <- gsub("std", "StandardDeviation", appropriate_names)
appropriate_names <- gsub("\\.+", "_", appropriate_names)
names(extracted_data) <- appropriate_names

#Step5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- extracted_data %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))
write.table(tidy_data, "tidy_data.txt", row.name=FALSE, quote = FALSE)

