# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

# Assumes https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20dataset.zip is loaded in the working directory
# rm(list = ls())

# Load the required libraries
print("Loading libraries: data.table, reshape2")
flush.console()
library(data.table)
library(reshape2)

# Define the root directory of the data
print("Defining data directories")
flush.console()

dir <- "UCI HAR dataset"
dir.train <- paste(dir, "train", sep="/")
dir.test <- paste(dir, "test", sep="/")

# Read the text files as data tables
print("Reading text files")
flush.console()

dt.subject_train <- read.table(paste(dir.train, "subject_train.txt", sep="/"))
dt.subject_test <- read.table(paste(dir.test, "subject_test.txt", sep="/"))

dt.x_train <- read.table(paste(dir.train, "X_train.txt", sep="/"))
dt.y_train <- read.table(paste(dir.train, "y_train.txt", sep="/"))
dt.x_test <- read.table(paste(dir.test, "X_test.txt", sep="/"))
dt.y_test <- read.table(paste(dir.test, "y_test.txt", sep="/"))

# The label files have a 1st column with the row number, skip that and only read the 2nd column
dt.labels.features <- read.table(paste(dir, "features.txt", sep="/"))[,2]
dt.labels.activity <- read.table(paste(dir, "activity_labels.txt", sep="/"))[,2]

#  Merge the training and the test sets to create one data set and add descriptive labels
print("Merging label tables")
flush.console()

dt.subject <- rbind(dt.subject_test, dt.subject_train)
colnames(dt.subject) <- "Subject_Number"

# Label the test and train data sets
names(dt.x_test) = dt.labels.features
names(dt.x_train) = dt.labels.features

# Extract only the measurements on the mean and standard deviation for each measurement
print("Extracting mean and standard deviation data for each measurement")
flush.console()

dt.x_test = dt.x_test[,grepl("mean|std", dt.labels.features)]
dt.x_train = dt.x_train[,grepl("mean|std", dt.labels.features)]

#  Merge the training and the test sets to create one data set and add descriptive labels
print("Merging test and train tables")
flush.console()

dt.test_train = rbind(dt.x_test, dt.x_train)

# Add a new column with the Acitiviy names and merge the datble
dt.y_test[,2] = dt.labels.activity[dt.y_test[,1]]
dt.y_train[,2] = dt.labels.activity[dt.y_train[,1]]
dt.activities = rbind(dt.y_test, dt.y_train)

# label the columns
names(dt.activities) = c("Activity_ID", "Activity_Name")

# combine all 3 tables
dt.test_train <- cbind(as.data.table(dt.subject), dt.activities, dt.test_train)

# Calculate the average of each variable for each activity and each subject
print("Calculating averate of each variable and each subject")
flush.console()

list_labels   = c("Subject_Number", "Activity_ID", "Activity_Name")
data_labels = setdiff(colnames(dt.test_train), list_labels)
dt.melted = melt(dt.test_train, id = list_labels, measure.vars = data_labels)

# create a second, independent tidy data set with the average of each variable for each activity and each subject.

dt.tidydata = dcast(dt.melted, Subject_Number + Activity_ID ~ variable, mean)

print("Writing tidy data set to:")
print(paste(getwd(), dir,"tidy_data.txt", sep="/"))
flush.console()

write.table(dt.tidydata, file = paste(dir, "tidy_data.txt", sep="/"), row.name = FALSE)
