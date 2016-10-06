run.analysis = function(X){

# Use the reshape2 library

library(reshape2)

# Name, download and unzip the datafile and URL

datafile = "getdata_dataset.zip"
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
download.file(URL, datafile)
unzip(filename) 

# Load features & activities
AcLabels = read.table("UCI HAR Dataset/activity_labels.txt")
Feat = read.table("UCI HAR Dataset/features.txt")

# Define second column as character
AcLabels[,2] = as.character(AcLabels[,2])
Feat[,2] <- as.character(Feat[,2])

# Create a set with the average and the stdev
Feat2 = grep(".*mean.*|.*std.*", Feat[,2])
Feat2.Descriptions = Feat[Feat1,2]
Feat2.Descriptions = gsub('-mean', 'Mean', Feat2.Descriptions)
Feat2.Descriptions = gsub('-std', 'Std', Feat2.Descriptions)
Feat2.Descriptions = gsub('[-()]', '', Feat2.Descriptions)

# Read the sets in with only the desired specifications - first the training set

Traintotal = read.table("UCI HAR Dataset/train/X_train.txt")[Feat2]
TrainY = read.table("UCI HAR Dataset/train/Y_train.txt")
TrainSubjects = read.table("UCI HAR Dataset/train/subject_train.txt")
Train = cbind(Traintotal, TrainY, TrainSubjects)

# Now the test set

Testtotal = read.table("UCI HAR Dataset/test/X_test.txt")[Feat2]
TestY = read.table("UCI HAR Dataset/test/Y_test.txt")
TestSubjects = read.table("UCI HAR Dataset/test/subject_test.txt")
Test = cbind(Testtotal, TestY, TestSubjects)

# Combine both sets
CompleteData <- rbind(Train, Test)
colnames(CompleteData) = c("Subject", "Activity", Feat2.Descriptions)

# Create factors based on the data
CompleteData$activity = factor(CompleteData$activity, levels = AcLabels[,1], labels = AcLabels[,2])
CompleteData$subject = as.factor(CompleteData$subject)

CompleteData.melted = melt(CompleteData, id = c("subject", "activity"))
CompleteData.mean <- dcast(CompleteData.melted, subject + activity ~ variable, mean)

# Write to text file
write.table(CompleteData.mean, "tidyset.txt", row.names = FALSE, quote = FALSE)
}