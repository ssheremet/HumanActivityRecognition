run_analysis <- function() {

     require(data.table)
     
     #read data, labels and subjects
     testSet <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt")
     testLabels <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt")
     testSubj <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt")
     
     trainSet <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt")
     trainLabels <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt")
     trainSubj <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt")
     
     ##read activity labels, features
     activityLabels <- read.table(".\\UCI HAR Dataset\\activity_labels.txt")
     features <- read.table(".\\UCI HAR Dataset\\features.txt")
     
     ##create data subsets with mean and std. deviation values only
     mean.std.Values <- grep("mean()\\b|std()\\b", features[,2])
     testSubSet <- testSet[, mean.std.Values]
     trainSubSet <- trainSet[, mean.std.Values]
     
     ##create vector with columns names, removing brackets '(', ')'
     colNames <- gsub("\\(.*?\\)", "", features[grep("mean()\\b|std()\\b", features[,2]),2])
     
     ##create final dataset
     finalTestSet <- cbind(testSubSet, testSubj, testLabels)
     finalTrainSet <- cbind(trainSubSet, trainSubj, trainLabels)
     
     ##merge data test and train data sets
     finalSet <- rbind(finalTestSet, finalTrainSet)
     
     ##set columns names
     colnames(finalSet) <- c(colNames,"Subject","activityKey")
     colnames(activityLabels) <- c("activityKey", "Activity")
     
     ##merge data to achive descriptive activity names
     data <- as.data.frame(merge(as.data.table(finalSet), as.data.table(activityLabels), by="activityKey"))
     
     ##drop activityKey column
     data <- data[,!(names(data) %in% "activityKey")]
     
     ##create tidy data
     tidyData <- aggregate(data[,1:length(mean.std.Values)], by=list(data$Activity, data$Subject), FUN=mean)
     colnames(tidyData) <- c("Activity", "Subject", colNames)
     
     write.table(tidyData, "tidy_data.txt", row.names=FALSE, quote=FALSE, sep="\t")
     
     
}