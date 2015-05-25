library(dplyr)
#1.Merge DataSets
features <- read.table("./UCI HAR Dataset/features.txt")
colNames <- features[,2]

trainingXSet <- read.table("./UCI HAR Dataset/train/X_train.txt") 
names(trainingXSet) <- colNames
trainingYSet <- read.table("./UCI HAR Dataset/train/y_train.txt") 
names(trainingYSet) <- c("activity")
trainingSet <- cbind(trainingYSet, trainingXSet)

testXSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
names(testXSet) <- colNames
testYSet <- read.table("./UCI HAR Dataset/test/y_test.txt")
names(testYSet) <- c("activity")
testSet <- cbind(testYSet, testXSet)

mergeSet <- rbind(trainingSet, testSet)

#2.Extracts only mean and standard deviation 
meansSet <- mergeSet[, c(1,grep("mean()", colnames(mergeSet)))]
stdSet <- mergeSet[, grep("std()", colnames(mergeSet))]
extractSet <- cbind(meansSet, stdSet)

#3.Uses descriptive activity names
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity_labels <- activity_labels[,2]
for (i in 1:length(activity_labels)){
    extractSet$activity[extractSet$activity == i] <- as.character(activity_labels[i])
}

#4.Appropriately labels 
#Done en step one

#5.Independent tidy data set with the average of each variable 
for (i in 1:length(activity_labels)){
    tmpDF <- extractSet[extractSet$activity == activity_labels[i],]
    colMeansByActivity <- colMeans(select(tmpDF, 2:ncol(tmpDF)))
    tmpFRow <- data.frame(c(as.character(activity_labels[i])))
    names(tmpFRow) <- c("activity")
    tmpRow <- cbind(tmpFRow, t(colMeansByActivity))
    if(i == 1) {
        averageSet <- tmpRow
    }else {
        averageSet <- rbind(averageSet, tmpRow)
    }    
}
write.table(averageSet, "Step5Answer.txt",row.names=FALSE) 

