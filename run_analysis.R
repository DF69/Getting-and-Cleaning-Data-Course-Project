# ----------- 1.Merges the training and the test sets to create one data set.
# upload files that will be used

Xtest <- read.table("X_test.txt")
ytest <- read.table("y_test.txt")

Xtrain <- read.table("X_train.txt")
ytrain <- read.table("y_train.txt")

SubTrain <- read.table("subject_train.txt")
SubTest <- read.table("subject_test.txt")

# merge in to groups Test and Train and finally in only one
Retest <- cbind(SubTest, ytest, Xtest)
Retrain <- cbind(SubTrain, ytrain, Xtrain)
dataAnalysis <- rbind(Retest, Retrain)

# ----------- 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# using features file select columns with "mean" and "std" words

features <- read.table("features.txt")
cualMean <- c(grep("mean()", features$V2, fixed = TRUE))      
cualStd <- c(grep("std()", features$V2, fixed = TRUE))

# sorting results and move two spaces to the right and add 1 and 2
cualesOriginal <- sort.int(c(cualMean, cualStd))
cuales <- c(c(1,2), cualesOriginal + 2)

# select columns
TidyData1 <- dataAnalysis[,cuales]

# 3.Uses descriptive activity names to name the activities in the data set
activity <- read.table("activity_labels.txt")

# loop over all TidyData1 to change its second column(integer) to character with activities
for (i in 1:nrow(TidyData1)) {
            num <- as.numeric(TidyData1$V1.1[i])
            TidyData1$V1.1[i] <- as.character(activity$V2[num])
}

# 4.Appropriately labels the data set with descriptive variable names. 
# take names of columns from features, changes names of columns to create new names
newColNames <- data.frame(features[cualesOriginal,]) 

newColNames$V2 <- sub("^t","time", newColNames$V2)
newColNames$V2 <- sub("^f","freq", newColNames$V2)
newColNames$V2 <- sub("-mean()-X","MeanX", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-mean()-Y","MeanY", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-mean()-Z","MeanZ", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-std()-X","StdX", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-std()-Y","StdY", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-std()-Z","StdZ", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-mean()","Mean", newColNames$V2, fixed = TRUE)
newColNames$V2 <- sub("-std()","Std", newColNames$V2, fixed = TRUE)

# add two names for the first columns   
colnames(TidyData1) <- c(c("Subject","Activity"), newColNames$V2)

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
TidyData2 <- data.frame()

# loop to grouping by Subject(30) by Activity(6) and create a TidyData2
for (i in 1:30) {
                Subject <- TidyData1[TidyData1$Subject == i, ]
                MeanAct <- aggregate(Subject, by = list(Subject$Activity), FUN = mean, na.rm = TRUE)
                TidyData2 <- rbind(TidyData2, MeanAct) 
}
write.table(TidyData2, "c:/tidyData.txt", sep="\t", row.name=FALSE) 