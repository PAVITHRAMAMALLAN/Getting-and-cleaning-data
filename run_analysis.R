
library(data.table)
library(dplyr)

# Read meta
featName <- read.table("UCI HAR Dataset/features.txt")
actLabel <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read training data
subTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
actTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Read test data
subTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
actTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

######## Merges the training and the test sets to create one data set. ###################################################
#Merge train data and test data
sub <- rbind(subTrain, subTest)
act <- rbind(actTrain, actTest)
feat <- rbind(featTrain, featTest)

#Name the col
colnames(feat) <- t(featName[2])

#Merge data
colnames(act) <- "Activity"
colnames(sub) <- "Subject"
mergeData <- cbind(feat,act,sub)
######################################### END ##############################################

########## Extracts only the measurements on the mean and standard deviation for each measurement. 
colMeanSTD <- grep(".*Mean.*|.*Std.*", names(mergeData), ignore.case=TRUE)

reqColumns <- c(colMeanSTD, 562, 563)
dim(mergeData)

extractDataSet <- mergeData[,reqColumns]
dim(extractDataSet)

##################### END #########################################################################

############ Appropriately labels the data set with descriptive variable names.####################
names(extractDataSet)

names(extractDataSet)<-gsub("Acc", "Accelerometer", names(extractDataSet))
names(extractDataSet)<-gsub("Gyro", "Gyroscope", names(extractDataSet))
names(extractDataSet)<-gsub("BodyBody", "Body", names(extractDataSet))
names(extractDataSet)<-gsub("Mag", "Magnitude", names(extractDataSet))
names(extractDataSet)<-gsub("^t", "Time", names(extractDataSet))
names(extractDataSet)<-gsub("^f", "Frequency", names(extractDataSet))
names(extractDataSet)<-gsub("tBody", "TimeBody", names(extractDataSet))
names(extractDataSet)<-gsub("-mean()", "Mean", names(extractDataSet), ignore.case = TRUE)
names(extractDataSet)<-gsub("-std()", "STD", names(extractDataSet), ignore.case = TRUE)
names(extractDataSet)<-gsub("-freq()", "Frequency", names(extractDataSet), ignore.case = TRUE)
names(extractDataSet)<-gsub("angle", "Angle", names(extractDataSet))
names(extractDataSet)<-gsub("gravity", "Gravity", names(extractDataSet)
                           
#Check
names(extractDataSet)
# Looks good :)
########################### END ##########################################################

########From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extractDataSet$Subject <- as.factor(extractDataSet$Subject)
extractDataSet <- data.table(extractDataSet)      
                           
cleanData <- aggregate(. ~Subject + Activity, extractDataSet, mean)
cleanData <- cleanData[order(cleanData$Subject,cleanData$Activity),]
write.table(cleanData, file = "Tidy.txt", row.names = FALSE)


