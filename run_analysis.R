# 1. Merge the training and test datasets

setwd("C:/Users/bwei/Desktop/UCI HAR Dataset") # Sets working directory
feature <- read.table('./features.txt', header = FALSE) # Imports features.txt
activityLabels <- read.table('./activity_labels.txt', header = FALSE)
trainSubject <- read.table('./train/subject_train.txt', header = FALSE)
trainX <- read.table('./train/x_train.txt', header = FALSE)
trainY <- read.table('./train/y_train.txt', header = FALSE)

colnames(activityLabels) <- c("Activity_ID", "Activity_Type")
colnames(trainSubject) <- c("Subject_ID")
colnames(trainX) <- feature[,2]
colnames(trainY) <- c("Activity_ID")

totalTrain <- cbind(trainY, trainSubject, trainX)

# Now will read in the test data
testSubject <- read.table('./test/subject_test.txt', header = FALSE)
testX <- read.table('./test/x_test.txt', header = FALSE)
textY <- read.table('./test/y_test.txt', header = FALSE)

colnames(testSubject) <- c("Subject_ID")
colnames(testX) <- feature[,2]
colnames(testY) <- c("Activity_ID")

totalTest <- cbind(testY, testSubject, testX)

totalData <- rbind(totalTrain, totalTest)

columns <- colnames(totalData)  # Vector of names of columns
# Create logical vector like quiz 3 that contains true or false for all relevant columns
# grepl is like a SQL LIKE Operator with the .. as the wildcard FYI
tfvector = (grepl("Activity..", columns) | grepl("Subject..", columns) | grepl("-mean..", columns) & !grepl("-meanFreq..", columns) & !grepl("mean..-", columns) | grepl("-std..", columns) & !grepl("-std()..-", columns))

totalData <- totalData[tfvector == TRUE]

# Merge data by Activity ID
totalData <- merge(totalData, activityLabels, by = "Activity_ID", all.x=TRUE)
# Rename Columns for descriptivity
colnames(totalData) <- c('Activity_ID',
'Subject_ID',
'meanBodyAccMagt',
'stdBodyAccMagt',
'meanGravityAccMagt',
'stdGravityAccMagt',
'meanBodyAccJerkMagt',
'stdBodyAccJerkMagt',
'meanBodyGyroMagt',
'stdBodyGyroMagt',
'meanBodyGyroJerkMatg',
'stdBodyGyroJerkMagt',
'meanBodyAccMagf',
'stdBodyAccMagf',
'meanBodyAccJerkMagf',
'stdBodyAccJerkMagf',
'meanBodyGyroMagf',
'stdBodyGyroMagf',
'meanBodyGyroJerkMagf',
'stdBodyGyroJerkMagf',
'Activity_Type')

# Take out Activity Type Variable
final <- totalData[,names(totalData) != 'Activity_Type']

# Aggregate the means of variables for each activity and subject ID
tidySet <- aggregate(final[,names(final) != c('Activity_ID', 'Subject_ID')], by=list(Activity_ID=final$Activity_ID,Subject_ID = final$Subject_ID),mean)

#Get activity names back
tidySet <- merge(tidySet, activityLabels, by = "Activity_ID", all.x = TRUE)

# Export!
write.table(tidySet, './tidySet.txt, row.names = TRUE, sep = '\t')
