# 1. Read Training and Test data, combine them using rbind function. Set the stage for the analysis
trainData <- read.table("./projectdata/train/X_train.txt")  # Fetches 7352 observations on 561 attributes 
testData <- read.table("./projectdata/test/X_test.txt") # Fetches 2947 observations on 561 attributes as test data
X <- rbind(trainData, testData) # creates 10299 x 561 data by combining (by rows) training and test data

subtrainData <- read.table("./projectdata/train/subject_train.txt") # Fetches 
subtestData <- read.table("./projectdata/test/subject_test.txt")
S <- rbind(subtrainData, subtestData)  #creates train and test subjects combined with a dim of 10299x1 

ytrainData <- read.table("./projectdata/train/y_train.txt")
ytestData <- read.table("./projectdata/test/y_test.txt")
Y <- rbind(ytrainData, ytestData)  # creates 10299x1 data vector

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

featuresData <- read.table("./projectdata/features.txt")
indfeatures <- grep("-mean\\(\\)|-std\\(\\)", featuresData[, 2])  # use grep text manipulation function to get mean and std attributes
X <- X[, indfeatures]
names(X) <- featuresData[indfeatures, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))  # see last slide of the lecture Editing Text Variables (week 4)

# 3. Uses descriptive activity names to name the activities in the data set

activitiesData <- read.table("./projectdata/activity_labels.txt")
activitiesData[, 2] = gsub("_", "", tolower(as.character(activitiesData[, 2])))
Y[,1] = activitiesData[Y[,1], 2]
names(Y) <- "activity" # makes Y data class to 'activity'

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"   #makes subject data class to 'subject'
cleaned <- cbind(S, Y, X)  #column bind S, Y and X data
write.table(cleaned, "merged_dataset.txt")   #creates combined data set and write to a file

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]   #gets unique subject data 
numSubjects = length(unique(S)[,1])   #gets number of subjects using length function
numActivities = length(activitiesData[,1])   # gets number of activities using length function
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]  #creates a vector 'result' with 'subject' times 'activities' rows

row = 1
for (s in 1:numSubjects) {    # outer loop to iterate through number of 'subjects'
  for (a in 1:numActivities) {  #innner loop to iterate through number of 'activities'
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activitiesData[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_mean.txt")  #creates tidy data set file with means of activities and subjects