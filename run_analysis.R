#Coursera: Getting and Cleaning Data Course
#Authour: M.Brink
#Date: 13 Jnauary 2016

#Load requried libraries
library(read.table)
library (plyr)

mywd <- "D:/Users/Michael.Brink/Desktop/Proto/UCI HAR Dataset"

#Import & merge data
##Import
features     = read.table('./features.txt',header=FALSE) 
activityType = read.table('./activity_labels.txt',header=FALSE) 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE) 
xTrain       = read.table('./train/x_train.txt',header=FALSE) 
yTrain       = read.table('./train/y_train.txt',header=FALSE) 

# Assign appropriate col names
names(activityType)  = c('activityId','activityType')
names(subjectTrain)  = "subjectId"
names(xTrain)        = features[,2]
names(yTrain)        = "activityId"

# Create combined data frame by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

#Now, for teh test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
xTest       = read.table('./test/x_test.txt',header=FALSE)
yTest       = read.table('./test/y_test.txt',header=FALSE) 

# Assign appropriate col names
names(subjectTest) = "subjectId"
names(xTest)       = features[,2]
names(yTest)       = "activityId"

# Create combined data frame by merging yTrain, subjectTrain, and xTrain
testData = cbind(yTest,subjectTest,xTest)

# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

#Retrieve column names to extract mean and std.dev
n <- names(finalData)
x <- finalData[, grepl("-mean..|-std..|activity..|subject..", n) & !grepl("mean..-|-meanFreq..|-std()..-", n)]

#add activity-type names to dataset
finalData = merge(x,activityType,by='activityId',all.x=TRUE)

#update column names vector with names of merged data frame
n <- names(finalData)

#Tidy up var names with better descriptions
head(n)
## create a function to apply several gsub replacement for inappropriate strings
replacements <- function (x){
n[x] = gsub("\\()","",n[x])
n[x] = gsub("-std$","StdDev",n[x])
n[x] = gsub("-mean","Mean",n[x])
n[x] = gsub("^(t)","txme",n[x])
n[x] = gsub("^(f)","freq",n[x])
n[x] = gsub("([Gg]ravxty)","Gravxty",n[x])
n[x] = gsub("[Bb]ody","Body",n[x])
n[x] = gsub("[Gg]yro","Gyro",n[x])
n[x] = gsub("AccMag","AccMagnxtude",n[x])
n[x] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnxtude",n[x])
n[x] = gsub("JerkMag","JerkMagnxtude",n[x])
n[x] = gsub("GyroMag","GyroMagnxtude",n[x])
}

n <- unlist(lapply(1:length(n), replacements))
names(finalData) <- n

#Creating the new, tidy dataset:
#Instruction: From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
require(dplyr) #call this library for the aggreagate function will will come in handy to summarise
tidydf <- aggregate(finalData[, c(-1, -2, -21)], by = list(finalData$activityId, finalData$subjectId), "mean")
#Rename activityId and subjectId
names(tidydf)[1] = n[1]
names(tidydf)[2] = n[2]

#merge the df with activity type to get the activity type back in
tidydf <- merge(tidydf, activityType, by = 'activityId', all.x=TRUE)

#Finallt, write to table to upload to coursera:
write.table(tidydf, paste0(mywd, "/tidyDataFrame.txt")  ,row.name=FALSE) 


