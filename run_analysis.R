#Create directory, download file, unzip file
if(!file.exists("./assignment")){dir.create("./assignment")}
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url,destfile="./assignment/Data.zip")
unzip(zipfile="./assignment/Data.zip")


#Read test/train subject, test/train label, test/train data
testSub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
trainSub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt")
trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")

#Step 1: Merge the training and the test sets to create one data set
AllSubject <- rbind(testSub,trainSub)
AllLabel <- rbind(testLabel,trainLabel)
AllData <- rbind(testData, trainData)
library(dplyr)
AllSubject <- rename(AllSubject,subject=V1)
AllLabel <- rename(AllLabel,label=V1)
featurenames <- read.table("./UCI HAR Dataset/features.txt")
featurenames <- featurenames[,2]
names(AllData) <- featurenames
data <- cbind(AllSubject,AllLabel,AllData)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
meanstdnames <- grep("(.*-mean.*)|(.*-std.*)",featurenames,value = TRUE)
meanstdnames2 <- meanstdnames[!grepl("Freq",meanstdnames)]
extractcolumn <- c("subject","label",meanstdnames2)
meanstddata <- subset(data,select=extractcolumn)

#Step 3: Uses descriptive activity names to name the activities in the data set
activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
data2 <- merge(meanstddata,activity,by.x = "label",by.y = "V1",all = TRUE)
data2 <- rename(data2,activity=V2)
gooddata <- data2[c(2,69,3:68)] #reorder column

#Step 4: Appropriately labels the data set with descriptive variable names
vr <- colnames(gooddata)
vr <- gsub("^t","time",vr)
vr <- gsub("^f","frequency",vr)
vr <- gsub("Acc","Accelerometer",vr)
vr <- gsub("Gyro","Gyroscope",vr)
vr <- gsub("Mag","Magnitude",vr) 
vr <- gsub("BodyBody","Body",vr)
vr <- gsub("[()]","",vr)
names(gooddata) <- vr

#Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2)
meltdata <- melt(gooddata,id=c("subject","activity"))
tidyData <- dcast(meltdata,subject+activity~variable,mean)
write.table(tidyData,file="./UCI HAR Dataset/tidayData.txt",row.name=FALSE)

