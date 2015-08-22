##Analysis Objectives:
    #1. Merges the training and the test sets to create one data set.
    #2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    #3. Uses descriptive activity names to name the activities in the data set
    #4. Appropriately labels the data set with descriptive variable names. 
    #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##STEP 1
##1. Merges the training and the test sets to create one data set.
    ##Read Applicable Datasets
    activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
    features <- read.table("./UCI HAR Dataset/features.txt")
    subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    Xtest <- read.table("./UCI HAR Dataset/test/X_test.txt")
    Xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
    labelTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
    labelTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
    
    #Set headers on Xtest and Xtain
    names(Xtest) <- features[,2]
    names(Xtrain) <- features[,2]
    
    #Combine the two datasets
    Xdata <- rbind(Xtest,Xtrain)
    
##STEP 2
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
    
    #Pull out only columns from Xdata that has "-mean" or "-std"
      pattern <- c("-mean()","-std()")
      Xdata <- Xdata[,grep(paste(pattern,collapse="|"), names(Xdata))]
      ##Remove meanFreq Columns
      Xdata <- Xdata[,grep("meanFreq", names(Xdata), invert=TRUE)]
    
##STEP 3
#3. Uses descriptive activity names to name the activities in the data set
    ##Combine subject tables and set headers
    subjectTest$Set <- "Test"  #This will be used once we merge the two sets to differeniate
    subjectTrain$Set <- "Training" #This will be used once we merge the two sets to differeniate
    subjectX <- rbind(subjectTest, subjectTrain)
    names(subjectX) <- c("Subject","Set")
    
    ##Combine activity labels
    names(activityLabels) <- c("Activity.Code","Activity.Name")
    labelX <- rbind(labelTest, labelTrain)
    names(labelX) <- "Activity.Code"
    labelX <- merge(labelX, activityLabels, by="Activity.Code")
    
    ##Add Activity and Subject to the main data table
    data <- cbind(labelX, Xdata)
    data <- cbind(subjectX, data)

##STEP 4
#4. Appropriately labels the data set with descriptive variable names. 
  #This has already been done above as we went.
    
##STEP 5
#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
    #We are going to use the dplyr library to summarize the data.
    library(dplyr)
    
    #create a group_by table using the Subject, and Activity Name group_by
    Group.data <- group_by(data, Subject, Activity.Name)
    
    #summarize data by Subject and Activity.Name, taking the mean of each variable
    Avg.data <- summarise_each(Group.data, funs(mean),5:70) #where 5:70 are all variable columns
    
    