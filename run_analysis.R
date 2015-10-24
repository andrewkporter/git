## Assign Varible to the location and name for the file to download
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## Download the file
download.file(fileUrl, "./data/dataset.zip")

## Unzip file
unzip("./data/dataset.zip", exdir = "./data")

## Path ./Data/UCI HAR Dataset/train/Inertial Signals

## Training Data Set

    # Set initial valiable
    Xtr <- read.table("./Data/UCI HAR Dataset/train/X_train.txt")
    Ytr <- read.table("./Data/UCI HAR Dataset/train/y_train.txt")
    Str <- read.table("./Data/UCI HAR Dataset/train/subject_train.txt")
    vlt <- read.table("./Data/UCI HAR Dataset/features.txt")
    Str[2] <- Ytr[1]
    Str <- cbind(Str, Xtr)
    Train <- Str
    
## End Training Data Set
    
## Testing Data Set
    
    # Set initial valiable
    Xte <- read.table("./Data/UCI HAR Dataset/test/X_test.txt")
    Yte <- read.table("./Data/UCI HAR Dataset/test/y_test.txt")
    Ste <- read.table("./Data/UCI HAR Dataset/test/subject_test.txt")
    Ste[2] <- Yte[1]
    Ste <- cbind(Ste, Xte)
    Test <- Ste
    
## End Testing Data Set
    
## #1 Merges the training and the test sets to create one data set.
    
    mData <- rbind(Train, Test, deparse.level = 1)

    names(mData)[1] <- "Subject"
    names(mData)[2] <- "Activity"
    
    nc <- ncol(mData) - 3

    ## Lable Columns from the Features Table
    
    for(x in 1:nc){
        
        cname <- as.character(vlt[x,2])
        y <- x + 2
        names(mData)[y] <- cname
        
    }
  
## End Merge
    
## #2 Extracts only the measurements on the mean and standard deviation for each measurement.
    
    clist <- mData[grep("mean()", names(mData), fixed = TRUE, value = TRUE)] ## Select mean Columns
    
    clist <- cbind(clist, mData[grep("std()", names(mData), fixed = TRUE, value = TRUE)]) ## Select and append columns foor Std Columns
    
    ## clist <- clist[,order(colnames(clist))] ## Reorder columns alpha sort
    
    cmData <- cbind("Subject" = mData$Subject, "Activity" = mData$Activity, clist) ## Append Activity, Subject & Data Source
    
## End Extract
    
## #3 Uses descriptive activity names to name the activities in the data set
    
    anames <- read.table("./Data/UCI HAR Dataset/activity_labels.txt") ## Read Labels
    
    anames <- as.vector(anames$V2) ## create list of names
    
    cmData$Activity <- factor(cmData$Activity, labels=c(anames)) ## Factorize the Activity Column
    
## End Actity Names
    
## #4 Appropriately labels the data set with descriptive variable names & reorder the columns
    
    names(cmData) <- gsub("^t", "Time", names(cmData))
    names(cmData) <- gsub("^f", "Frequency", names(cmData))
    names(cmData) <- gsub("Acc", "Accelerometer", names(cmData))
    names(cmData) <- gsub("Gyro", "Gyroscope", names(cmData))
    names(cmData) <- gsub("Mag", "Magnitude", names(cmData))
    names(cmData) <- gsub("-mean[(][])]", "Mean", names(cmData))
    names(cmData) <- gsub("-std[(][])]", "Std", names(cmData))
    names(cmData) <- gsub("-X", "-X-Axis", names(cmData))
    names(cmData) <- gsub("-Y", "-Y-Axis", names(cmData))
    names(cmData) <- gsub("-Z", "-Z-Axis", names(cmData))
    
    ## Reorder the columns in the Table
    cmData <- cmData[,order(colnames(cmData))] ## Sort Columns Alpha
    col_idx <- grep("Subject", names(cmData))  
    cmData <- cmData[, c(col_idx, (1:ncol(cmData))[-col_idx])] ## Move Subject back to the front of the Data.Frame

## End Actity Names 
    
## #5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    acmData = ddply(cmData, c("Subject","Activity"), numcolwise(mean)) ## averagge all columns by subject by activity
    
    write.table(acmData, "./data/AvgSubActData.txt", row.names = FALSE)
    
## End Actity Names 
    

rm(list = ls()) ## Clean up Global Environment


    