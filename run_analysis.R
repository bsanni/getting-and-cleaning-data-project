# Merges the training and the test sets to create one data set

# Extracts only the measurements on the mean and standard deviation for each measurement.

# Uses descriptive activity names to name the activities in the data set

# Appropriately labels the data set with descriptive variable names.

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Load pacman and other packages
if (!require("pacman")) {install.packages("pacman")}
pacman :: p_load(pacman,data.table,reshape2)
# get wirking data
dataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataUrl,file.path(getwd(), "data.zip"))
unzip (zipfile = "data.zip", overwrite = TRUE)
# load two data sets - activity_labels and features
activity_labels <- fread(file.path(getwd(),"UCI HAR Dataset/activity_labels.txt")
                         , col.names = c("classLabel","activityName"))

features <- fread(file.path(getwd(),"UCI HAR Dataset/features.txt")
                  , col.names = c("index","featureName"))
# extract mean or std from features
featureName_limit <- grep("(mean|std)\\(\\)", features[, featureName])
measurement <- features[featureName_limit, featureName]

# clean measurement - replace/remove pattern "()"
measurement <- gsub("[()]","",measurement)



## load train data sets
# load x_train, filter only names in 'featureName_limit'
train <- fread(file.path(getwd(),"UCI HAR Dataset/train/X_train.txt"))[, featureName_limit, with = FALSE] 

# change train column name base on the measurement
data.table::setnames(train, colnames(train), measurement)
head(train,3)

train_activity <- fread(file.path(getwd(), "UCI HAR Dataset/train/y_train.txt")
                        , col.names = c("activity"))
train_subject <- fread(file.path(getwd(), "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("subjectnumber"))
train <- cbind(train_subject, train_activity, train)
head(train)



## load test data sets
test <- fread(file.path(getwd(),"UCI HAR Dataset/test/X_test.txt"))[, featureName_limit, with = FALSE] 

# change test column name base on the measurement
data.table::setnames(test, colnames(test), measurement)
head(test,3)


test_activity <- fread(file.path(getwd(), "UCI HAR Dataset/test/y_test.txt")
                       , col.names = c("activity"))
test_subject <- fread(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("subjectnumber"))
test <- cbind(test_activity, test_subject, test)
head(test)

# merge data sets
combine <- rbind(train, test)

# convert classLabel to activityName
#?factor
combine[["activity"]] <- factor(combine[, activity]
                                , levels = activity_labels[["classLabel"]]
                                , labels = activity_labels[["activityName"]])

combine[["subjectnumber"]] <- as.factor(combine[["subjectnumber"]])


## reshape data - melt and cast data table 
#melt - convert to a molten data frame, melt down to variable and value
combine <- reshape2::melt(combine, id = c("subjectnumber", "activity"))

#dcast - cast a molten data frame to a data frame, with average of subjectnumber + activity 
combine <- reshape2::dcast(combine, subjectnumber + activity ~ variable, fun.aggregate = mean)


## write final tidy data into new file
#fwrite(combine, file="tidyData.txt")
write.table(combine, file="tidyData.txt", row.name=FALSE, quote = FALSE)  

rm(list=ls())
pacman::p_unload()


