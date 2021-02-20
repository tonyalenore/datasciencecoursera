
# load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()

# download data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")

# load activity labels
activity_labels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
head(activity_labels)

# load features
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
head(features)
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])

# load measurements
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)
head(measurements)

# read datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]

setnames(train, colnames(train), measurements)

y_train <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt"), col.names = c("Activity"))

subject_train <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt"), col.names = c("SubjectNum"))

train <- cbind(subject_train, y_train, train)

test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]

setnames(test, colnames(test), measurements)

y_test <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt"), col.names = c("Activity"))

subject_test <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))

test <- cbind(subject_test, y_test, test)

# merge datasets
combined <- rbind(train, test)

# write clean data to file
fwrite(x = combined, file = "tidy_data.txt", quote = FALSE)


