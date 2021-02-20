---
title: "README"
author: "Tonya MacDonald"
date: "2/20/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Coursera - Getting and Cleaning Data Week 4 Project

Project instructions:

You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Load Packages and Download Data

```{r}
# load packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)
path <- getwd()

# download data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, file.path(path, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")
```

### Load data into variables

```{r}
# load activity labels
activity_labels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))

# load features
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))

featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])

# load measurements
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements)

```

### Load the features we want

```{r}
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
```

### Merge the data sets

```{r}
# merge datasets
combined <- rbind(train, test)
```

### Create a file with the cleaned data

```{r}
# write clean data to file
fwrite(x = combined, file = "tidy_data.txt", quote = FALSE)
```


