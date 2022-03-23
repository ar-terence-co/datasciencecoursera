library(plyr)
library(dplyr)
library(stringr)

run_analysis <- function(destdir = "3_getting_and_cleaning_data_proj/data", force_download = FALSE) {
  filename = "smartphones"
  download_and_extract_data(destdir, filename = filename, force_download=force_download)
  
  datadir = paste(destdir, filename, "UCI HAR Dataset", sep="/")
  dts <- load_to_dts(datadir)
  
  tidy_dt <- generate_tidy_data(dts)
  fwrite(tidy_dt, paste0(destdir, "/", filename, "-tidy.csv"))
  
  summary_dt <- tidy_dt %>% group_by(subject, activity) %>% summarize_all(mean)
  fwrite(tidy_dt, paste0(destdir, "/", filename, "-summary.csv"))
  
  summary_dt
}

download_and_extract_data <- function(destdir, filename = "smartphones", force_download = FALSE) {
  filepath <- paste0(destdir, "/", filename, ".zip")
  if (force_download | !file.exists(filepath)) {
    download.file(
      "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
      destfile=filepath,
    )
    write_date_downloaded(destdir)
  }
  datadir <- paste0(destdir, "/", filename)
  unzip(filepath, exdir=datadir)
}

write_date_downloaded <- function(destdir, filename = "date_downloaded.txt") {
  date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  filepath <- paste(destdir, filename, sep="/")
  
  writeLines(date, con = filepath)
  print(date)
}

load_to_dts <- function(datadir) {
  activity_labels <- fread(
    paste(datadir, "activity_labels.txt", sep="/"),
    header=FALSE,
    col.names=c("id", "activity"),
  )
  feature_labels <- fread(
    paste(datadir, "features.txt", sep="/"),
    header=FALSE,
    col.names=c("id", "label"),
  )
  
  train_subjects <- fread(
    paste(datadir, "train/subject_train.txt", sep="/"),
    header=FALSE,
    col.names=c("subject"),
  )
  test_subjects <- fread(
    paste(datadir, "test/subject_test.txt", sep="/"),
    header=FALSE,
    col.names=c("subject"),
  )
  subjects <- rbind(train_subjects, test_subjects)
  
  train_features <- fread(
    paste(datadir, "train/X_train.txt", sep="/"),
    header=FALSE,
    col.names=feature_labels$label,
  )
  test_features <- fread(
    paste(datadir, "test/X_test.txt", sep="/"),
    header=FALSE,
    col.names=feature_labels$label,
  )
  features <- rbind(train_features, test_features)
  
  train_activities <- fread(
    paste(datadir, "train/y_train.txt", sep="/"),
    header=FALSE,
    col.names=c("activityid"),
  )
  test_activities <- fread(
    paste(datadir, "test/y_test.txt", sep="/"),
    header=FALSE,
    col.names=c("activityid"),
  )
  activities <- rbind(train_activities, test_activities)
  
  list(
    subjects = subjects,
    features = features,
    activities = activities,
    activity_labels = activity_labels
  )
}

generate_tidy_data <- function(dts) {
  subjects <- dts$subjects
  
  features <- dts$features %>% select(matches("-mean\\(\\)|-std\\(\\)"))
  names(features) <- generate_tidy_labels(names(features))
  
  activities <- dts$activities %>%
    left_join(dts$activity_labels, by = c("activityid" = "id")) %>%
    select(activity)
    
  cbind(subjects, activities, features)
}

generate_tidy_labels <- function(labels) {
  label_split <- str_split(labels, "-", simplify=TRUE)
  
  signal_domain <- substring(label_split[,1], 1, 1)
  measurement <- substring(label_split[,1], 2)
  aggregation <- label_split[,2]
  component <- label_split[,3]
  
  signal_domain <- mapvalues(
    signal_domain,
    from=c("t", "f"),
    to=c("TimeDomain", "FrequencyDomain")
  )
  measurement <- mapvalues(
    measurement,
    from=c(
      "GravityAcc",
      "GravityAccMag",
      "BodyAcc",
      "BodyAccMag",
      "BodyAccJerk",
      "BodyAccJerkMag",
      "BodyBodyAccJerkMag",
      "BodyGyro",
      "BodyGyroMag",
      "BodyGyroJerk",
      "BodyGyroJerkMag"
    ),
    to=c(
      "GravityAccelerometerSignal",
      "GravityAccelerometerSignalMagnitude",
      "BodyAccelerometerSignal",
      "BodyAccelerometerSignalMagnitude",
      "BodyAccelerometerJerk",
      "BodyAccelerometerJerkMagnitude",
      "BodyAccelerometerJerkMagnitude",
      "BodyGyroscopeSignal",
      "BodyGyroscopeSignalMagnitude",
      "BodyGyroscopeJerk",
      "BodyGyroscopeJerkMagnitude"
    )
  )
  aggregation <- mapvalues(
    aggregation,
    from=c("mean()", "std()"),
    to=c("Mean", "StandardDeviation")
  )
  paste0(signal_domain, measurement, aggregation, component)
}
