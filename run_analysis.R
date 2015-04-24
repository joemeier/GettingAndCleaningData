run_analysis <- function () {

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

path <- getwd()
pathDat <- path

# (1) read files:

# (1.1) read subject files:
dtSubTrain <- fread(file.path(pathDat, "train", "subject_train.txt"))
dtSubTest  <- fread(file.path(pathDat, "test" , "subject_test.txt" ))

# (1.2) read activity files:
dtActTrain <- fread(file.path(pathDat, "train", "Y_train.txt"))
dtActTest  <- fread(file.path(pathDat, "test" , "Y_test.txt" ))

# (1.3) read data files:
file2DT <- function (f) {
        df <- read.table(f)
        dt <- data.table(df)
}
dtTrain <- file2DT(file.path(pathDat, "train", "X_train.txt"))
dtTest  <- file2DT(file.path(pathDat, "test" , "X_test.txt" ))


# (2) merge training and test set:

# (2.1) concatenate subject tables:
dtSub <- rbind(dtSubTrain, dtSubTest)
setnames(dtSub, "V1", "subject")

# (2.2) concatenate activity tables:
dtAct <- rbind(dtActTrain, dtActTest)
setnames(dtAct, "V1", "activityNum")

# (2.3) concatenate data tables:
dt <- rbind(dtTrain, dtTest)

# (2.4) merge subject/activity/data table columns:
dtSub <- cbind(dtSub, dtAct)
dt <- cbind(dtSub, dt)

# (2.5) set data table key:
setkey(dt, subject, activityNum)


# (3) extract the mean and standard deviation:

# (3.1) read features.txt file:
dtFeat <- fread(file.path(pathDat, "features.txt"))
setnames(dtFeat, names(dtFeat), c("featureNum", "featureName"))

# (3.2) subset only measurements for the mean and standard deviation:
dtFeat <- dtFeat[grepl("mean\\(\\)|std\\(\\)", featureName)]

# (3.3) convert column numbers to a vector of variable names matching columns in dt:
dtFeat$featureCode <- dtFeat[, paste0("V", featureNum)]
#head(dtFeat)
#dtFeat$featureCode

# (3.4) subset these variables using variable names:
select <- c(key(dt), dtFeat$featureCode)
dt <- dt[, select, with=FALSE]

# (4) descriptive activity names:

# (4.1) read activity_labels.txt file:
dtActNames <- fread(file.path(pathDat, "activity_labels.txt"))
setnames(dtActNames, names(dtActNames), c("activityNum", "activityName"))

# (5) label with descriptive activity names:

# (5.1) merge activity labels:
dt <- merge(dt, dtActNames, by="activityNum", all.x=TRUE)

# (5.2) add activityName as a key.
setkey(dt, subject, activityNum, activityName)

# (5.3) melt data table:
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# (5.4) merge activity name:
dt <- merge(dt, dtFeat[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# (5.5) create new variable, activity that is equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# (5.6) seperate features from featureName using grepthis.
grepthis <- function (regex) {
        grepl(regex, dt$feature)
}

# Features with 1 category:
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

# Features with 2 categories:
n <- 2
y <- matrix(seq(1, n), nrow=n)

x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))

x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))

x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))

x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))

# Features with 3 categories:
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# (6) Create a tidy data set with the average of each variable for each activity and each subject:
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# (7) save the tidy data set to file:
f <- file.path(path, "HAR_tidy_dataset.txt")
write.table(dtTidy, f, quote = FALSE, sep = "\t", row.names = FALSE)

}
