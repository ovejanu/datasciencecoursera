##Peer Assessments - Getting and Cleaning Data
##1

set1 <- read.table("train/X_train.txt")
set2 <- read.table("test/X_test.txt")
X <- rbind(set1, set2)

set1 <- read.table("train/subject_train.txt")
set2 <- read.table("test/subject_test.txt")
Y <- rbind(set1, set2)

set1 <- read.table("train/y_train.txt")
set2 <- read.table("test/y_test.txt")
Z <- rbind(set1, set2)

print ("Subject 1")

##2
features <- read.table("features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices]
names(X) <- features[indices, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

print ("Subject 2")

##3
activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Z[,1] = activities[Y[,1], 2]
names(Z) <- "activity"

print ("Subject 3")

##4
names(Y) <- "subject"
combined <- cbind(Y, Z, X)
write.table(combined, "clean_data.txt")

print ("Subject 4")

##5
uniqueSubjects = unique(Y)[,1]
numSubjects = length(unique(Y)[,1])
numActivities = length(activities[,1])
numCols = dim(combined)[2]
result = combined[1:(numSubjects*numActivities), ]

records = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[records, 1] = uniqueSubjects[s]
    result[records, 2] = activities[a, 2]
    tmp <- combined[combined$subject==s & combined$activity==activities[a, 2], ]
    result[records, 3:numCols] <- colMeans(tmp[, 3:numCols])
    records = records+1
  }
}
write.table(result, "average_last.txt")
print ("Subject 5")

print ("All Done")