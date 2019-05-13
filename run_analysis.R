library(dplyr)
ruta <-  "C:/Users/Adriana/Documents/Adriana/Curso Data Science/Curso 3/Semana 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
setwd("~/Adriana/Curso Data Science/Curso 3/Semana 4/getdata_projectfiles_UCI HAR Dataset")

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

x.all <- rbind(x_test,x_train)
y.all <- rbind(y_test,y_train)
sub.all <- rbind(subject_test,subject_train)

datos <- cbind(x.all,y.all,sub.all)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement
filtro <-features$functions[grep("mean\\(\\)|std\\(\\)", features$functions)]
variables <- c(as.character(filtro),"code")
datos.aux<-subset(datos,select=(filtro))
datos <- data.frame(datos.aux,code=datos$code, subject=datos$subject)

# 3.Uses descriptive activity names to name the activities in the data set.
datos$code <- activities[datos$code, 2]

# 4.Appropriately labels the data set with descriptive variable names.

names(datos)[2] = "activity"
names(datos)<-gsub("Acc", "Accelerometer", names(datos))
names(datos)<-gsub("Gyro", "Gyroscope", names(datos))
names(datos)<-gsub("BodyBody", "Body", names(datos))
names(datos)<-gsub("Mag", "Magnitude", names(datos))
names(datos)<-gsub("^t", "Time", names(datos))
names(datos)<-gsub("^f", "Frequency", names(datos))
names(datos)<-gsub("tBody", "TimeBody", names(datos))
names(datos)<-gsub("-mean()", "Mean", names(datos), ignore.case = TRUE)
names(datos)<-gsub("-std()", "STD", names(datos), ignore.case = TRUE)
names(datos)<-gsub("-freq()", "Frequency", names(datos), ignore.case = TRUE)
names(datos)<-gsub("angle", "Angle", names(datos))
names(datos)<-gsub("gravity", "Gravity", names(datos))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

datos2 <- datos %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

write.table(datos2, "Means.txt", row.name=FALSE)

