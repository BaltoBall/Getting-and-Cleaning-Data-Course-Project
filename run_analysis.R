## 0 install necessary package dplyr
install.packages("dplyr")
library (dplyr)

##1. Merges the training and the test sets to create one data set.
##1.1 Reading files
X_train<-read.table("UCI HAR Dataset/train/X_train.txt")
X_test<-read.table("UCI HAR Dataset/test/X_test.txt")
Y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
Y_test<-read.table("UCI HAR Dataset/test/y_test.txt")
features <- read.table("UCI HAR Dataset/features.txt", dec=",", quote="")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
## 1.2 Merge data
train<-cbind(X_train,subject_train, Y_train)
test<-cbind(X_test,subject_test,Y_test)
DataSet<-rbind(train,test)
rm(train,test)
## 1.3 labels the data set with descriptive variable names
names(DataSet)<-features[,2]
names(DataSet)[562:563]<-c("Subjects","Activities")

##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##2.1 Checking column to extract
log1<-grepl("mean", features[,2], ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
log2<-grepl("std", features[,2], ignore.case = TRUE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
log3<- log1 + log2
log3df<-as.data.frame(log3)
filter(log3df, log3=="1")
## 2.2 column to extract
needcol<-filter(log3df, log3=="1")
##2.3 a smaller dataset with only mean and standard deviation
DataSet_mean_std<-DataSet[,c(needcol[,1],562,563)]
rm(log1, log2,log3,log3df,needcol)
##3. Uses descriptive activity names to name the activities in the data set
DataSet_mean_std$Activities<-factor(DataSet_mean_std$Activities)
levels(DataSet_mean_std$Activities) = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
##4. Appropriately labels the data set with descriptive variable names. 

## Completed in Step 1.3

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## 5.1 output for each subject by Activities is calculated  (output1, output2 ... output30)
for (i in 1:30){
    for ( j in 1:6){
        assign(paste("output", i, sep = ""),filter(DataSet_mean_std,Subjects ==i) %>% group_by(Activities) %>% summarise_each(funs(mean)))
    }
}
##5.2 final output my rbind all output by subjects
output <- rbind(output1,output2,output3, output4,output5, output6, output7, output8, output9, output10, output11, output12, output13, 
      output14, output15, output16, output17, output18, output19, output20, output21, output22, output23, output24, output25, output26,
      output27, output28, output29, output30)
rm(output1,output2,output3, output4,output5, output6, output7, output8, output9, output10, output11, output12, output13, output14, output15, output16, output17, output18, output19, output20, output21, output22, output23, output24, output25, output26, output27, output28, output29, output30)
##6. Output txt file
write.table(output,file ="output.txt", row.name=FALSE)
