#set to working directory
#Access the file location, download, and unzip the files
fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "UCIHARdataset.zip",method="auto")
unzip("UCIHARdataset.zip")
# read in the files
xtrain = read.table("X_train.txt")
xtest = read.table("X_test.txt")
ytrain = read.table("y_train.txt")
ytest = read.table("y_test.txt")
subtrain = read.table("subject_train.txt")
subtest = read.table("subject_test.txt")
#activity = read.table("activity_labels.txt")
#features = read.table("features.txt")
# create the training data set
Trainset = cbind(subtrain,ytrain,xtrain)
#Create the testing data set
Testset = cbind(subtest,ytest,xtest) 
#Set = merge(Trainset,Testset, all.x=TRUE,all.y=TRUE)
#Merge the training and testing data set
Set = rbind(Trainset,Testset)
#Initialize empty data frame for storing the tidy dataset
tidydf = data.frame()
# Extract only the mean and standard deviation measurements
Set1 = Set[,c(1,2,3:8,43:48,83:88,123:128,163:168)]
Set2 = Set[,c(203:204,216:217,229:230,255,256,268:273,296:298,347:352,375:377,426:431,454:456,505,506,515,518,519,531,532,541,544,545,554)]
Set3 = cbind(Set1,Set2)
#Label the activities appropriately based on the activity_label.txt file
for (i in 1:nrow(Set3)){
    if (Set3[i,2]==1){
        Set3[i,2]= "WALKING"
    }
    if (Set3[i,2]==2){
            Set3[i,2]="WALKING_UPSTAIRS"
        }
    if (Set3[i,2]==3){
        Set3[i,2]= "WALKING_DOWNSTAIRS"
    }    
    if (Set3[i,2]==4){
        Set3[i,2]= "SITTING"
    }
    if (Set3[i,2]==5){
        Set3[i,2]="STANDING"
    }
    if (Set3[i,2]== 6){
        Set3[i,2]="LAYING"
    }
    
}
# Descriptive names to the variable columns based on the features_info.txt
colnames(Set3) = c("Subject", "Activity","BodyAccelerationXmean","BodyAccelerationYmean","BodyAccelerationZmean","BodyAccelerationXStddev","BodyAccelerationYStddev",
                   "BodyAccelerationZStddev","GravityAccelerationXmean","GravityAccelerationYmean","GravityAccelerationZmean",
                   "GravityAccelerationXStddev","GravityAccelerationYStddev","GravityAccelerationZStddev",
                   "BodyAccelerationJerkXmean","BodyAccelerationJerkYmean","BodyAccelerationJerkZmean","BodyAccelerationJerkXStddev","BodyAccelerationJerkYStddev","BodyAccelerationJerkZStddev",
                   "BodyGyroXmean","BodyGyroYmean","BodyGyroZmean","BodyGyroXStddev","BodyGyroYStddev","BodyGyroZStddev",
                   "BodyGyroJerkXmean","BodyGyroJerkYmean","BodyGyroJerkZmean","BodyGyroJerkXStddev","BodyGyroJerkYStddev","BodyGyroJerkZStddev",
                   "BodyAccelerationMagnitudemean","BodyAccelerationMagnitudeStddev",
                   "GravityAccelerationMagnitudemean","GravityAccelerationMagnitudeStddev",
                   "BodyAccelerationJerkMagnitudemean","BodyAccelerationJerkMagnitudeStddev",
                   "BodyGyroJerkMagnitudemean","BodyGyroJerkMagnitudeStddev","FFTBodyAccelerationXmean","FFTBodyAccelerationYmean","FFTBodyAccelerationZmean",
                   "FFTBodyAccelerationXStddev","FFTBodyAccelerationYStddev","FFTBodyAccelerationZStddev","FFTBodyAccelerationXmeanfreq","FFTBodyAccelerationYmeanfreq",
                   "FFTBodyAccelerationZmeanfreq","FFTBodyAccelerationJerkXmean","FFTBodyAccelerationJerkYmean","FFTBodyAccelerationJerkZmean",
                   "FFTBodyAccelerationJerkXStddev","FFTBodyAccelerationJerkYStddev","FFTBodyAccelerationJerkZStddev","FFTBodyAccelerationJerkXmeanfreq","FFTBodyAccelerationJerkYmeanfreq","FFTBodyAccelerationJerkZmeanfreq",
                   "FFTBodyGyroXmean","FFTBodyGyroYmean","FFTBodyGyroZmean","FFTBodyGyroXStddev",
                   "FFTBodyGyroYStddev","FFTBodyGyroZStddev","FFTBodyGyroXmeanfreq","FFTBodyGyroYmeanfreq","FFTBodyGyroZmeanfreq",
                   "FFTBodyAccelerationMagmean","FFTBodyAccelerationMagStddev",
                   "FFTBodyAccelerationMagmeanfreq","FFTBodyBodyAccelerationJerkMagnitudemean",
                   "FFTBodyBodyAccelerationJerkMagnitudeStddev","FFTBodyGyroMagmean", "FFTBodyGyroMagStddev","FFTBodyGyroMagmeanfreq",
                   "FFTBodyGyroJerkMagmean","FFTBodyGyroJerkMagStddev","FFTBodyBodyGyroJerkMagmeanfreq")
# Create tidy data set with average of the extracted measurements
library(plyr)
tidydf=ddply(Set3,.(Subject,Activity),numcolwise(mean))
colnames(tidydf) = c("Subject", "Activity","AvgBodyAccelerationXmean","AvgBodyAccelerationYmean","AvgBodyAccelerationZmean","AvgBodyAccelerationXStddev","AvgBodyAccelerationYStddev",
                   "AvgBodyAccelerationZStddev","AvgGravityAccelerationXmean","AvgGravityAccelerationYmean","AvgGravityAccelerationZmean",
                   "AvgGravityAccelerationXStddev","AvgGravityAccelerationYStddev","AvgGravityAccelerationZStddev",
                   "AvgBodyAccelerationJerkXmean","AvgBodyAccelerationJerkYmean","AvgBodyAccelerationJerkZmean","AvgBodyAccelerationJerkXStddev","AvgBodyAccelerationJerkYStddev","AvgBodyAccelerationJerkZStddev",
                   "AvgBodyGyroXmean","AVgBodyGyroYmean","AvgBodyGyroZmean","AvgBodyGyroXStddev","AvgBodyGyroYStddev","AvgBodyGyroZStddev",
                   "AvgBodyGyroJerkXmean","AvgBodyGyroJerkYmean","AvgBodyGyroJerkZmean","AvgBodyGyroJerkXStddev","AvgBodyGyroJerkYStddev","AvgBodyGyroJerkZStddev",
                   "AvgBodyAccelerationMagnitudemean","AvgBodyAccelerationMagnitudeStddev",
                   "AvgGravityAccelerationMagnitudemean","AvgGravityAccelerationMagnitudeStddev",
                   "AvgBodyAccelerationJerkMagnitudemean","AvgBodyAccelerationJerkMagnitudeStddev",
                   "AvgBodyGyroJerkMagnitudemean","AvgBodyGyroJerkMagnitudeStddev","AvgFFTBodyAccelerationXmean","AvgFFTBodyAccelerationYmean","AvgFFTBodyAccelerationZmean",
                   "AvgFFTBodyAccelerationXStddev","AvgFFTBodyAccelerationYStddev","AvgFFTBodyAccelerationZStddev","AvgFFTBodyAccelerationXmeanfreq","AvgFFTBodyAccelerationYmeanfreq",
                   "AvgFFTBodyAccelerationZmeanfreq","AvgFFTBodyAccelerationJerkXmean","AvgFFTBodyAccelerationJerkYmean","AvgFFTBodyAccelerationJerkZmean",
                   "AvgFFTBodyAccelerationJerkXStddev","AvgFFTBodyAccelerationJerkYStddev","AvgFFTBodyAccelerationJerkZStddev","AvgFFTBodyAccelerationJerkXmeanfreq","AvgFFTBodyAccelerationJerkYmeanfreq","AvgFFTBodyAccelerationJerkZmeanfreq",
                   "AvgFFTBodyGyroXmean","AvgFFTBodyGyroYmean","AvgFFTBodyGyroZmean","AvgFFTBodyGyroXStddev",
                   "AVgFFTBodyGyroYStddev","AvgFFTBodyGyroZStddev","AvgFFTBodyGyroXmeanfreq","AvgFFTBodyGyroYmeanfreq","AvgFFTBodyGyroZmeanfreq",
                   "AvgFFTBodyAccelerationMagmean","AvgFFTBodyAccelerationMagStddev",
                   "AvgFFTBodyAccelerationMagmeanfreq","AvgFFTBodyBodyAccelerationJerkMagnitudemean",
                   "AvgFFTBodyBodyAccelerationJerkMagnitudeStddev","AvgFFTBodyGyroMagmean", "AvgFFTBodyGyroMagStddev","AvgFFTBodyGyroMagmeanfreq",
                   "AvgFFTBodyGyroJerkMagmean","AvgFFTBodyGyroJerkMagStddev","AvgFFTBodyBodyGyroJerkMagmeanfreq")
write.table(tidydf, file="tidydataset.txt",sep="\t",row.names=FALSE)
