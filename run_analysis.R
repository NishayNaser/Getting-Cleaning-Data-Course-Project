setwd("/Users/nishay/Desktop/UCI HAR Dataset")                                  # setting the working directory

features = read.table('./features.txt',header=FALSE);                           # importing data
activityType = read.table('./activity_labels.txt',header=FALSE);
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
xTrain = read.table('./train/x_train.txt',header=FALSE);
yTrain = read.table('./train/y_train.txt',header=FALSE);

colnames(activityType) = c('activityId','activityType');                        # assigning column names
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2]; 
colnames(yTrain) = "activityId";

trainingData = cbind(yTrain,subjectTrain,xTrain);                               # merging data

subjectTest = read.table('./test/subject_test.txt',header=FALSE);               # importing data
xTest       = read.table('./test/x_test.txt',header=FALSE);
yTest       = read.table('./test/y_test.txt',header=FALSE);

colnames(subjectTest) = "subjectId";                                            # assigning column names
colnames(xTest) = features[,2]; 
colnames(yTest) = "activityId";

testData = cbind(yTest,subjectTest,xTest);                                      # merging data
finalData = rbind(trainingData,testData);                                       # merging all data
colNames  = colnames(finalData);
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];                                     # selecting required columns
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);           # including descriptive activity names 
colNames  = colnames(finalData);                                                # adding new column names

for (i in 1:length(colNames))                                                   # cleaning variable names
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames;                                                 # reassigning column names
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];      # new table
tidyData = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);                # summarizing data
tidyData = merge(tidyData,activityType,by='activityId',all.x=TRUE);             # merging data
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');                # exporting data