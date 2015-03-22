#step 1. read data
setwd("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/")
test<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/test/X_test.txt",header=FALSE)
train<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/train/X_train.txt",header=FALSE)

#step 2. feature, active and subject index
testLb<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/test/Y_test.txt",header=FALSE)
colnames(testLb)<-"activity"
trainLb<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/train/Y_train.txt",header=FALSE)
colnames(trainLb)<-"activity"

activity<-read.table("activity_labels.txt")
feature<-read.table("features.txt")
flb<-as.character(t(feature$V2))

trainSub<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/train/subject_train.txt",header=FALSE)
colnames(trainSub)<-"subject"
testSub<-read.table("C:/Users/xin/Dropbox/Courses/Getting and Cleaning Data/test/subject_test.txt",header=FALSE)
colnames(testSub)<-"subject"

#step 3. apply feaure as column names
colnames(test)<-flb
colnames(train)<-flb
head(train)

#step 4. merge subject,activity index
aTe<-cbind(testSub,testLb,test)
aTr<-cbind(trainSub,trainLb,train)

#step 5. merge train and test data
mydata0<-rbind(aTe,aTr)

#step 6. make a list of variable to keep
varlst<-colnames(mydata0)
a1<-sapply(varlst,function(x) grep("Freq",x,value=FALSE,invert=TRUE))
varlst2<-names(unlist(a1))
a1<-sapply(varlst2,function(x) grep("Jerk",x,value=FALSE,invert=TRUE))
varlst3<-names(unlist(a1))
a1<-sapply(varlst3,function(x) grep("fBody",x,value=FALSE,invert=TRUE))
varlst4<-names(unlist(a1))
a1<-sapply(varlst4,function(x) grep("Mag",x,value=FALSE,invert=TRUE))
varlst5<-names(unlist(a1))

keep1<-grep('mean()', varlst5, value=TRUE)
keep2<-grep('std()', varlst5, value=TRUE)
keep<-c("subject","activity",keep1,keep2)

#step 7. subset data for relavent variables
mydata1<-mydata0[,which(varlst %in% keep)]

#step 8. aggregate mean by subject and activity
d1<-aggregate(mydata1,by=list(mydata1$subject,mydata1$activity),mean)

#step 9. add descriptive activity to data table 
library(sqldf)
dt1<-sqldf("select a1.V2 as activityLabel,a.* from d1 a join activity a1 on a.activity=a1.V1")
mydata3<-subset(dt1,select=-c(activity,Group.1,Group.2))
colnames(mydata3)[1]<-"activity"
#Step10.make a tidy data,subject,type of statistics, activity, and 3 measures
library(tidyr)
m4<-gather(mydata3, key, value, -c(subject,activity))
m5<-separate(m4,col=key,into=c("measure","stats","axial"))

d1<-sqldf("select subject,activity,value as tBodyAcc, axial,stats from m5 where measure='tBodyAcc'")
d2<-sqldf("select subject,activity,value as tGravityAcc, axial,stats from m5 where measure='tGravityAcc'")
d3<-sqldf("select subject,activity,value as tBodyGyro, axial,stats from m5 where measure='tBodyGyro'")

mydata<-sqldf("select d1.subject, d1.activity,d1.axial,d1.stats,d1.tBodyAcc,d2.tGravityAcc,d3.tBodyGyro 
              from d1 join d2 on 
              d1.subject=d2.subject and d1.activity=d2.activity and d1.axial=d2.axial and d1.stats=d2.stats
              join d3 on 
              d1.subject=d3.subject and d1.activity=d3.activity and d1.axial=d3.axial and d1.stats=d3.stats
              ")

#step 10. export to txt format 
write.table(mydata, "mydata.txt", sep="\t",row.name=FALSE)
