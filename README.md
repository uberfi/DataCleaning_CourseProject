DataCleaning_CourseProject
==========================

### Cos Fi 
### DataCleaning_Course project
#### May 25, 2014
------------------------------------------------------------------------------
1. The code run_analysis.R accomplishes the following:

2. Merge the training and the test sets to create one data set.

3.	Use descriptive activity names to name the activities in the data set

The main renaming protocol was to change the hyphens into dots to make 
the names more R compliant. The names are very descriptive within the context in which
the data was obtained. The naming protocol preserves the useful time, frequency, and axes.

For a detailed description of the dataset and data definition, please see CodeBook.md


4.	Appropriately labels the data set with descriptive activity names. 

The gsub() function was used to accomplish the renaming.


5.	Creates a second, independent tidy data set with the average of each variable 
     for each activity and each subject. 

     
In the full dataset HAR, we use the reshape2 library to transform the 
dataset, then we compute means for each of the predictor variables for each subject and 
activity using the dcast() function.

Since I was not sure what was intended for this task, 
I also ran a similar reshaping on the smaller dataset of means and standard deviations.

I have submitted the tidy dataset for the smaller susetted dataset of means and standard deviations.


######################################################
Reference for creating tables in markdown files

http://www.tablesgenerator.com/markdown_tables

The following site has an analysis of the dataset
I was able to re-use some of the code and modify as needed for the specifics of the cpurse project

http://rstudio-pubs-static.s3.amazonaws.com/10696_c676703d98c84553b9e3510b095153b9.html

######################################################
## Merging the dataset into one data frame.
### Load the train and test datasets
### Tag the train and test observations with the labels, 
### lest you lose the train/test subsetting needed for data modeling
### Distingush the predictor variables from the test subjects and subset membership
### Now merge the train and test datasets into a complete HAR dataset
######################################################

```
setwd("~/Desktop/MOOCs/Coursera/DataScience/Getting_Cleaning_Data/CourseProject")
    temp <- read.table("activity_labels.txt", sep = "")
    activityLabels <- as.character(temp$V2)
    temp <- read.table("features.txt", sep = "")
    attributeNames <- temp$V2

    Xtrain <- read.table("train/X_train.txt", sep = "")
    names(Xtrain) <- attributeNames
    Ytrain <- read.table("train/y_train.txt", sep = "")
    names(Ytrain) <- "Activity"
    Ytrain$Activity <- as.factor(Ytrain$Activity)
    levels(Ytrain$Activity) <- activityLabels
    trainSubjects <- read.table("train/subject_train.txt", sep = "")
    names(trainSubjects) <- "subject"
    trainSubjects$subject <- as.factor(trainSubjects$subject)
    train <- cbind(Xtrain, trainSubjects, Ytrain)

    Xtest <- read.table("test/X_test.txt", sep = "")
    names(Xtest) <- attributeNames
    Ytest <- read.table("test/y_test.txt", sep = "")
    names(Ytest) <- "Activity"
    Ytest$Activity <- as.factor(Ytest$Activity)
    levels(Ytest$Activity) <- activityLabels
    testSubjects <- read.table("test/subject_test.txt", sep = "")
    names(testSubjects) <- "subject"
    testSubjects$subject <- as.factor(testSubjects$subject)
    test <- cbind(Xtest, testSubjects, Ytest)
    
    datafile <- "data.RData"
    
    save(train, test, file = datafile)
    rm(train, test, temp, Ytrain, Ytest, Xtrain, Xtest, trainSubjects, testSubjects, 
        activityLabels, attributeNames)
        
load("data.RData")

numPredictors <- ncol(train) - 2 # The idea here is to distinguish the predictors from the subject and train/test labels

```
Now merge the train and test datasets to form one dataset.
But first, we need to tag the cases as either train or test.
We would use the rbind() function to execute the merger

```
train$Partition <- "Train"
test$Partition <- "Test"

HAR <- rbind(train, test)

HAR$Partition = as.factor(HAR$Partition)
```
The prior sequence of codes have merged the data into one file.
In what follows, I will extract the means and standard deviations
for each measurement. There are 66 such variables:
33 means and 33 standard deviations.

## Extract Mean() and std() for each measurement
### There are 66 such variables
### The renaming was undertaken with gsub()
### The main reanmeing protocol was to change the hyphens to dots inorder
### to make the names more R compliant.
### The variables are well-desriptive within the HAR context
### I included the labels as well: "subject", "Activities", and "Partition"

```
meanStd.vars <- c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543,562:564)

length(meanStd.vars)

HAR.MeanStd <- HAR[, meanStd.vars]

names(HAR.MeanStd) <- gsub("-", ".", names(HAR.MeanStd)) # Here the only modification is to make the names more R compliant by changing hyphens to dots

str(HAR.MeanStd)
summary(HAR.MeanStd)
```

The following code snippet computes the means of each of the predictor variable in the full dataset for each of the 30 volenteers in the study. I was not sure whether I was to complete this
task on the full dataset or the subsetted dataset of means and standard deviations. So I did both.

```
library(reshape2)

id <- names(HAR[, -(1:numPredictors)]) # Non-Predictors as ids
id
measures <- names(HAR[,1:numPredictors])
measures


HAR.melt <- melt(HAR, id = id)
head(HAR.melt)

HAR.Mean.Subj <- dcast(HAR.melt, subject ~ variable, mean, na.rm =TRUE)
HAR.Mean.Subj

HAR.Mean.Act <- dcast(HAR.melt, Activity ~ variable, mean, na.rm=TRUE)
HAR.Mean.Act

HAR.Means.ActSubj <- dcast(HAR.melt, Activity+subject ~ variable, mean, na.rm=TRUE)
head(HAR.Means.ActSubj[, 1:5])

HAR.Means.SubjAct <- dcast(HAR.melt, subject+Activity ~ variable, mean, na.rm=TRUE)
head(HAR.Means.SubjAct[,1:5])

```
Here we create another dataset on just the means and standard deviations.
I have included this tidy dataset in the submission.

```
id2 <- names(HAR.MeanStd[, 67:69]) # Non-Predictors as ids
id2

measures2 <- names(HAR.MeanStd[,1:66])
measures2


HAR.melt2 <- melt(HAR.MeanStd, id = id2)
head(HAR.melt2)

HAR.Mean.Subj2 <- dcast(HAR.melt2, subject ~ variable, mean, na.rm =TRUE)
HAR.Mean.Subj2

HAR.Mean.Act2 <- dcast(HAR.melt2, Activity ~ variable, mean, na.rm=TRUE)
HAR.Mean.Act2

HAR.Means.ActSubj2 <- dcast(HAR.melt2, Activity+subject ~ variable, mean, na.rm=TRUE)
head(HAR.Means.ActSubj2[, 1:5])

HAR.Means.SubjAct2 <- dcast(HAR.melt2, subject+Activity ~ variable, mean, na.rm=TRUE)
head(HAR.Means.SubjAct2[,1:5])

write.csv(HAR.Means.SubjAct2, file = "HAR.MeansStd.TidyData.csv")

```

