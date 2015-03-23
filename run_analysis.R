## Script run_analysis.R

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Set up Environment and Libraries required.

library(plyr)
library(dplyr)
library(tidyr)


## Aquire Reference Data Section
## =============================
##
## Read and format the Activity reference file.

colNames.Activity <- c("Activity.Id", "Activity.Name")
refActivity <- read.csv("Activity_Labels.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE, col.names = colNames.Activity)

## Read and format the Feature reference file.

colNames.Feature <- c("Feature.Id", "Feature.Name")
refFeature <- read.csv("Features.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE, col.names = colNames.Feature)

## Get a List of Features for column headings

colNames.FeatureName <- refFeature$Feature.Name


## Aquire Test Data Section
## ========================
##
## Get the Subject associated with each Test event.

colNames.Subject <- c("Subject.Id")
datTest.Subject <- read.csv("./test/Subject_Test.txt", header = FALSE, col.names = colNames.Subject)

## Get the Activity Id associated with each Testing event.

colNames.ActivityData <- c("Activity.Id")
datTest.Activity <- read.csv("./test/y_Test.txt", header = FALSE, col.names = colNames.ActivityData)

## Get the Testing Events

datTest.Event <- read.csv("./test/X_Test.txt", header = FALSE,  stringsAsFactors = FALSE,  sep = "", col.names = colNames.FeatureName)


## Aquire Training Data Section
## ============================
##
##  Get the Subjects associated with each Training event

datTrain.Subject <- read.csv("./train/Subject_Train.txt", header = FALSE, col.names = colNames.Subject)

## Get the Activity Id associated with each Training event.

datTrain.Activity <- read.csv("./train/y_Train.txt", header = FALSE, col.names = colNames.ActivityData)

## Get the Training Events

datTrain.Event <- read.csv("./train/X_Train.txt", header = FALSE,  stringsAsFactors = FALSE,  sep = "", col.names = colNames.FeatureName)


## Assemble Testing Data Table Section
## ===================================
##
## Apply the Activity Names to the Activity Id associated with each Testing Event

datTest.Activity <- join(datTest.Activity, refActivity, by="Activity.Id")

## Combine the Activity, Subject and Events into a single Testing data table

datTest.Combine <- cbind(datTest.Activity, datTest.Subject, datTest.Event)


## Assemble Training Data Table Section
## ====================================
##
## Apply the Activity Names to the Activity Id associated with each Training Event

datTrain.Activity <- join(datTrain.Activity, refActivity, by="Activity.Id")

## Combine the Activity, Subject and Events into a single Training data table

datTrain.Combine <- cbind(datTrain.Activity, datTrain.Subject, datTrain.Event)


## Assemble Combined Training and Testing Data Table Section
## =========================================================
##
## Combine the Training and Testing data tables

datCombine <- rbind(datTrain.Combine, datTest.Combine)


## Create and Output the reporting data subset Section
## ===================================================
##
## Create a subset of data that contains those Features that refer to Mean or Standard Deviation measurements

datCombine.Subset <- select(datTrain.Combine,2,3, contains(".mean"), contains(".std") )

## Calculate the average of the measurements, grouped by Activity Name and Subject Id

datSummary.Wide <- datCombine.Subset %>% group_by(Activity.Name, Subject.Id) %>% summarise_each(funs(mean))

## Turn the measurement columns into a single set of "Feature" and "Value" pair columns

datSummary <- gather(datSummary.Wide, Feature, Avg_Value,  -1,-2 )

## Sort the summarised dataset

datSummary <- datSummary[order(datSummary$Activity.Name, datSummary$Subject.Id, datSummary$Feature),]

## Output the Summarised Tidy dataset

write.table(datSummary, "datSummaryTidy.txt", row.name = FALSE, sep = "\t")

## End of Script
## =============
