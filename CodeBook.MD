# Code Book

## Introduction

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the analysis is located at: 

	https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Data Collection

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

The data provided in each record is:

* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

## Raw Data Files

Raw data are obtained from UCI Machine Learning repository: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.

The data collection includes the following files:

### Reference Data

* 'features_info.txt' : Shows information about the variables used on the feature vector.

* 'features.txt': List of all features.

* 'activity_labels.txt': Links the class labels with their activity name.

### Training Data 

* 'train/X_train.txt': Training set.

* 'train/y_train.txt': Training labels.

* 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

### Testing Data

* 'test/X_test.txt': Test set.

* 'test/y_test.txt': Test labels.

* 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 


## Raw Data Transformation

The "run_analysis.R" script processes the raw data files to create a summarised subset of the data, which is saved in "tidy format" to a text dataset.

### Outline of the Process

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Aquire Reference Data Section

Read the Activity reference file ("Activity_Labels.txt") into varaiable "refActivity".
Read the Feature reference file("Features.txt") into variable "refFeature".
Make a list of Feature Names for column headings ("colNames.FeatureName")

### Aquire Test Data Section

Get the Subject associated with each Test event ("./test/Subject_Test.txt"). Save as variable "datTest.Subject".
Get the Activity Id associated with each Testing event ("./test/y_Test.txt"). Save as variable "datTest.Activity".
Get the Testing Events ("./test/X_Test.txt"). Save as data frame "datTest.Event:.

### Aquire Training Data Section

Get the Subjects associated with each Training event ("./train/Subject_Train.txt"). Save as variable "datTrain.Subject".
Get the Activity Id associated with each Training event ("./train/y_Train.txt"). Save as variable "datTrain.Activity".
Get the Training Events ("./train/X_Train.txt"). Save as data frame "datTrain.Event".

### Assemble Testing Data Table Section

Apply the Activity Names to the Activity Id associated with each Testing Event ("datTest.Activity")
Combine the Activity, Subject and Events into a single Testing data table ("datTest.Combine")

### Assemble Training Data Table Section

Apply the Activity Names to the Activity Id associated with each Training Event ("datTrain.Activity")
Combine the Activity, Subject and Events into a single Training data table ("datTrain.Combine")

## Assemble Combined Training and Testing Data Table Section

Combine the Training and Testing data tables ("datCombine")

## Create and Output the reporting data subset Section

Create a subset of data that contains those Features that refer to Mean or Standard Deviation measurements ("datCombine.Subset")
Calculate the average of the measurements, grouped by Activity Name and Subject Id ("datSummary.Wide")
Turn the measurement columns into a single set of "Feature" and "Value" pair columns ("datSummary")
Sort the summarised dataset by Activity Name, Subject Id and Feature ("datSummary")
Output the Summarised Tidy dataset (""datSummaryTidy.txt")


##Output from the Analysis 

The run_analysis script produces a text file of the summarised raw data. The file is named "datSummaryTidy.txt" as is stored in the root directory. The output file is in "tidy' format. It has three dimension attributes and a numeric measurement variable.

The 'tidy' dataset has the following variables:

* Activity.Name
* Subject.Id
* Feature
* Avg_Value

### Activity Name
A label describing the activity undertaken whilst the event was conducted.
The Activities classified are:
* WALKING
* WALKING_UPSTAIRS
* WALKING_DOWNSTAIRS
* SITTING
* STANDING
* LAYING

### Subject.Id

A numeric identifier of the person (Subject) on whom the experiment was conducted. It takes on the values of 1 to 30.

### Feature

A label that identifies the type of measurement taken for the event and collected from the accelerometer and gyroscope 3-axial raw signal. Refer to the documentation reference in the Input Data section of this document.

### Avg_Value

The average value (mean) of the event measurements described by the "Feature" dimension and grouped by the "Activity.Name" and "Subject.Id".
Essentially it is the average of selected mean and standard deviation measurements.

