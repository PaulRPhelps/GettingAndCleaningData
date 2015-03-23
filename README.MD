# Getting and Cleaning Data

## Getting and Cleaning Data Course Project (Coursera) ReadMe file.

###Description

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

### Objective

Create a single R script called "run_analysis.R" that does the following:

* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement.
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names.
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Contents of this repository

* CodeBook.md: information about raw data collection, transformations and the output (tidy dataset).
* README.md: this file
* "run_analysis.R" : R script to transform the raw data collection into a summarised tidy dataset.

### Execution (How to Use)

* Clone this Repository.
* Refer to the CodeBook to identify the source of the raw data collection.
* Download compressed raw data collection.
* Unzip the raw data collection into your working directory.
* Open a R console and set the working directory to your working directory.
* Run the "run_analysis.R" script
* The output from the analysis is found in the "datSummaryTidy.txt" dataset in your working directory.
