# Getting and Cleaning Data Course Project

The files in this folder provide a solution for the Coursera "Getting And Cleaning Data" course project. The course project works with the "Human Activity Recognition Using Smartphones Dataset" from the UCI Machine Learning Repository. A description of the dataset can be found at [http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The dataset can be downloaded at [https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).

## The Transformation Script run_analysis.R

The file run_analysis.R contains an R script for transforming the original dataset into a second dataset according to the following rules:

You should create one R script called run_analysis.R that does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Running The Script

The script requires the package dplyr.

The script assumes that the extracted dataset can be found in the current working directory, i.e. the current working directory contains the folder ```UCI HAR Dataset```. 

To run the script ```run_analysis.R```:

1. put it into the parent folder of ```UCI HAR Dataset```
2. launch an R session and change into that directory using the ```setwd``` command
3. type ```source("run_analysis.R")``` to execute the script

The script will read the test and training data from the folder ```UCI HAR Dataset```, apply the transformations listed above and write the result to the file "tidy_data.txt" in the current working directory.

The file "tidy_data.txt" can be read into a data.frame using

```R
read.table("tidy_data.txt", header = T)
```

### Explanation Of The Script

#### Merging Of Test And Training Set

Requirement 1 states: "Merges the training and the test sets to create one data set." To achive this, the run_analysis.R script first loads the three input files (subject data, label data and feature data) for each of the two datasets and merges them into one data.frame. Finally the two resulting data.frames are concatenated to one big data.frame. The subjectid and activityname form the first to column, the features are contained in the remaining columns.

#### Mean And Standard Deviation

Requirement 2 states: "Extracts only the measurements on the mean and standard deviation for each measurement." The input data contains several candidates for mean and standard deviation measurements:

* features created by the mean() function (e.g. *fBodyBodyGyroMag-mean()*)
* features created by the std() function (e.g. *fBodyBodyGyroMag-std()*)
* features created by the meanFreq() function (e.g. *fBodyBodyGyroJerkMag-meanFreq()*)
* features created by calling the angle function on variables that have the string *mean* in their name (e.g. *angle(tBodyAccJerkMean),gravityMean)*)

The requirement is unclear about which features should be selected. I decided to select only features created using the mean() and std() functions since they definitely contain mean or standard deviation measurements. This selects 66 features, the resulting data.frame with subjectid and activityname has 68 columns.


#### Activity Labels

Requirement 3 states: "Uses descriptive activity names to name the activities in the data set." To achive this, the script loads activity names from the file "activity_labels.txt" and transforms the column *activityname* to a factor with appropiate labels.

#### Variable Names

Requirement 4 states: "Appropriately labels the data set with descriptive variable names." I used the following rules to create descriptive variable names:

* Expand abbreviations
* Remove hyphens
* Since the expanded variable names are very long I decided to use CamelCase variable names instead of all lower case names for better readability (as suggested in lecture "Components Of Tidy Data": "Make variable names human readable AgeAtDiagnosis instead of AgeDx" or in [https://github.com/jtleek/datasharing/README.md](https://github.com/jtleek/datasharing/README.md)).


#### Tidy data

Requirement 5 states: "From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject."

The transformation script outputs data to a tidy data text file that meets the principles of tidy data as stated by "Hadley Wickham":

* Each variable forms a column.
* Each observation forms a row.
* Each type of observational unit forms a table

In the course forums there has been a lot of discussion about the correct tidy data format (e.g. on https://class.coursera.org/getdata-008/forum/thread?thread_id=94). One possibility is to list one mean value per subject id and activity in one row. The second option is to put all mean values for one subject id / activity combination into one row. 
I decided on the second option since in my opinon all mean values for one subject id, acitivity pair belong to one observation.

## The codebook. 

The codebook is contained in the file CodeBook.md. It describes the variables of the output data set and summaries used to calculate the values, along with units and any other relevant information.



