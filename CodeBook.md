# Codebook

This document describes the variables of the output data set and summaries used to calculate the values, along with units and any other relevant information.

The [first section](#input-data) summarizes relevant parts from the input data codebook. The [next section](#output-variables) describes the output variables with their units. The [last section](#transformations) describes the transformations used to calculate the output values.

## Input Data

The original data set contains a codebook that describes the data set. In this section I repeat the parts of the codebook relevant to the generated data set.

The input data contains data recorded from the accelerometer and the gyroscope of a smartphone while the person who carried the smartphone was performing one of the following six activities: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING.

### Input Data Variables

From the raw measurements, several other values were derived. The input dataset contains data for the following variables:

|      Variable     |     source    |   domain  |
| ----------------- | ------------- | --------- |
| tBodyAcc-XYZ      | accelerometer | time      |
| tGravityAcc-XYZ   | accelerometer | time      |
| tBodyAccJerk-XYZ  | accelerometer | time      |
| tBodyAccMag       | accelerometer | time      |
| tGravityAccMag    | accelerometer | time      |
| tBodyAccJerkMag   | accelerometer | frequency |
| fBodyAcc-XYZ      | accelerometer | frequency |
| fBodyAccJerk-XYZ  | accelerometer | frequency |
| fBodyAccMag       | accelerometer | frequency |
| fBodyAccJerkMag   | accelerometer | frequency |
| tBodyGyro-XYZ     | gyroscope     | time      |
| tBodyGyroJerk-XYZ | gyroscope     | time      |
| tBodyGyroMag      | gyroscope     | time      |
| tBodyGyroJerkMag  | gyroscope     | time      |
| fBodyGyro-XYZ     | gyroscope     | frequency |
| fBodyGyroMag      | gyroscope     | frequency |
| fBodyGyroJerkMag  | gyroscope     | frequency |

The table above also shows the source for each variable and the domain it belongs to. Variable names ending with XYZ denote 3-axial variables. The dataset contains one separate variable for each axis.

The original sensor data was recorded at a rate of 50 Hz. The data was grouped into fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). Several functions where applied to these windows to compute the input features. The functions include: 

* mean(): Mean value
* std(): Standard deviation
* mad(): Median absolute deviation 
* and many others.

In total, each input record contains 651 features.

### Input Data Units

The accelerometer data is measured in standard gravity units 'g'. The gyroscope data is measured in radians/second.

The input features are normalized and bounded within the interval [-1,1]. During normalization the units have been divided by themselves so units have been cancelled.

### Structure of the input data

The input data is split into two subsets: a training set and a test set. Each subsets consists of three files:

* A features file (train/X_train.txt and test/X_test.txt) with one feature vector per row
* A label with one activity label per row (train/y_train.txt and test/y_test.txt)
* A file with a subject id per row (train/subject_train.txt and test/subject_test.txt)

The names of the features are listed in the file features.txt.

Additionally, the input contains the file activity_labels.txt which links the class labels with their activity name.

The input data set also contains the raw measurement data that was used to compute the features. This raw data is not beeing used in this project.


## Output Variables

The following table shows a table with all variables in the output file. The first column in the table shows the column number of the variable, the second column shows the variable name and column three shows the source variable used to compute the value of the variable.

| Colmun |                         Variable                        |        source variable        |
| ------ | :------------------------------------------------------ | :---------------------------- |
|      1 | subjectid                                               | subject id from subjects file |
|      2 | activityname                                            | activity id from labels file  |
|      3 | timeBodyAccelerationMeanX                               | tBodyAcc-mean()-X             |
|      4 | timeBodyAccelerationMeanY                               | tBodyAcc-mean()-Y             |
|      5 | timeBodyAccelerationMeanZ                               | tBodyAcc-mean()-Z             |
|      6 | timeBodyAccelerationStandarddeviationX                  | tBodyAcc-std()-X              |
|      7 | timeBodyAccelerationStandarddeviationY                  | tBodyAcc-std()-Y              |
|      8 | timeBodyAccelerationStandarddeviationZ                  | tBodyAcc-std()-Z              |
|      9 | timeGravityAccelerationMeanX                            | tGravityAcc-mean()-X          |
|     10 | timeGravityAccelerationMeanY                            | tGravityAcc-mean()-Y          |
|     11 | timeGravityAccelerationMeanZ                            | tGravityAcc-mean()-Z          |
|     12 | timeGravityAccelerationStandarddeviationX               | tGravityAcc-std()-X           |
|     13 | timeGravityAccelerationStandarddeviationY               | tGravityAcc-std()-Y           |
|     14 | timeGravityAccelerationStandarddeviationZ               | tGravityAcc-std()-Z           |
|     15 | timeBodyAccelerationJerkMeanX                           | tBodyAccJerk-mean()-X         |
|     16 | timeBodyAccelerationJerkMeanY                           | tBodyAccJerk-mean()-Y         |
|     17 | timeBodyAccelerationJerkMeanZ                           | tBodyAccJerk-mean()-Z         |
|     18 | timeBodyAccelerationJerkStandarddeviationX              | tBodyAccJerk-std()-X          |
|     19 | timeBodyAccelerationJerkStandarddeviationY              | tBodyAccJerk-std()-Y          |
|     20 | timeBodyAccelerationJerkStandarddeviationZ              | tBodyAccJerk-std()-Z          |
|     21 | timeBodyGyroscopeMeanX                                  | tBodyGyro-mean()-X            |
|     22 | timeBodyGyroscopeMeanY                                  | tBodyGyro-mean()-Y            |
|     23 | timeBodyGyroscopeMeanZ                                  | tBodyGyro-mean()-Z            |
|     24 | timeBodyGyroscopeStandarddeviationX                     | tBodyGyro-std()-X             |
|     25 | timeBodyGyroscopeStandarddeviationY                     | tBodyGyro-std()-Y             |
|     26 | timeBodyGyroscopeStandarddeviationZ                     | tBodyGyro-std()-Z             |
|     27 | timeBodyGyroscopeJerkMeanX                              | tBodyGyroJerk-mean()-X        |
|     28 | timeBodyGyroscopeJerkMeanY                              | tBodyGyroJerk-mean()-Y        |
|     29 | timeBodyGyroscopeJerkMeanZ                              | tBodyGyroJerk-mean()-Z        |
|     30 | timeBodyGyroscopeJerkStandarddeviationX                 | tBodyGyroJerk-std()-X         |
|     31 | timeBodyGyroscopeJerkStandarddeviationY                 | tBodyGyroJerk-std()-Y         |
|     32 | timeBodyGyroscopeJerkStandarddeviationZ                 | tBodyGyroJerk-std()-Z         |
|     33 | timeBodyAccelerationMagnitudeMean                       | tBodyAccMag-mean()            |
|     34 | timeBodyAccelerationMagnitudeStandarddeviation          | tBodyAccMag-std()             |
|     35 | timeGravityAccelerationMagnitudeMean                    | tGravityAccMag-mean()         |
|     36 | timeGravityAccelerationMagnitudeStandarddeviation       | tGravityAccMag-std()          |
|     37 | timeBodyAccelerationJerkMagnitudeMean                   | tBodyAccJerkMag-mean()        |
|     38 | timeBodyAccelerationJerkMagnitudeStandarddeviation      | tBodyAccJerkMag-std()         |
|     39 | timeBodyGyroscopeMagnitudeMean                          | tBodyGyroMag-mean()           |
|     40 | timeBodyGyroscopeMagnitudeStandarddeviation             | tBodyGyroMag-std()            |
|     41 | timeBodyGyroscopeJerkMagnitudeMean                      | tBodyGyroJerkMag-mean()       |
|     42 | timeBodyGyroscopeJerkMagnitudeStandarddeviation         | tBodyGyroJerkMag-std()        |
|     43 | frequencyBodyAccelerationMeanX                          | fBodyAcc-mean()-X             |
|     44 | frequencyBodyAccelerationMeanY                          | fBodyAcc-mean()-Y             |
|     45 | frequencyBodyAccelerationMeanZ                          | fBodyAcc-mean()-Z             |
|     46 | frequencyBodyAccelerationStandarddeviationX             | fBodyAcc-std()-X              |
|     47 | frequencyBodyAccelerationStandarddeviationY             | fBodyAcc-std()-Y              |
|     48 | frequencyBodyAccelerationStandarddeviationZ             | fBodyAcc-std()-Z              |
|     49 | frequencyBodyAccelerationJerkMeanX                      | fBodyAccJerk-mean()-X         |
|     50 | frequencyBodyAccelerationJerkMeanY                      | fBodyAccJerk-mean()-Y         |
|     51 | frequencyBodyAccelerationJerkMeanZ                      | fBodyAccJerk-mean()-Z         |
|     52 | frequencyBodyAccelerationJerkStandarddeviationX         | fBodyAccJerk-std()-X          |
|     53 | frequencyBodyAccelerationJerkStandarddeviationY         | fBodyAccJerk-std()-Y          |
|     54 | frequencyBodyAccelerationJerkStandarddeviationZ         | fBodyAccJerk-std()-Z          |
|     55 | frequencyBodyGyroscopeMeanX                             | fBodyGyro-mean()-X            |
|     56 | frequencyBodyGyroscopeMeanY                             | fBodyGyro-mean()-Y            |
|     57 | frequencyBodyGyroscopeMeanZ                             | fBodyGyro-mean()-Z            |
|     58 | frequencyBodyGyroscopeStandarddeviationX                | fBodyGyro-std()-X             |
|     59 | frequencyBodyGyroscopeStandarddeviationY                | fBodyGyro-std()-Y             |
|     60 | frequencyBodyGyroscopeStandarddeviationZ                | fBodyGyro-std()-Z             |
|     61 | frequencyBodyAccelerationMagnitudeMean                  | fBodyAccMag-mean()            |
|     62 | frequencyBodyAccelerationMagnitudeStandarddeviation     | fBodyAccMag-std()             |
|     63 | frequencyBodyAccelerationJerkMagnitudeMean              | fBodyBodyAccJerkMag-mean()    |
|     64 | frequencyBodyAccelerationJerkMagnitudeStandarddeviation | fBodyBodyAccJerkMag-std()     |
|     65 | frequencyBodyGyroscopeMagnitudeMean                     | fBodyBodyGyroMag-mean()       |
|     66 | frequencyBodyGyroscopeMagnitudeStandarddeviation        | fBodyBodyGyroMag-std()        |
|     67 | frequencyBodyGyroscopeJerkMagnitudeMean                 | fBodyBodyGyroJerkMag-mean()   |
|     68 | frequencyBodyGyroscopeJerkMagnitudeStandarddeviation    | fBodyBodyGyroJerkMag-std()    |

* Column 1 holds the unmodified subject id from the input data.
* Column 2 holds a readable activity name for the activity id in the lables file
* The remaining columns hold the mean of all values for the source variable for a given subject and activity.

The source variables consist of the following parts:

* The first letter defines the domain: t for time or f for frequency
* The next part defines the source variable: BodyAcc (BodyAcceleration), GravityAcc (GravityAcceleration) or BodyGyro (BodyGyroscope)
* The next (optional) part describes the method that was used to derive the variable: Mag (Magnitued), Jerk or JerkMag (JerkMagnitude)
* The part after the dash describes the function that was used to compute the feature mean (Mean) or std (Standarddeviation)
* In case of three axial measurements, the last part of the variable defines the axis (X, Y or Z)

### Variable Units

* subjectid: identifier of an observed volunteer within an age bracket of 19-48 years
    * Data type: Numeric
    * Value range: 1 - 30
* activityname: Label string of the observed activity a person was performing
    * Data type: factor
    * Labels: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
* Rows 3 - 68: Means of selected features per subjectid and activityname
    * Data type: The feature means have the same units as their input data. Due to the normalization they do not have units attached see [Input Data Units](#input-data-units)
    * Value range: [-1, 1]


### Additional notes

* Some input variable names contain the string 'BodyBody'. I considered this to be a typo and transformed it to the string 'Body'.
* The original input data contained duplicated variable names. This does not affect our script since the features with duplicated names are not selected as source for the output data.

## Transformations

To compute the mean values for the features, the input data was grouped by column 1 and column 2. Afterwards the mean for each feature variable per group was computed.

The activity ids were converted to readable activity descriptions by loading the activity descriptions from the file "activity_labels.txt" provieded with the input data.