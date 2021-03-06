###STUDY DESIGN
	The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ 
	and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
	Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency
	of 20Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration 
	signals(tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
	
	Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals 
	(tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using 
	the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
	
	Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, 
	fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
	These signals were used to estimate variables of the feature vector for each pattern:  
	
	'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
	
### Requirements:
   The content of https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip is unzipped 
   in .\\getdata_projectfiles_UCI HAR Dataset\\ directory

###CODE BOOK
####subject_key
	Description:
		The identifier for the test subject out of the group of 30 volunteers. 
	Range:
		1 - 30 
	Unit:
		no unit

####tBodyAcc_mean_X
	Description:
		The mean value of X-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tBodyAcc_mean_Y
	Description:
		The mean value of Y-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tBodyAcc_mean_Z
	Description:
		The mean value of Z-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tBodyAcc_std_X
	Description:
		The standard deviation value of X-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tBodyAcc_std_Y
	Description:
		The standard deviation value of Y-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tBodyAcc_std_Z
	Description:
		The standard deviation value of Z-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_mean_X
	Description:
		The mean value of X-axis total acceleration measured in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_mean_Y
	Description:
		The mean value of Y-axis total acceleration measured in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_mean_Z
	Description:
		The mean value of Z-axis total acceleration measured in time domain.
	Summary choices:
		The average of the value group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_std_X
	Description:
		The standard deviation value of X-axis total acceleration measured in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_std_Y
	Description:
		The standard deviation value of Y-axis total acceleration measured in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####tGravityAcc_std_Z
	Description:
		The standard deviation value of Z-axis total acceleration measured in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g
		
####tBodyAccJerk_mean_X
	Description:
		The mean value of rate of change of X-axis body acceleration in time domain.
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyAccJerk_mean_Y
	Description:
		The mean value of rate of change of Y-axis body acceleration in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyAccJerk_mean_Z
	Description:
		The mean value of rate of change of Z-axis body acceleration in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyAccJerk_std_X
	Description:
		The standard deviation value of rate of change of X-axis body acceleration in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyAccJerk_std_Y
	Description:
		The standard deviation value of rate of change of Y-axis body acceleration in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyAccJerk_std_Z
	Description:
		The standard deviation value of rate of change of Z-axis body acceleration in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####tBodyGyro_mean_X
	Description:
		The mean values of X-axis angular velocity vector measured by the gyroscope in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####tBodyGyro_mean_Y
	Description:
		The mean values of Y-axis angular velocity vector measured by the gyroscope in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####tBodyGyro_mean_Z
	Description:
		The mean values of Z-axis angular velocity vector measured by the gyroscope in time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####tBodyGyro_std_X
	Description:
		The standard deviation value of X-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####tBodyGyro_std_Y
	Description:
		The standard deviation value of Y-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####tBodyGyro_std_Z
	Description:
		The standard deviation value of Z-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s
				
####tBodyGyroJerk_mean_X
	Description:
		The mean values of the rate of change of X-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyGyroJerk_mean_Y
	Description:
		The mean values of the rate of change of Y-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyGyroJerk_mean_Z
	Description:
		The mean values of the rate of change of Z-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyGyroJerk_std_X
	Description:
		The standard deviation value of the rate of change of X-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyGyroJerk_std_Y
	Description:
		The standard deviation value of the rate of change of Y-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyGyroJerk_std_Z
	Description:
		The standard deviation value of the rate of change of Z-axis angular velocity vector measured by the gyroscope in
		time domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####tBodyAccMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyAccMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tGravityAccMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tGravityAccMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyAccJerkMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyAccJerkMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyGyroMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyGyroMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?		

####tBodyGyroJerkMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####tBodyGyroJerkMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?	
		
####fBodyAcc_mean_X
	Description:
		The mean value of X-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####fBodyAcc_mean_Y
	Description:
		The mean value of Y-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####fBodyAcc_mean_Z
	Description:
		The mean value of Z-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####fBodyAcc_std_X
	Description:
		The standard deviation value of X-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####fBodyAcc_std_Y
	Description:
		The standard deviation value of Y-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g

####fBodyAcc_std_Z
	Description:
		The standard deviation value of Z-axis body acceleration signal obtained by subtracting the gravity from the total acceleration measured in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g
		
####fBodyAccJerk_mean_X
	Description:
		The mean value of rate of change of X-axis body acceleration in frequency domain.
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyAccJerk_mean_Y
	Description:
		The mean value of rate of change of Y-axis body acceleration in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyAccJerk_mean_Z
	Description:
		The mean value of rate of change of Z-axis body acceleration in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyAccJerk_std_X
	Description:
		The standard deviation value of rate of change of X-axis body acceleration in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyAccJerk_std_Y
	Description:
		The standard deviation value of rate of change of Y-axis body acceleration in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyAccJerk_std_Z
	Description:
		The standard deviation value of rate of change of Z-axis body acceleration in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		standard gravity unit g/s

####fBodyGyro_mean_X
	Description:
		The mean values of X-axis angular velocity vector measured by the gyroscope in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####fBodyGyro_mean_Y
	Description:
		The mean values of Y-axis angular velocity vector measured by the gyroscope in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####fBodyGyro_mean_Z
	Description:
		The mean values of Z-axis angular velocity vector measured by the gyroscope in frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####fBodyGyro_std_X
	Description:
		The standard deviation value of X-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####fBodyGyro_std_Y
	Description:
		The standard deviation value of Y-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s

####fBodyGyro_std_Z
	Description:
		The standard deviation value of Z-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s
				
####fBodyGyroJerk_mean_X
	Description:
		The mean values of the rate of change of X-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyGyroJerk_mean_Y
	Description:
		The mean values of the rate of change of Y-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyGyroJerk_mean_Z
	Description:
		The mean values of the rate of change of Z-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyGyroJerk_std_X
	Description:
		The standard deviation value of the rate of change of X-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyGyroJerk_std_Y
	Description:
		The standard deviation value of the rate of change of Y-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyGyroJerk_std_Z
	Description:
		The standard deviation value of the rate of change of Z-axis angular velocity vector measured by the gyroscope in
		frequency domain.
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		radians/s^2

####fBodyAccMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyAccMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyAccJerkMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyAccJerkMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyGyroMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyGyroMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?		

####fBodyGyroJerkMag_mean
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?

####fBodyGyroJerkMag_std
	Description:
		?
	Summary choices:
		The average of the values group by subject_key and activity_lbl
	Unit:
		?			

####activity_lbl
	Description:
		The identifier for the activity that test subject is performing. 
	Values:
		WALKING
		WALKING_UPSTAIRS
		WALKING_DOWNSTAIRS
		SITTING
		STANDING
		LAYING
	Unit:
		no unit
