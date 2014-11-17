Get and Clean Data project.
This document descibes the intent of the script run_analysis.R. 
This scripts is used to filter and extract a tidy data set from the given input data.

2 scripts used to complete the project.
	1. run_analysis_functions.R
	2. run_analysis.R
libraries used:
	stringr.

run_analysis_functions.R
	This scripts define the function to combine the training data, combine test data, extracting data etc.
	Basically functions were defined in this so that the main script is concise.
	
run_analysis.R
	Main script which extracts the required data from the given input data sets.
	
	It follow the below steps to accomplish the same.
	
	1. Read the measurement and activity definitions.
	2. Combine the training data set (like actual measurements, labels and subjects into 1 data frame).
	   NOTE that in this step itself, the descriptive names to the various measurements are applied.
	3. Similarly the test data set is combined.
	4. It then extracts the data of only the mean() and std() measurements. It uses the function str_detect from 'stringr'
	   library for this purpose.
	5. Then it applies labels to activity. that is, the activity id in the data set (1,2,... etc) is replaced with the 
	   actual activity name like WALKING, STANDING etc.
	6. Then it extracts the tidy data set with mean of the measurements grouped by activity and subject.
	   This is accomplished using aggregate function, which returns a data.frame. (using tapply would have returned a vector).
	7. Finally it writes this tidy data set into a text file, 'getDataProject_TidyDataSet.txt'.
	
NOTE that the actual scripts have very little comments in it.
Actually it is considered best practice to have little comment in code. Rather the variable names and function names 
should best describe the intent of the script/program.

