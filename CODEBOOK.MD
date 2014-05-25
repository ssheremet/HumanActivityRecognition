CODEBOOK

DATA

The dataset includes the following files:
=========================================

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

DATA MANIPULATION
- From 'features.txt' was got numeric vector which contains index of mean and standard deviation variables. 
- Test and train data was filtered using this vector.
- Filtered test and train datasets was merged with subjects and activity labels columns
- Test and train data was merged together by rows
- United dataset merged with activity labels data set to achieve descriptive activity labels
- Activity key column was removed
