# KNN-DTree-CV
This is an implementation of K Nearest Neighbors, Decision Tree and Cross Validation in R on the ```pima-indians-diabetes``` dataset.  

The function ```knn``` calculates the K nearest neighbors for the training dataset and return a vector of predictions for the test dataset.  

The ```rpart``` library is used to implement the decision tree in the ```dtree``` function.  

The ```generate_k_folds``` function randomly assigns n datapoints to k folds for cross validation. This function will be used  in k_fold_cross_validation_prediction.  

The ```k_fold_cross_validation_prediction``` function returns a vector of predicted class values for each instance in the dataset.  

A confusion matrix, accuracy, recall and precision are also calculated in appropriate functions.
