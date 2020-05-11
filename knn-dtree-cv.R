require(caret)
require(rpart)

# ------- Part A -------

calculate_euclidean <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the euclidean distance between p and q.
  return(sqrt(sum((p-q)^2)))
  
}

calculate_cosine <- function(p, q) {
  # Input: p, q are numeric vectors of the same length
  # output: a single value of type double, containing the cosine distance between p and q.
  return( sum(p*q)/sqrt(sum(p^2)*sum(q^2)) )
  
}

knn <- function(x_train, y_train, x_test, distance_method = 'cosine', k = 3){
  # INPUT:
  # x_train: Matrix with dimensions: (number_training_samples x number_features)
  # y_train: Vector with length number_training_samples of type factor - refers to the class labels
  # x_test: Matrix with dimensions: (number_test_samples x number_features)
  # k: integer, represents the 'k' to consider in the knn classifier
  # distance_method: String, can be of type ('euclidean' or 'cosine')
  
  # OUTPUT:
  # A vector of predictions of length = number of samples in y_test and of type factor.
  
  # Vector to store the final prediction
  prediction<-vector()
  # Iterating through all the values
  for (j in 1:nrow(x_test)){
    # Extracting only single row
    row1<-as.numeric(x_test[j,])
    distances<-vector()
    for(i in 1:nrow(x_train)){
      # Other rows to compare with
      row2<-as.numeric(x_train[i,])
      # Using cosine distance
      if (distance_method == 'cosine'){
        distance<-calculate_cosine(row1,row2)
        distances<-c(distances,distance)
        # Sorting in decending order
        sorted <- sort(distances, index.return=TRUE, decreasing = TRUE)
        
      }
      # Using Euclidean distance
      else if(distance_method == 'euclidean'){
        distance<-calculate_euclidean(row1,row2)
        distances<-c(distances,distance)
        # Sorting in ascending order
        sorted <- sort(distances, index.return=TRUE)
        
      }
    }
    sorted_list<-lapply(sorted, `[`, sorted$ix %in% head(unique(sorted$ix),k))$ix
    classes<-vector()
    sorted_list_y<-y_train[sorted_list]
    # Number of Class values 0
    candidate1<-sum(sorted_list_y == 0)
    # Number of Class values 1
    candidate2<-sum(sorted_list_y == 1)
    if(candidate1>candidate2){
      prediction<-c(prediction,0)
    }
    else if(candidate2>candidate1){
      prediction<-c(prediction,1)
    }
    else{
      prediction<-c(prediction,0)
    }
  }
  factor(prediction)
}

# ------- Part B -------

dtree <- function(x_train, y_train, x_test){
  # INPUT:
  # x_train: Matrix with dimensions: (number_training_samples x number_features)
  # y_train: Vector with length number_training_samples of type factor - refers to the class labels
  # x_test: Matrix with dimensions: (number_test_samples x number_features)
  # n_folds: integer, refers to the number of folds for n-fold cross validation
  
  # OUTPUT:
  # A vector of predictions of length = number of samples in y_test and of type factor.
  
  # Combining the separate variables
  combined <- cbind(x_train, y_train)
  # Training 
  model <- rpart(y_train ~ ., data = combined,parms = list(split = 'information'))
  # Predicting
  predict(model,x_test,type="class")
  
}

# ------- Part C -------

generate_k_folds <- function(n, k) {
  # This function should randomly assign n datapoints to k folds
  # for cross validation. We will use this function in 
  # k_fold_cross_validation_prediction below.
  
  # INPUT:
  # n: Total number of samples.
  # k: The number of cross validation folds (e.g. 10 for 10-fold CV)
  
  # OUTPUT:
  # A vector representing the "fold" assignment of each row in
  # a dataset with n rows (e.g. 1 = first fold, 2 = second fold)
  # Example output:
  # > generate_k_folds(10, 3)
  # [1] 3 3 1 1 3 2 2 2 1 1
  sample(rep(1:k, length.out = n))
}

k_fold_cross_validation_prediction <- function(x, y, k, k_folds, classifier_function) {
  # INPUT:
  # x: Dataframe with dimensions: (number_samples x number_features)
  # y: Vector with length number_samples of type factor - refers to the class labels
  # k: The total fold number of cross validation.
  # k_folds: a vector representing the "fold" assignment of each row in the dataset, generated with
  # the generate_k_folds function
  # classifier_function: The classifier function you wish to use, it can be either knn or dtree. 
  # Note that you don't need to use quote marks for the functions as parameters.
  # OUTPUT:
  # A vector of predicted class values for each instance in x (length = nrow(x)). The ith
  # prediction should correspond to the ith row in x.
  
  prediction <- c()
  for (i in 1:k){
    prediction <- c(prediction, classifier_function(x[k_folds!=i,], y[k_folds!=i], x[k_folds==i,]))
  }
  factor(prediction, labels = levels(y))
}

# ------- Part D -------

calculate_confusion_matrix <- function(y_pred, y_true){
  # Given the following:
  
  # INPUT:
  # y_pred: predicted class labels (vector, each value of type factor)
  # y_true: ground truth class labels (vector, each value of type factor)
  
  # OUTPUT:
  # a confusion matrix of class "table" with Prediction to the left, and Reference on the top:
  # TN FN 
  # FP TP
  table(y_pred,y_true)
}

calculate_accuracy <- function(confusion_matrix){
  # Given the following:
  
  # INPUT:
  # confusion_matrix: A confusion matrix
  
  # OUTPUT:
  # prediction accuracy
  
  (confusion_matrix[1] + confusion_matrix[4])/(confusion_matrix[1]+confusion_matrix[2]+confusion_matrix[3]+confusion_matrix[4])
  
}

calculate_recall <- function(confusion_matrix){
  # Given the following:
  
  # INPUT:
  # confusion_matrix: A confusion matrix
  
  # OUTPUT:
  # prediction recall
  (confusion_matrix[1])/(confusion_matrix[1]+confusion_matrix[2])
  
}

calculate_precision <- function(confusion_matrix){
  # Given the following:
  
  # INPUT:
  # confusion_matrix: A confusion matrix
  
  # OUTPUT:
  # prediction precision
  (confusion_matrix[1])/(confusion_matrix[1]+confusion_matrix[3])
}

