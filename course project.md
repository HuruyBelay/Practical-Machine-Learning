#### Background
    
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website [here](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset).

#### Data

# The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

# The test data are available here:https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# The data for this project come from this source:http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

###### What you should submit

# The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

# Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
# You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details.

###### Reproducibility

# Due to security concerns with the exchange of R code, your code will not be run during the evaluation by your classmates. Please be sure that if they download the repo, they will be able to view the compiled HTML version of your analysis.

###### Data Preprocessing

###### load libraries

set.seed(12345)
library(caret)
library(lattice)
library(ggplot2)
library(rpart)

## Loading the training and testing data sets

pml.training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
pml.testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

dim(pml.training)
head(pml.training)

## We are interested in variables that predict the movement. The set contains a number of variables that can be removed:
    
## Variables (X and user_name) should be removed and
## cvtd_timestamp is removed because it is a factor instead of a numeric value and the raw_timestamp_part_1 + raw_timestamp_part_2 contain the same info in numeric format.

rIndex <- grep("X|user_name|cvtd_timestamp", names(pml.training))
pml.training <- pml.training[, -rIndex]

Some variable have also near Zero variance which indicates that they do not contribute to the model. They are removed from the set.

nzv <- nearZeroVar(pml.training)
pml.training <- pml.training[, -nzv]

# There are varaiables contains NA's. Variables with NA`s makes the model creation slower and also results in lower accuracy in the model. These variables will be removed from the set:

NAs <- apply(pml.training, 2, function(x) {
sum(is.na(x))
})
pml.training <- pml.training[, which(NAs == 0)]

dim(pml.training)

# The original set is large. We create a smaller training set of 80% of the original set

tIndex <- createDataPartition(y = pml.training$classe, p = 0.2, list = FALSE)
pml.sub.training <- pml.training[tIndex, ]    # 3927 obs. of 56 variables
pml.test.training <- pml.training[-tIndex, ]  # test set for cross validation

##### Model creation

# We can now create a model based on the pre-processed data set. Note that at this point, we are still working with a large set of variables. We do have however a reduced number of rows.

# A first attempt to create a model is done by fitting a single tree:

pml.sub.training<-as.data.frame(pml.sub.training)
modFit <- train(classe ~ ., data = pml.sub.training, method = "rpart")
modFit

results <- modFit$results
round(max(results$Accuracy), 4) * 100

# The accuracy of the model is low: 55.11 %

# A second attempt to create a model is done by using Random forests:

library(randomForest)
ctrl <- trainControl(method = "cv", number = 4, allowParallel = TRUE)
modFit <- train(classe ~ ., data = pml.sub.training, method = "rf", 
prof = TRUE, trControl = ctrl)
modFit

results <- modFit$results
accuracy<-round(max(results$Accuracy), 4) * 100
accuracy

# This second attempt provides us with a model that has a much higher accuracy which is `r accuracy`.

##### Cross-validation

# We now use the modFit to predict new values within the test set that we created for cross-validation:

pred <- predict(modFit, pml.test.training)
pml.test.training$predRight <- pred == pml.test.training$classe
table(pred, pml.test.training$classe)

# We can calculate the accuracy of the prediction:

pRes <- postResample(pred, pml.test.training$classe)
pRes

# The prediction fitted the test set even slightly better than the training set: 99.1526%.

##### Expected out of sample error

# We can calculate the expected out of sample error based on the test set that we created for cross-validation:

cfM <- confusionMatrix(pred, pml.test.training$classe)
cfM

# The expected out of sample error is: 0.8474 %

# Note: The confusionMatrix function from the Caret package does provide all the information that we calculated 'by hand' in the first part of the Cross-validation. It shows that both methods provide the same answer.

##### 20 case test set predictions

# The is the script that creates the 20 .txt files:

pred2 <- predict(modFit, pml.testing)
pml.testing.2 <- pml.testing
pml.testing.2$classe <- pred2

pml_write_files = function(x) {
n = length(x)
for (i in 1:n) {
filename = paste0("problem_id_", i, ".txt")
write.table(x[i], file = filename, quote = FALSE, row.names = FALSE, 
col.names = FALSE)
}
}


answers <- pml.testing.2$classe

pml_write_files(answers)
answers

