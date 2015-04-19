---
title: "Practical Machine Learning - Course project"
author: "Roberto Baladrón"
date: "Sunday, April 19, 2014"
output:
  html_document:
    theme: spacelab
---
# Summary
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. 
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

This project was to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 


## Libraries
The code checks for required packages, installs and loads the following required libraries
```{r}
library(caret)
library(corrplot)
library(kernlab)
library(knitr)
library(randomForest)
```
```{r setoptions, echo = FALSE}
opts_chunk$set(cache = FALSE)
```

## Loading and preprocessing the data
The csv files containing the the training and test data was downloaded into a data folder in the working directory. 

```{r, eval = FALSE}
### Check if a data folder exists; if not then create one
if (!file.exists("data")) {dir.create("data")}

### Check if data files exits; if not download files, from the URL to destination files
URLArchivoTraining <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URLArchivoTesting <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
ArchivoTraining <- "./data/pml-training.csv"
ArchivoTesting <- "./data/pml-testing.csv"

### Download the files and note the time
download.file(URLArchivoTraining, ArchivoTraining)
download.file(URLArchivoTesting, ArchivoTesting)
dateDownloaded <- date()
```

The training and test data was then loaded into R.

```{r}
## Read the datasets for training and test
DSTraining <- read.csv(ArchivoTraining, na.strings= c("NA",""," "))
DSTest <- read.csv(ArchivoTesting, na.strings= c("NA",""," "))
```

There was a lot of NA values in the data which would create a lot of noise for the model. As a result, these columns were removed from the data set. The first eight columns that acted as identifiers for the experiment were also removed.

```{r}
##  Cleaning the data, remove columns identifier.
DSTrainingNAs <- apply(DSTraining, 2, function(x) {sum(is.na(x))})
DSTrainingClean <- DSTraining[,which(DSTrainingNAs == 0)]
DSTrainingClean <- DSTrainingClean[8:length(DSTrainingClean)]
```

## Creating a model
The test data set was split up into training and cross validation sets in a 70:30 ratio.

```{r}
### Split the cleaned testing data into training and cross validation
DSTrainingSplit <- createDataPartition(y = DSTrainingClean$classe, p = 0.7, list = FALSE)
trainingData <- DSTrainingClean[DSTrainingSplit, ]
crossValidationData <- DSTrainingClean[-DSTrainingSplit, ]
```

A random forest model was selected to predict the classification because it has methods for balancing error in class population unbalanced data sets. The correlation between any two trees in the forest increases the forest error rate. Therefore, a correllation plot was produced in order to see how strong the variables relationships are with each other.

```{r, fig.height = 6, fig.width = 8}
## Plotting a correlation matrix
correlacionMatrix <- cor(trainingData[, -length(trainingData)])
corrplot(correlacionMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))
```

In this type of plot the dark red and blue colours indicate a highly negative and positive relationship respectively between the variables. There isn't much concern for highly correlated predictors which means that all of them can be included in the model.

Then a model was fitted with the outcome set to the training class and all the other variables used to predict.

```{r}
## Prediction model using trainingData
modeloPrediccion <- randomForest(classe ~ ., data = trainingData)
modeloPrediccion
```

The model produced a very small OOB error rate of .56%. This was deemed satisfactory enough to progress the testing.

### Cross-validation
The model was then used to classify the remaining 30% of data. The results were placed in a confusion matrix along with the actual classifications in order to determine the accuracy of the model.

```{r}
### Crossvalidate the model using the remaining 30% of data
crossValidationPrediccion <- predict(modelPrediccion, crossValidationData)
confusionMatrix(crossValidationData$classe, crossValidationPrediccion)
```


### Predictions
The test data set is cleaned in the same manner as before. The model was then used to predict the classifications of the 20 results of this new data.

```{r}
### Apply the same proccess to the final testing data
dataTestNAs <- apply(DSTest, 2, function(x) {sum(is.na(x))})
dataTestClean <- DSTest[,which(dataTestNAs == 0)]
dataTestClean <- dataTestClean[8:length(dataTestClean)]

## Predict the classes of the test set
predictTest <- predict(model, data_test_clean)
predictTest
```
