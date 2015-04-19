#**********************************************************
# Function compruebaPackage 
# Check for a package, install it otherwise and load again
#**********************************************************
compruebaPackage <- function(x) {
  if (!require(x,character.only = TRUE)) {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Check for required packages 
compruebaPackage('caret')
compruebaPackage('kernlab')
compruebaPackage('randomForest')
compruebaPackage('corrplot')


# Check if a data folder exists; if not then create one
if (!file.exists("data")) {dir.create("data")}

# Check if data files exits; if not download files 
if (!file.exists("./data/pml-training.csv") || !file.exists("./data/pml-testing.csv")) {
  # file URL, destination zip file, destination data files
  URLArchivoTraining <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
  URLArchivoTesting <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
  ArchivoTraining <- "./data/pml-training.csv"
  ArchivoTesting <- "./data/pml-testing.csv"
  # Download the files and note the time
  download.file(URLArchivoTraining, ArchivoTraining)
  download.file(URLArchivoTesting, ArchivoTesting)
  dateDownloaded <- date()

}


# Data sets for training and test
DSTraining <- read.csv(ArchivoTraining, na.strings= c("NA",""," "))
DSTest <- read.csv(ArchivoTesting, na.strings= c("NA",""," "))


# Cleaning the data, remove columns identifier.
DSTrainingNAs <- apply(DSTraining, 2, function(x) {sum(is.na(x))})
DSTrainingClean <- DSTraining[,which(DSTrainingNAs == 0)]
DSTrainingClean <- DSTrainingClean[8:length(DSTrainingClean)]

# Split the cleaned testing data into training (70% of data) and cross validation (30% of data)
DSTrainingSplit <- createDataPartition(y = DSTrainingClean$classe, p = 0.7, list = FALSE)
trainingData <- DSTrainingClean[DSTrainingSplit, ]
crossValidationData <- DSTrainingClean[-DSTrainingSplit, ]

# Plottig a correlation matrix
correlacionMatrix <- cor(trainingData[, -length(trainingData)])
corrplot(correlacionMatrix, order = "FPC", method = "circle", type = "lower", tl.cex = 0.8,  tl.col = rgb(0, 0, 0))

# Prediction model and crossvalidate the model
modeloPrediccion <- randomForest(classe ~ ., data = trainingData)
crossValidationPrediccion <- predict(modeloPrediccion, crossValidationData)
confusionMatrix(crossValidationData$classe, crossValidationPrediccion)

# The same proccess to final test data
# Cleaning the data, remove columns identifier.
dataTestNAs <- apply(DSTest, 2, function(x) {sum(is.na(x))})
dataTestClean <- DSTest[,which(dataTestNAs == 0)]
dataTestClean <- dataTestClean[8:length(dataTestClean)]

# Predict the classes of the test set, using prediction model
testPrediccion <- predict(modeloPrediccion, dataTestClean)
testPrediccion
