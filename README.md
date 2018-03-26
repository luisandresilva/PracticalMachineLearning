---
title: "Practical Machine Learning"
author: "Luis Andre Dutra e Silva"
date: "3/25/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)
```

### Pre-processing

#### Download original data
```{r}
if (!file.exists("pml-training.csv")) {
    library(downloader)
    download("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
         dest="pml-training.csv", mode="wb") 
}
if (!file.exists("pml-testing.csv")) {
    library(downloader)
    download("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
         dest="pml-testing.csv", mode="wb") 
}
```

#### Load data
```{r}
set.seed(1969)
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')
```
#### Replace NAs
The NAs in numeric columns are replaced with means and the NAs in factor columns are replaced by it most common value
```{r}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in 1:ncol(training)){
  if (class(training[,i])!="factor") {
    training[is.na(training[,i]), i] <- mean(training[,i], na.rm = TRUE)
  } else {
    training[is.na(training[,i]), i] <- Mode(training[,i])  
  }
}
for(i in 1:ncol(testing)){
  if (class(testing[,i])!="factor") {
    testing[is.na(testing[,i]), i] <- mean(testing[,i], na.rm = TRUE)
  } else {
    testing[is.na(testing[,i]), i] <- Mode(testing[,i])  
  }
}
```

### Select Variables
After inspecting variables, there are many derived columns, so only original variables are kept.
```{r}
library(caret)

training <- training[sample(nrow(training)),]
ttrain <- training[,grep("^(classe|num_window|magnet|pitch|roll|yaw|roll|gyros|accel)",names(training))]
inTrain <- createDataPartition(ttrain$classe, p=0.95, list=FALSE)
train <- ttrain[inTrain,]
valid <- ttrain[-inTrain,]
test <- testing[,grep("^(classe|num_window|magnet|pitch|roll|yaw|roll|gyros|accel)",names(testing))]

cat("train size : ", dim(train), " | valid size : ", dim(valid), " | test  size : ", dim(test))


```
### Fit Models

#### Fit Random Forest
The random forest algorithm was chosen because it is explainable since it generates multiple trees that vote for the final classification.
```{r}
library(randomForest)
model <- randomForest(classe ~ ., data=train)
preds <- predict(model, newdata = valid)
conf <- confusionMatrix(preds,valid$classe)
conf$overall["Accuracy"]
```

### Prediction

Finally, the prediction of the 20 test samples is done.
```{r}
preds <- predict(model, newdata = test)
preds
```