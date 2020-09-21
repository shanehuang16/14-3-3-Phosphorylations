#############################
## Support Vector Machines ##
#############################

## Libraries
library(tidyverse)
library(caret)

## Read Data
phos <- read_csv("phos_cleaned.csv")
phos$Amino.Acid <- as.factor(phos$Response)

## Split into train and test
phos_train <- phos %>% filter(Set == "train")
phos_test <- phos %>% filter(Set == "test")

## svmLinear model
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = prSummary)

tunegrid <- expand.grid(C = seq(.5, 3, length = 500))

tictoc::tic()
phos_svm <- train(form=Response~.,
                  data = (phos_train %>% select(-Set, -SiteNum)),
                  method = "svmLinear",
                  preProcess = c("center", "scale", "pca"),
                  metric = "F",
                  trControl = train.control,
                  tuneGrid = tunegrid
)
tictoc::toc()
beepr::beep(sound=1)

phos_svm$bestTune
print(phos_svm)
plot(phos_svm)
