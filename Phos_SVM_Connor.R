#############################
## Support Vector Machines ##
#############################

## Libraries
library(tidyverse)
library(caret)

## Read Data
phos <- read_csv("phos_cleaned.csv")
phos$Response <- as.factor(phos$Response)


## Split into train and test
phos_train <- phos %>% filter(Set == "train")
phos_test <- phos %>% filter(Set == "test")

## svmLinear model
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              summaryFunction = prSummary)

tunegrid <- expand.grid(C = seq(0, 3, length = 40))

##
tictoc::tic()
phos_svm <- train(form=Response~.,
                  data = (phos_train %>% select(-Set, -SiteNum)),
                  method = "svmLinear",
                  preProcess = c("center", "scale", "pca"),
                  metric = "F",
                  trControl = train_control,
                  tuneGrid = tunegrid
)
tictoc::toc()
beepr::beep(sound=1)

phos_svm$bestTune
print(phos_svm)
plot(phos_svm)

preds_svm <- phos_svm %>% predict(phos_test) 
svm_preds1<- data.frame(Id = phos_test$SiteNum, Predicted = preds_svm)
write.csv(svm_preds1,"C:\\Users\\18592\\Documents\\Kaggle\\Phosphorylations_Team\\14-3-3-Phosphorylations\\SVM1 Predictions.csv", row.names = FALSE)
