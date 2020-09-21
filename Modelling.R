###############
## Modelling ##
###############

## Read in data
phos <- read_csv('phos_cleaned.csv')

## Libraries
library(tidyverse)
library(caret)
library(DataExplorer)

## Split train and test
phos_train <- phos %>% filter(Set == 'train')
phos_test <- phos %>% filter(Set == 'test')


## XGBTree Model
train.control=trainControl(method="repeatedcv", number=10, repeats=3,
                          summaryFunction = prSummary)

tunegrid = expand.grid(nrounds = seq(10,50,length.out = 4),
                       max_depth = 1:5,
                       eta = seq(0.1,0.4,0.1),
                       gamma = 0,
                       colsample_bytree = 0.6,
                       min_child_weight = 1:3,
                       subsample = 0.5)

xgb_model <- train(form=Response~.,
               data = (phos_train %>% select(-Set, -SiteNum)),
               method = "xgbTree",
               preProcess = c("center", "scale"),
               metric = "F",
               trControl = train.control,
               tuneGrid = tunegrid
               )
beepr::beep(sound=5)

xgb_model$bestTune
print(xgb_model)
plot(xgb_model)

preds <- predict(xgb_model, phos_test)
out <- data.frame(Id=phos_test$SiteNum, Predicted=preds)
write_csv(out, "xgb-preds.csv")