###############
## Modelling ##
###############

## Libraries
library(tidyverse)
library(caret)
library(MLmetrics)
library(DataExplorer)

## Read in data
phos <- read_csv('phos_cleaned.csv')

## Dummy Variables
IVTrans <- dummyVars(Response~.-SiteNum-Set, data=phos)
phos <- predict(IVTrans, newdata=phos)  %>% as.data.frame() %>%
  bind_cols(., phos %>% select(SiteNum, Set, Response))

# Set factors
phos <- phos %>% mutate_at(c('Set','Amino.AcidS','Amino.AcidT','Response'), funs(factor(.)))

## Split train and test
phos_train <- phos %>% filter(Set == 'train')
phos_test <- phos %>% filter(Set == 'test')

## Set up F1 metric
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = 1)
  c(F1 = f1_val)
}

##
## XGBTree Model
##
train.control=trainControl(method="repeatedcv", number=10, repeats=3,
                          summaryFunction = f1)

tunegrid = expand.grid(
                        nrounds = seq(from = 50, to = 1500, by = 50),
                        eta = 0.025,
                        max_depth = 2,
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 1:3,
                        subsample = 0.75
                       )

xgb_model <- train(form=Response~.,
               data = (phos_train %>% select(-Set, -SiteNum)),
               method = "xgbTree",
               preProcess = c("center", "scale"),
               metric = "F1",
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

##
## Random Forest Model
##
rf.train.control=trainControl(method="repeatedcv", number=10, repeats=3,
                          summaryFunction = f1)


rf.tunegrid <- expand.grid(
  .mtry = 3,
  .splitrule = "gini",
  .min.node.size = 7
)

set.seed(122)
rf_model <- train(form=Response~.,
               data = (phos_train %>% select(-Set, -SiteNum)),
               method = "ranger",
               preProcess = c("center", "scale", "nzv"),
               metric = "F1",
               trControl = rf.train.control,
               tuneGrid = rf.tunegrid
               )
beepr::beep(sound=5)

rf_model$bestTune
print(rf_model)
plot(rf_model)

preds <- predict(rf_model, phos_test)
out <- data.frame(Id=phos_test$SiteNum, Predicted=preds)
write_csv(out, "rf-preds.csv")