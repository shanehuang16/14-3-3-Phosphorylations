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

## XGBTree Model
train.control=trainControl(method="repeatedcv", number=10, repeats=3,
                          summaryFunction = f1)

tunegrid = expand.grid(
                        nrounds = seq(from = 50, to = 1000, by = 50),
                        eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                        max_depth = 2,
                        gamma = 0,
                        colsample_bytree = 1,
                        min_child_weight = 3,
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
