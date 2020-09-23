##
## A Neural Network Analysis of the 14-3-3 Data
##

## Libraries
library(tidyverse)
library(keras)
use_python("/Users/mheaton/opt/anaconda3/bin/python")
library(MLmetrics)
library(caret)

## Read in and merge the data
train <- read_csv("./train.csv")
test <- read_csv("./test.csv")
answer <- read_csv("./answerBool.csv")
dat <- bind_rows(train, test)
sn <- dat$SiteNum

#########################
## Impute Missing Data ##
#########################

## Run a regression imputation for missing PSSM values
PSSM.lm <- lm(PSSM~SVM+ANN, data=dat)
dat <- dat %>% 
  mutate(PSSM=replace(PSSM, is.na(PSSM), predict.lm(PSSM.lm, newdata=dat %>% filter(is.na(PSSM)))))

## Set Consensus to average of the three
cons <- with(dat %>% filter(is.na(Consensus)), {
  round((ANN+SVM+PSSM)/3,3)
})
dat <- dat %>%
  mutate(Consensus=replace(Consensus, is.na(Consensus), cons))


#########################
## Preprocess the Data ##
#########################

## Set up Indicator Variables
dat <- dat %>% mutate(Amino.Acid=ifelse(Amino.Acid=="S",1,0))

## Calculate principal components
# pc <- preProcess(x=dat %>% select(-SiteNum, -Response), method="pca",
#                  pcaComp=2)
# dat <- predict(pc, dat)

## Scale to [0,1]
trans01 <- preProcess(x=dat %>% select(-SiteNum), method="range",
                      rangeBounds=c(0,1))
dat <- predict(trans01, newdata=dat) %>% 
  mutate(SiteNum=sn)

## Resplit the data
train <- dat %>% filter(!is.na(Response)) %>% as.data.frame()
test <- dat %>% filter(is.na(Response))

##############################
## Develop a SMOTEd dataset ##
## to help the imbalance    ##
##############################

train$Response <- as.factor(train$Response)
nrep <- 2000
nother <- 100*(sum(train$Response=="0")/(sum(train$Response=="1")*nrep/100))
smoted <- DMwR::SMOTE(Response~.-SiteNum, 
                      data=train,
                      perc.over=nrep,
                      perc.under=nother)
table(smoted$Response)

#####################################
## Fit Neural Networks using keras ##
#####################################

## Set up matrices for data
X.train <- train %>% select(-Response, -SiteNum) %>%
  as.matrix()
y.train <- train %>% pull(Response) %>% as.numeric() %>%
  as.matrix(., ncol=1)-1
X.test <- test %>% select(-Response, -SiteNum) %>%
  as.matrix()
y.test <- test %>% select(Response) %>%
  as.matrix(., ncol=1)

## Define a NN
nn.model <- keras_model_sequential() #Initialize
nn.model %>%
  layer_dense(units=3, input_shape=ncol(X.train)) %>%
  layer_activation("relu") %>%
  layer_dense(units=1) %>%
  layer_activation("sigmoid")

## Define how to fit a NN
nn.model %>% compile(optimizer=optimizer_sgd(lr=0.01),
                     loss="binary_crossentropy",
                     metrics=c("Precision", "Recall"))

## Now fit a NN
fit.history <- nn.model %>% fit(x=X.train,
                                y=y.train,
                                batch_size=32,
                                epochs=1000,
                                validation_split=0.1,
                                verbose=0)
plot(fit.history)


## Generate predictions
preds <- nn.model %>% predict_classes(x=X.test)
table(answer$Predicted, preds) %>% addmargins()
F1_Score(answer$Predicted*1, preds, positive=1)

## Calibrate Probabilities for better prediction?
train.probs <- nn.model %>% predict_proba(x=X.train)
test.probs <- nn.model %>% predict_proba(x=X.test)
calib.df <- data.frame(y=y.train, p=train.probs)
calib.model <- glm(y~p, family="binomial", data=calib.df)
calib.probs <- predict(calib.model, newdata=data.frame(p=test.probs), type="response")
calib.preds <- calib.probs>0.5
table(answer$Predicted, calib.preds) %>% addmargins()
F1_Score(answer$Predicted, calib.preds, positive=TRUE)






