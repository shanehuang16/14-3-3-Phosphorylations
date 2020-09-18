##
## 14-3-3 Phosphorylations

## Libraries
library(tidyverse)
library(caret)
library(DataExplorer)

## Read files
phos_train <- read_csv("train.csv")
phos_test <- read_csv("test.csv")

phos_data <- bind_rows(train=phos_train, test=phos_test, .id="Set")

## EDA
summary(phos_data)
plot_missing(phos_data)
plot_correlation(phos_data, type="continuous", 
                 cor_args=list(use="pairwise.complete.obs"))

# Change characters to factors
phos_data <- phos_data %>% mutate_at(c('Set','Amino.Acid'), as.factor(.))
