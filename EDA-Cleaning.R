###########################
## EDA and Data Cleaning ##
###########################

## Libraries
library(tidyverse)
library(DataExplorer)

## Read files
phos_train <- read_csv("train.csv")
phos_test <- read_csv("test.csv")

phos <- bind_rows(train=phos_train, test=phos_test, .id="Set")

## EDA
summary(phos)
plot_missing(phos)
plot_correlation(phos, type="continuous", 
                 cor_args=list(use="pairwise.complete.obs"))

# Change characters to factors
phos <- phos %>% mutate_at(c('Set','Amino.Acid','Response'), funs(factor(.)))

# Stochastic Imputation for PSSM
pssm.lm <- lm(PSSM~Amino.Acid+Iupred.score+ANN+SVM+normalization, data=phos)
pssm.preds <- round((predict(pssm.lm, newdata=(phos %>% filter(is.na(PSSM))))+
                   rnorm(sum(is.na(phos$PSSM)), 0, sigma(pssm.lm)))^2,3)
phos <- phos %>%
  mutate(PSSM=replace(PSSM, is.na(PSSM), pssm.preds))

rm(pssm.lm)

# Compute missing Consensus using mean
phos <- phos %>% mutate(Consensus=replace(Consensus, is.na(Consensus),
                                              round(rowMeans(phos %>% filter(is.na(Consensus)) %>% select('ANN','PSSM','SVM')),3)))


# Write out cleaned data
write_csv(x=phos, path="./phos_cleaned.csv")
