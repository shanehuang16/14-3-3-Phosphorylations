# Libraries needed
library(tidyverse)
library(DataExplorer)
library(caret)

# Merge datasets
phos_data <- phos_cleaned

summary(phos_data)
plot_missing(phos_data)
plot_correlation(phos_data, type="continuous",
                 cor_args=list(use="pairwise.complete.obs"))

phos_data$Amino.Acid <- as.factor(phos_data$Amino.Acid)


# try prediction without pssm na's
# phos_data2 <- phos_data[complete.cases(phos_data[ , 3:6]),]
phos_data2 <- phos_data %>% select(-Set)
phos_data2 <- phos_data2 %>% select(-Amino.Acid)
phos_data2 <- phos_data2 %>% select(-PSSM)
phos_data2 <- phos_data2 %>% select(-Consensus)



phos_train2 <- phos_data2[which(!is.na(phos_data2$Response)),]
phos_train2 <- phos_train2 %>% select(-SiteNum)


phos_test2 <- phos_data2[which(is.na(phos_data2$Response)),]
test.id <- phos_test2 %>% select(SiteNum)
phos_test2 <- phos_test2 %>% select(-SiteNum)


library(class)

# normalize?
# YourNormalizedDataSet <- as.data.frame(lapply(YourDataSet, normalize))

plot_missing( phos_train2[, -1])

phos_pred1 <- knn(train = phos_train2[, -5],
                  test = phos_test2[, -5],
                  cl = phos_train2[, 5, drop = TRUE],
                  k=60)

pred1 <- cbind(test.id, phos_pred1)

names(pred1)[1] <- "Id"
names(pred1)[2] <- "Predicted"


write.csv(pred1,"C:\\Users\\eamon\\Documents\\Submission1.csv", row.names = FALSE)
