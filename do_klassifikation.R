library(ggplot2)
library(dplyr)
df <- read.csv("data.csv")
df <- transform(df, Abschluss = as.factor(Abschluss), Geschlecht = as.factor(Geschlecht), Wohngebiet = as.factor(Wohngebiet), Eigenheim = (Eigenheim == "j"), Familienstand = as.factor(Familienstand), MitgliedSportverein = (MitgliedSportverein == "j"), Preiskategorie = as.factor(Preiskategorie))
set.seed(1765)
sample_idx <- sample.int(nrow(df), round(nrow(df)*0.2))
test <- df[sample_idx,]
train <- df[-sample_idx,]
probs <- table(train$Preiskategorie)/length(train$Preiskategorie)
requiredInfoBits <- -sum(probs * log2(probs))
calcEntropy <- function(table) {
  return (sum(colSums(apply(apply(table,2,function(x){x/sum(x)}), 2,function(x){ ifelse(x!=0, x * log2(x), 0)})) * -1))
}