library(rpart)
df <- read.csv("data.csv")
df <- transform(df, Abschluss = as.factor(Abschluss), Geschlecht = as.factor(Geschlecht), Wohngebiet = as.factor(Wohngebiet), Eigenheim = (Eigenheim == "j"), Familienstand = as.factor(Familienstand), MitgliedSportverein = (MitgliedSportverein == "j"), Preiskategorie = as.factor(Preiskategorie))
set.seed(1765)
sample_idx <- sample.int(nrow(df), round(nrow(df)*0.2))
test <- df[sample_idx,]
train <- df[-sample_idx,]
ct <- rpart(Preiskategorie~Alter+Abschluss+Geschlecht, train, method="class")