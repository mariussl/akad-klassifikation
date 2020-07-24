library(data.tree)
#df <- read.csv("testdata.csv")
#df <- transform(df, Streckenverlauf = as.factor(Streckenverlauf), Streckenqualitaet = as.factor(Streckenqualitaet), Wind = (Wind == "Ja"), Startzeit = as.factor(Startzeit), Sieg = (Sieg == "Ja"))
df <- read.csv("data.csv")
df <- transform(df, Bundesland = as.factor(Bundesland),
                Abschluss = as.factor(Abschluss), Stellung = as.factor(Stellung),
                Geschlecht = as.factor(Geschlecht), Wohngebiet = as.factor(Wohngebiet),
                Eigenheim = (Eigenheim == "j"), Familienstand = as.factor(Familienstand),
                MitgliedSportverein = (MitgliedSportverein == "j"), 
                Preiskategorie = as.factor(Preiskategorie))
df$Kinder <- as.logical(lapply(df$Kinderanzahl, function(x) { x > 0}))
breaks <- c(10000,30000,52000,70000,500000)
tags <- c("niedrig", "mittel", "hoch", "sehr hoch")
df$EinkommensKlasse <- cut(df$Einkommen, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)
breaks <- c(0, 3, 6, 8, 20)
tags <- c("0-3", "3-6", "6-8", "8-20")
df$KrankheitsKlasse <- cut(df$Krankheitstage, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)
breaks <- c(-Inf, 0, 1, 2, Inf)
tags <- c("keine", "eines", "zwei", "2+")
df$KinderanzahlKlasse <- cut(df$Kinderanzahl, breaks = breaks, include.lowest = TRUE, right = TRUE, labels = tags)
breaks = c(20, 35, Inf)
tags = c("20-35", "35+")
df$AltersKlasse <- cut(df$Alter, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)
set.seed(12)
sample_idx <- sample.int(nrow(df), round(nrow(df)*0.2))
test <- df[sample_idx,]
train <- df[-sample_idx,]
calcInfoGain <- function(target, source) {
  targetCount <- length(target)
  probs <- table(target)/targetCount
  baseBits <- (-sum(probs * log2(probs)))
  dep <- table(target, source)
  dep_freq <- apply(dep,2,function(x){x/sum(x)})
  dep_entropy <- apply(dep_freq, 2,function(x){ ifelse(x!=0, x * log2(x), 0)})
  depSums <- rowSums(dep_entropy, na.rm = TRUE) * -1
  weightedDepSum <- sum(depSums * probs)
  return (baseBits - weightedDepSum)
}
calcEntropy <- function(ctable) {
  ctable_freq <- apply(ctable,2,function(x){x/sum(x)})
  ctable_entropy <- apply(ctable_freq, 2,function(x){ ifelse(x!=0, x * log2(x), 0)})
  entropy_sum <- sum(colSums(ctable_entropy, na.rm = TRUE) * -1)
  return (entropy_sum)
}
isHomogen <- function(v) {
  return (length(v) - length(v[v == 0]) == 1) 
}
getFrequDist <- function(t) {
  return (t / sum(t))
}
getErrorCount <- function(v) {
  return (sum(v[v < max(v)]))
}
getTotalCount <- function(v) {
  return (sum(v))
}
idx <- 1
baseName <- "Knoten_%i"
getNodeId <- function() {
  id <- sprintf(baseName, idx)
  idx <<- idx + 1
  return (id)
}
buildTree <- function(df, targetProp, classProps) {
  nodeId <- getNodeId()
  thisNode <- Node$new(nodeId)
  thisNode$id <- nodeId
  thisNode$data <- df
  thisNode$targetFrequDist <- getFrequDist(table(df[targetProp]))
  thisNode$label <- sprintf("%s|%s", thisNode$id, paste(sprintf("%.2f", thisNode$targetFrequDist), sep = "", collapse = " "))
  thisNode$error <- getErrorCount(table(df[targetProp]))
  thisNode$total <- getTotalCount(table(df[targetProp]))
  thisNode$predClass <- names(which(table(df[targetProp]) == max(table(df[targetProp]))))[1]
  thisNode$label <- sprintf("%s|Vorhersage %s|Total_Richtig_Falsch %d_%d_%d", 
                            thisNode$label, thisNode$predClass, 
                            thisNode$total, (thisNode$total - thisNode$error), 
                            thisNode$error)
  if ((length(classProps) == 0) || isHomogen(table(df[targetProp]))) {
    SetNodeStyle(thisNode, shape = "record", label=sprintf("{%s}", thisNode$label ))
    return (thisNode)
  }
  classPropsEntropy <- data.frame(matrix(ncol = length(classProps), nrow = 1))
  colnames(classPropsEntropy) <- classProps
  for (classProp in classProps) {
    #print(sprintf("starting entropy calc for %s", classProp))
    ctable <- table(df[, targetProp], df[, classProp])
    classPropsEntropy[classProp] <- calcEntropy(ctable)
  }
  useClassProp <- colnames(classPropsEntropy)[which(classPropsEntropy==min(classPropsEntropy))][1]
  # classPropValues only contains factor levels present in the training data
  # thus levels could be missing that will be present in the test data
  # this is not changeable since the entropy of this property was calculated
  # based on the occurence of the values
  classPropValues <- unique(df[, useClassProp])
  classPropValueToNodeId <- data.frame(matrix(ncol = length(classPropValues), nrow = 1))
  colnames(classPropValueToNodeId) <- classPropValues
  thisNode$useClassProp = useClassProp
  thisNode$label <- sprintf("%s|div by %s", thisNode$label, useClassProp)
  newClassProps <- classProps[!(classProps %in% useClassProp)]
  for (classPropValue in classPropValues) {
    childNode <- buildTree(df[df[useClassProp] == classPropValue, ], targetProp, newClassProps)
    classPropValueToNodeId[, as.character(classPropValue)] <- childNode$id
    thisNode$AddChildNode(childNode)
  }
  thisNode$classPropValueToNodeId = classPropValueToNodeId
  SetNodeStyle(thisNode, shape = "record", label=sprintf("{%s}", thisNode$label ))
  return (thisNode)
}
pruneTree <- function(tree) {
  if (tree$isLeaf) {
    return (tree)
  }
  subtreeError <- 0
  for (childNode in tree$children) {
    pruneTree(childNode)
    subtreeError <- subtreeError + childNode$error
  }
  if (tree$error <= subtreeError) {
    for (childNode in tree$children) {
      tree$RemoveChild(childNode$id)
    }
  } else {
    # replace error count with that from subtree, else tree is replaced by parent node 
    # by looking at incorrect performance
    tree$error <- subtreeError
  }
  return (tree)
}
classifyByTree <- function(datarow, tree) {
  if (tree$isLeaf) {
    return (tree$predClass)
  }
  classPropValues <- colnames(tree$classPropValueToNodeId)
  for (classPropValue in classPropValues) {
    if (datarow[tree$useClassProp] == classPropValue) {
      childNode <- FindNode(tree, as.character(tree$classPropValueToNodeId[classPropValue]))
      return (classifyByTree(datarow, childNode))
    } 
  }
  # the present classPropValue was not trained in the data set
  # so return a this node
  return (tree$predClass)
}
calcGini <- function() {
  
}
classifyDataFrame <- function(row) {
  return (classifyByTree(row, ptree))
}
#tree <- buildTree(df, "Sieg", c("Streckenverlauf", "Streckenqualitaet", "Wind", "Startzeit"))
tree <- buildTree(train, "Preiskategorie", c("EinkommensKlasse", "Abschluss", 
                                             "Geschlecht" , "Wohngebiet" , "Stellung", 
                                             "Eigenheim", "Familienstand", "Bundesland", 
                                             "MitgliedSportverein", "KrankheitsKlasse", 
                                             "KinderanzahlKlasse", "AltersKlasse"))
ptree <- pruneTree(Clone(tree))
print(sprintf("Der Baum hat %d Knoten", ptree$totalCount))
result <- cbind(test, Vorhersage = apply(test, 1, classifyDataFrame))
confMatrix <- (table(result$Vorhersage, result$Preiskategorie, dnn = c("Vorhersage", "Preiskategorie")) / nrow(result))
print("Konfusionsmatrix der Vorhersage-Ergebnisse")
print(format(confMatrix * 100, trim = TRUE, digits = 2))
predSuccess <- sum(diag(confMatrix))
predError <- 1 - predSuccess
print(sprintf("Von %d Vorhersagen sind %1.2f%% richtig und %1.2f%% falsch.", 
              nrow(result), predSuccess * 100, predError * 100))
falsePredictedData <- subset(result, result$Vorhersage != result$Preiskategorie)
print(falsePredictedData)
#print(ptree, "label")
#plot(tree)