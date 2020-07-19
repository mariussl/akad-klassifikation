library(data.tree)
#df <- read.csv("testdata.csv")
#df <- transform(df, Streckenverlauf = as.factor(Streckenverlauf), Streckenqualitaet = as.factor(Streckenqualitaet), Wind = (Wind == "Ja"), Startzeit = as.factor(Startzeit), Sieg = (Sieg == "Ja"))
df <- read.csv("data.csv")
df <- transform(df, Bundesland = as.factor(Bundesland), Abschluss = as.factor(Abschluss), Stellung = as.factor(Stellung), Geschlecht = as.factor(Geschlecht), Wohngebiet = as.factor(Wohngebiet), Eigenheim = (Eigenheim == "j"), Familienstand = as.factor(Familienstand), MitgliedSportverein = (MitgliedSportverein == "j"), Preiskategorie = as.factor(Preiskategorie))
set.seed(1765)
sample_idx <- sample.int(nrow(df), round(nrow(df)*0.2))
test <- df[sample_idx,]
train <- df[-sample_idx,]
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
idx <- 0
baseName <- "Knoten_%i"
getNodeId <- function() {
  id <- sprintf(baseName, idx)
  idx <<- idx + 1
  return (id)
}
buildTree <- function(df, targetProp, classProps) {
  nodeId <- getNodeId()
  if (nodeId == "Knoten_9") {
    print("Ml")
  }
  thisNode <- Node$new(nodeId)
  thisNode$id <- nodeId
  thisNode$data <- df
  thisNode$targetFrequDist <- getFrequDist(table(df[targetProp]))
  thisNode$label <- sprintf("%s|%s", thisNode$id, paste(sprintf("%.2f", thisNode$targetFrequDist), sep = "", collapse = " "))
  thisNode$error <- getErrorCount(table(df[targetProp]))
  thisNode$total <- getTotalCount(table(df[targetProp]))
  thisNode$label <- sprintf("%s|Vorhersage %s|Total_Richtig_Falsch %d_%d_%d", 
                            thisNode$label, names(which(table(df[targetProp]) == max(table(df[targetProp]))))[1], 
                            thisNode$total, (thisNode$total - thisNode$error), 
                            thisNode$error)
  if ((length(classProps) == 0) || isHomogen(table(df[targetProp]))) {
    SetNodeStyle(thisNode, shape = "record", label=sprintf("{%s}", thisNode$label ))
    return (thisNode)
  }
  classPropsEntropy <- data.frame(matrix(ncol = length(classProps), nrow = 1))
  colnames(classPropsEntropy) <- classProps
  for (classProp in classProps) {
    print(sprintf("starting entropy calc for %s", classProp))
    ctable <- table(df[, targetProp], df[, classProp])
    classPropsEntropy[classProp] <- calcEntropy(ctable)
  }
  useClassProp <- colnames(classPropsEntropy)[which(classPropsEntropy==min(classPropsEntropy))][1]
  thisNode$label <- sprintf("%s|div by %s", thisNode$label, useClassProp)
  newClassProps <- classProps[!(classProps %in% useClassProp)]
  for (classPropValue in unique(df[, useClassProp])) {
    childNode <- buildTree(df[df[useClassProp] == classPropValue, ], targetProp, newClassProps)
    thisNode$AddChildNode(childNode)
  }
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
  if (subtreeError >= tree$error) {
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
#tree <- buildTree(df, "Sieg", c("Streckenverlauf", "Streckenqualitaet", "Wind", "Startzeit"))
tree <- buildTree(train, "Preiskategorie", c("Abschluss", "Geschlecht" , "Wohngebiet" , "Stellung", 
                  "Eigenheim", "Familienstand","Bundesland", "MitgliedSportverein"))
#tree <- buildTree(train, "Preiskategorie", c("Bundesland", "MitgliedSportverein"))
ptree <- pruneTree(Clone(tree))
#print(ptree, "label")
#plot(tree)