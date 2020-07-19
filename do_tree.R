library(data.tree)
df <- read.csv("testdata.csv")
df <- transform(df, Streckenverlauf = as.factor(Streckenverlauf), Streckenqualitaet = as.factor(Streckenqualitaet), Wind = (Wind == "Ja"), Startzeit = as.factor(Startzeit), Sieg = (Sieg == "Ja"))
calcEntropy <- function(ctable) {
  ctable_freq <- apply(ctable,2,function(x){x/sum(x)})
  ctable_entropy <- apply(ctable_freq, 2,function(x){ ifelse(x!=0, x * log2(x), 0)})
  entropy_sum <- sum(colSums(ctable_entropy) * -1)
  return (entropy_sum)
}
isHomogen <- function(v) {
  return (length(v) - length(v[v == 0]) == 1) 
}
getFrequDist <- function(t) {
  return (t / sum(t))
}
errorCount <- function(v) {
  return (sum(v[v < max(v)]))
}
totalCount <- function(v) {
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
  thisNode <- Node$new(nodeId)
  thisNode$id <- nodeId
  thisNode$data <- df
  thisNode$targetFrequDist <- getFrequDist(table(df[targetProp]))
  label <- sprintf("%s|%s", thisNode$id, paste(sprintf("%.2f", thisNode$targetFrequDist), sep = "", collapse = " "))
  thisNode$label <- label
  if ((length(classProps) == 0) || isHomogen(table(df[targetProp]))) {
    thisNode$label <- sprintf("%s|Vorhersage %s", thisNode$label, names(table(df[targetProp])))
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
  useClassProp <- colnames(classPropsEntropy)[which(classPropsEntropy==min(classPropsEntropy))]
  thisNode$label <- sprintf("%s|div by %s", thisNode$label, useClassProp)
  newClassProps <- classProps[!(classProps %in% useClassProp)]
  for (classPropValue in unique(df[, useClassProp])) {
    childNode <- buildTree(df[df[useClassProp] == classPropValue, ], targetProp, newClassProps)
    thisNode$AddChildNode(childNode)
  }
  SetNodeStyle(thisNode, shape = "record", label=sprintf("{%s}", thisNode$label ))
  return (thisNode)
}
tree <- buildTree(df, "Sieg", c("Streckenverlauf", "Streckenqualitaet", "Wind", "Startzeit"))