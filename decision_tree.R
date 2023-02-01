library("party")
library(dplyr)
penguins <- read.csv("penguins.csv", header=TRUE,
                           stringsAsFactors=TRUE)
data <- penguins[complete.cases(penguins),]
data <- select(data, -c(island, sex, year))
acc=function(x)
{sum(diag(x))/sum(x)}
q = c()
for (i in 1:10) {
  idx=sample(1:nrow(data), 0.8*nrow(data))
  train = data[idx,]
  test = data[-idx,]
  model = ctree(species ~ . ,data=train)
  p = predict(model, test)
  tab = table(p, test$species) #confusion matrix
  q = append(q,acc(tab))
}
tab
mean(q) # całkowita jakość klasyfikatora
sd(q) # odchylenie standardowe
