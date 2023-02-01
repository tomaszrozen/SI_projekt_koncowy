library(ggplot2)
library(class)
library(dplyr)
penguins <- read.csv("penguins.csv", header=TRUE,
                           stringsAsFactors=TRUE)
data <- penguins[complete.cases(penguins),]
data <- select(data, -c(island, sex, year))
# funkcja wyliczajaca jakość klasyfikatora
acc=function(x)
{sum(diag(x))/sum(x)}

q = c()
for (i in 1:10){
  idx = sample(1:nrow(data),0.8*nrow(data))
  cl = data$species 
  clTrain = cl[idx]
  clTest = cl[-idx]
  train = data[idx,]
  test = data[-idx,] 
  model = knn(train[,-1], test[,-1],cl=clTrain,k=5) 
  #tworzenie confusionMatrix
  tab=table(model,clTest)
  q = append(q, acc(tab))
}
tab
mean(q) # całkowita jakość klasyfikatora
sd(q) # odchylenie standardowe

