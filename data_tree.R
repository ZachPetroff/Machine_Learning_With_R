library(rpart)
library(rpart.plot)
tdd <- read.csv('/Users/12194/Downloads/training_diabetes_data.csv', header=FALSE)
vdd <- read.csv('/Users/12194/Downloads/validation_diabetes_data.csv', header=FALSE)

# make sure data frames are correct
head(tdd)
head(vdd)

# shave off extra column and row that was added during saving process
tdd <- tdd[-1,-1]
tdd <- tdd[-1,]
vdd <- vdd[-1,-1]
vdd <- vdd[-1,]

# rename columns
dimnames(tdd)[[2]] <- list("Gender", "Polyuria", "Polydipsia", "sudden.weight.loss", "partial.paresis", "class")
dimnames(vdd)[[2]] <- list("Gender", "Polyuria", "Polydipsia", "sudden.weight.loss", "partial.paresis", "class")

tree <- rpart(class~Gender+Polyuria+Polydipsia+sudden.weight.loss+partial.paresis,
              data=tdd,
              method = "class", 
              parms=list(split="information"))
rpart.plot(tree, nn=TRUE)
pred <- predict(object=tree,vdd,type="class")
pred[1]
accuracy <- 0

for (prediction in 1:length(pred)) {
  if (pred[prediction] == true_classes[prediction]) {
    accuracy <- accuracy + 1
  }
}
accuracy <- accuracy / length(pred)
accuracy
printcp(tree)
# xerror is lowest when no pruning takes place
# accuracy = 74%