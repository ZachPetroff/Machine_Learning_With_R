tdd <- read.csv('/Users/12194/Downloads/training_diabetes_data.csv', header=FALSE)
vdd <- read.csv('/Users/12194/Downloads/validation_diabetes_data.csv', header=FALSE)

# make sure data frames are correct
head(tdd)
head(vdd)

# shave off extra column and row that was added during saving process
tdd <- tdd[-1,-1]
vdd <- vdd[-1,-1]

# change type to numeric
for (col in 1:ncol(tdd)) {
  tdd[,col] <- as.numeric(as.factor(tdd[,col]))
  vdd[,col] <- as.numeric(as.factor(vdd[,col]))
}

dist <- function(x, y) {
  d <- 0
  for (i in 1:length(x)) {
    d <- d + abs(x[i] - y[i])  
  }
  return (d)
}

nn_classify <- function(x, n, training_data, class_col) {
  distances = c()
  neighbors = c()
  for (data in 1:nrow(training_data)) {
    d <- 0 
    d <- dist(x,training_data[data,1:class_col-1])
    distances <- append(distances, d)
  }
  for (neighbor in 1:n) {
    mindex <- which.min(distances)
    neighbors <- append(neighbors, training_data[mindex, class_col])
    distances <- distances[-mindex]
  }
  class <- round(sum(neighbors)/n)
  return (class)
}

y <- nn_classify(vdd[5,1:5], 5, tdd, 6)
true_classes <- vdd[,6]

accuracys <- c()

for (j in 1:10) {
  accuracy <- 0
  for (i in 1:nrow(vdd)) {
    prediction <- nn_classify(vdd[i,1:5], j, tdd, 6)
    if (prediction == true_classes[i]) {
      accuracy <- accuracy + 1
    }
  }
  accuracy <- accuracy / nrow(vdd)
  accuracys <- append(accuracys, accuracy)
}
cat(accuracys)
plot(accuracys)

# optimal_n = 5
# accuracy = 64%