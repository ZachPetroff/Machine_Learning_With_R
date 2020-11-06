tdd <- read.csv('/Users/12194/Downloads/training_diabetes_data.csv', header=FALSE)
vdd <- read.csv('/Users/12194/Downloads/validation_diabetes_data.csv', header=FALSE)

# make sure data frames are correct
head(tdd)
head(vdd)

# shave off extra column and row that was added during saving process
tdd <- tdd[-1,-1]
vdd <- vdd[-1,-1]

# Define a naive bayes classifier that predicts the class of 'test' based on 'data'
naive_bayes_pred <- function(data, test) {
  prob_of_one <- 0
  total <- 0
  for (row in 1:nrow(data)) {
    if (isTRUE(data[row, 1] == test[1] & data[row, 2] == test[2] & data[row, 3] == test[3] & data[row, 4] == test[4] & data[row, 5] == test[5])) {
      total <- total + 1
      if (data[row, 6] == 1) {
        prob_of_one <- prob_of_one + 1
      }
    }
  }
  prob_of_one = prob_of_one / total
  if (isTRUE(prob_of_one > .5)) {
    return (1)
  } else {
    return (2)
  }
}

# Calculate accuracy
accuracy <- 0 
for (row in 1:nrow(vdd)) {
  pred <- naive_bayes_pred(tdd, vdd[row, 1:5])
  if (pred == vdd[row, 6]) {
    accuracy <- accuracy + 1
  }
}
accuracy <- accuracy / nrow(vdd)
print(accuracy)

# accuracy = .89