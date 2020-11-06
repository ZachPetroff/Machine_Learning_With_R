diabetes_data <- read.csv("/Users/12194/Downloads/diabetes_data_upload.csv")
head(diabetes_data)
summary(diabetes_data)

nrows = nrow(diabetes_data)
ncols = ncol(diabetes_data)
cat(nrows, "x", ncols)

set.seed(5)
# adjusting data: changing all to numeric, binning ages (find median and make bins over/under)

# make all columns numeric to make them easier to work with 
for (col in 2:ncols) {
  diabetes_data[,col] <- as.numeric(as.factor(diabetes_data[,col]))
}

# Ages are continuous, so they need to be binned before classification
# I chose 2 bins, because the rest of the data is binary 
median = 47.5

for (age in 1:nrows) {
  if (diabetes_data[age,1] > median) {
    diabetes_data[age,1] = 2
  }
  else {
    diabetes_data[age,1] = 1
  }
}

head(diabetes_data)
summary(diabetes_data)

# picking the best features based on pearson coefficient
cors <- c()
# cors = list of correlations
for (col in 1:ncols-1) {
  c = abs(cor(diabetes_data[,col], diabetes_data$class))
  cat(col,":",c,"\n")
  cors <- append(cors,c)
}

correlations <- sort(cors, decreasing=TRUE)

# picking the 5 strongest correlations
# correlations = list of 5 highest correlations
correlations <- correlations[1:5]
correlations

# getting column indexes of highest correlations
indexes <- c()
for (col in 1:length(cors)) {
  for (cor in 1:length(correlations)) {
    if (cors[col] == correlations[cor]) {
      indexes <- append(indexes, col)
    }
  }
}

# pull indexes out from dataFrame to create new dataFrame
# new dataFrame consists of 5 most important attributes
diabetes_data <- diabetes_data[,c(indexes[1], indexes[2], indexes[3], indexes[4], indexes[5], ncols)]

# split into training data and validation data
# pick random row indexes 
val_pct = .18
val_indxs <- c()
train_indxs <- c()

for (row in 1:nrows) {
  if (runif(1) > .18) {
    train_indxs <- append(train_indxs, row)
  }
  else {
    val_indxs <- append(val_indxs, row)
  }
}

train_diabetes_data <- diabetes_data[train_indxs,]
val_diabetes_data <- diabetes_data[val_indxs,]

train_rows = nrow(train_diabetes_data)
val_rows = nrow(val_diabetes_data)

cat("Size of training data =", train_rows, "\nSize of validation data =", val_rows)

# abbreviate dataFrames
tdd <- train_diabetes_data
vdd <- val_diabetes_data

tdd <- na.omit(tdd)
vdd <- na.omit(vdd)

write.csv(tdd,'/Users/12194/Downloads/training_diabetes_data.csv', row.names=FALSE)
write.csv(vdd,'/Users/12194/Downloads/validation_diabetes_data.csv', row.names=FALSE)
 