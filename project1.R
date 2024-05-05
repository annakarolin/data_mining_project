credit_data <- read.csv("\\fraud_test.csv")
credit_data

convert_to_numeric <- function(x) {
  if (!is.numeric(x)) {
    as.numeric(as.factor(x))
  } else {
    x
  }
}


numeric_data <- lapply(credit_data, convert_to_numeric)
numeric_data <- as.data.frame(numeric_data)
print(numeric_data)

library(corrplot)
numeric_data <- numeric_data[sapply(numeric_data, is.numeric)]
correlation_matrix <- cor(numeric_data)
corrplot(correlation_matrix, method = "color")

library(factoextra)

pca_result <- prcomp(numeric_data, scale. = TRUE)
summary(pca_result)
pca_2d <- pca_result$x[, 1:2]
fviz_eig(pca_result, addlabels = TRUE)
fviz_pca_biplot(pca_result, label = "var")

pca <- cbind(pca_2d, numeric_data$is_fraud)


numeric_data$is_fraud <- factor(numeric_data$is_fraud)


set.seed(123)
class1_data <- numeric_data[numeric_data$is_fraud == 0, ]
class2_data <- numeric_data[numeric_data$is_fraud == 1, ]

n_class1 <- round(0.3 * nrow(class1_data))
n_class2 <- round(0.7 * nrow(class2_data))

sample_class1 <- class1_data[sample(nrow(class1_data), n_class1), ]
sample_class2 <- class2_data[sample(nrow(class2_data), n_class2), ]
sample_data <- rbind(sample_class1, sample_class2)

library(caret)
index <- createDataPartition(sample_data$is_fraud, p = 0.8, list = FALSE)

train_data <- sample_data[index, ]
test_data <- sample_data[-index, ]

logistic_model <- glm(is_fraud ~ ., data = train_data, family = "binomial")

probabilities <- predict(logistic_model, newdata = test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)

confusion_matrix <- table(predictions, test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))


tp <- confusion_matrix[2, 2] 
fp <- confusion_matrix[1, 2] 
fn <- confusion_matrix[2, 1] 
precision <- tp / (tp + fp)
print(precision)
recall <- tp / (tp + fn)
print(recall)
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
f2_score <- 5 * precision * recall / ((4*precision )+ recall)
print(f2_score)
f3_score <- 10 * precision * recall / ((9*precision )+ recall)
print(f3_score)


library(rpart)
tree_model <- rpart(is_fraud ~ ., data = train_data, method = "class")

predictions <- predict(tree_model, newdata = test_data, type = "class")

confusion_matrix <- table(predictions, test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

plot(tree_model)
text(tree_model)
predictions <- factor(predictions)

tp <- confusion_matrix[2, 2] 
fp <- confusion_matrix[1, 2] 
fn <- confusion_matrix[2, 1] 
precision <- tp / (tp + fp)
print(precision)
recall <- tp / (tp + fn)
print(recall)
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
f2_score <- 5 * precision * recall / ((4*precision )+ recall)
print(f2_score)
f3_score <- 10 * precision * recall / ((9*precision )+ recall)
print(f3_score)


library(randomForest)
rf_model <- randomForest(is_fraud ~ ., data = train_data, ntree = 50)

predictions <- predict(rf_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

tp <- confusion_matrix[2, 2] 
fp <- confusion_matrix[1, 2] 
fn <- confusion_matrix[2, 1] 
precision <- tp / (tp + fp)
print(precision)
recall <- tp / (tp + fn)
print(recall)
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
f2_score <- 5 * precision * recall / ((4*precision )+ recall)
print(f2_score)
f3_score <- 10 * precision * recall / ((9*precision )+ recall)
print(f3_score)

rf_model <- randomForest(is_fraud ~ ., data = train_data, ntree = 100)

predictions <- predict(rf_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

tp <- confusion_matrix[2, 2] 
fp <- confusion_matrix[1, 2] 
fn <- confusion_matrix[2, 1] 
precision <- tp / (tp + fp)
print(precision)
recall <- tp / (tp + fn)
print(recall)
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
f2_score <- 5 * precision * recall / ((4*precision )+ recall)
print(f2_score)
f3_score <- 10 * precision * recall / ((9*precision )+ recall)
print(f3_score)

library(e1071)

svm_model <- svm(is_fraud ~ ., data = train_data, kernel = "linear")
predictions <- predict(svm_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$is_fraud)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

tp <- confusion_matrix[2, 2] 
fp <- confusion_matrix[1, 2] 
fn <- confusion_matrix[2, 1] 
precision <- tp / (tp + fp)
print(precision)
recall <- tp / (tp + fn)
print(recall)
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
f2_score <- 5 * precision * recall / ((4*precision )+ recall)
print(f2_score)
f3_score <- 10 * precision * recall / ((9*precision )+ recall)
print(f3_score)
