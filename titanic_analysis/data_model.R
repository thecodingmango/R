library(pROC)

# Using the processed data as our training data
training_data <- read.csv('data/Processed_training_data.csv')

# Start off with a simple logistic model
logistic_model_1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch, 
                        data=training_data, family = binomial)

# Summary for logistic model 1
summary(logistic_model_1)

# Predicting the survival using the logistic model
test_prediction <- predict(logistic_model_1, training_data, type = 'response')

# ROC values and plotting ROC curve
roc_values <- cbind(roc(training_data$Survived, logistic_model_1$fitted.values)$sensitivities,
      roc(training_data$Survived, logistic_model_1$fitted.values)$threshold)

roc(training_data$Survived, logistic_model_1$fitted.values, 
    legacy.axes = T,
    plot = T,
    percent = T,
    print.auc = T,
    xlab = "False Positive %", 
    ylab = "True Positive %", 
    col = rainbow(1))

# If Threshold is greater than 0.34577114 then, 1, else 0
test_prediction <- ifelse(test_prediction >= 0.4409982, 1, 0)

# Compare the correct number of prediction to the test set
sum(test_prediction == training_data$Survived)/dim(training_data)[1]

# calculating the tp, fp, tn, fn
tp <- sum((test_prediction == 1) & (training_data$Survived == 1))
fp <- sum((test_prediction == 1) & (training_data$Survived == 0))
tn <- sum((test_prediction == 0) & (training_data$Survived == 0))
fn <- sum((test_prediction == 0) & (training_data$Survived == 1))

matrix(c(tp, fn, fp, tn), ncol=2)


