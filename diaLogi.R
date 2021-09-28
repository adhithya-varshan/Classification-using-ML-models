# importing Dataset
dataset = read.csv("diabetes.csv")

#splitting
library(caTools)
set.seed(123)
split = sample.split(dataset$Outcome, SplitRatio = 0.8)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#feature scaling
train_set[, 1:8] = scale(train_set[, 1:8])
test_set[, 1:8] = scale(test_set[, 1:8])

#fitting log reg to the Dataset

classifier = glm(formula = Outcome ~ .,
                 family = binomial,
                 data = train_set)

prob_pred = predict(classifier, type = 'response', newdata = test_set[-9])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#making confusion matrix

cm = table(test_set[, 9],y_pred)




