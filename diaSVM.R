# importing Dataset
dataset = read.csv("diabetes.csv")
install.packages("e1071")
#splitting
library(caTools)
set.seed(123)
split = sample.split(dataset$Outcome, SplitRatio = 0.75)
train_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#feature scaling
train_set[, 1:8] = scale(train_set[, 1:8])
test_set[, 1:8] = scale(test_set[, 1:8])

#fitting log reg to the Dataset
library(e1071)

classifier = svm(formula = Outcome ~ .,
                 type = 'C-classification',
                 data = train_set,
                 kernel = 'radial')

y_pred = predict(classifier, newdata = test_set[-9])


#making confusion matrix
cm = table(test_set[, 9],y_pred)
