# libraries
library(tidyverse)
library(e1071)

# data
data = iris
str(iris)
qplot(x = Petal.Length, y = Petal.Width, data = data, color = Species)

# model
model = svm(Species ~ ., data)
summary(model)
plot(model, data, Petal.Width~Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# prediction
p1 = predict(model, data)
tab = table(p1, data$Species)
tab

# tuning
set.seed(122)
t = tune(svm, Species ~ ., data = data, ranges = list(epsilon = seq(0,1,0.1),
                                                      cost = 2^(2:7)))
plot(t)
summary(t)

# best model
mymodel = t$best.model
plot(mymodel, data, Petal.Width~Petal.Length, 
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

# prediction
p2 = predict(mymodel, data)
tab2 = table(p2, data$Species)
tab2

# misclassification error
1-sum(diag(tab))/sum(tab) #0.02666667
1-sum(diag(tab2))/sum(tab2) #0.01333333
