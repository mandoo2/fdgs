library(nnet)

df = data.frame( 
  x2 = c(1:6),
  x1 = c(6:1),
  y = factor(c('no', 'no', 'no', 'yes', 'yes', 'yes'))
)
str(df)

model_net = nnet(y ~ ., df, size = 1)

model_net

summary(model_net)

model_net$fitted.values

p <- predict(model_net, df, type = "class")
table(p, df$y)

data(iris)
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training = iris[idx, ]
testing = iris[-idx, ]
nrow(training)
nrow(testing)

model_net_iris1 = nnet(Species ~ ., training, size = 1)
model_net_iris1
model_net_iris3 = nnet(Species ~ ., training, size = 3)
model_net_iris3

summary(model_net_iris1)

summary(model_net_iris3)

table(predict(model_net_iris1, testing, type = "class"), testing$Species)
table(predict(model_net_iris3, testing, type = "class"), testing$Species)

library(neuralnet)

data("iris")
idx = sample(1:nrow(iris), 0.7 * nrow(iris))
training_iris = iris[idx, ]
testing_iris = iris[-idx, ]
dim(training_iris)
dim(testing_iris)

training_iris$Species2[training_iris$Species == 'setosa'] <- 1
training_iris$Species2[training_iris$Species == 'versicolor'] <- 2
training_iris$Species2[training_iris$Species == 'virginica'] <- 3
training_iris$Species <- NULL
head(training_iris)
testing_iris$Species2[testing_iris$Species == 'setosa'] <- 1
testing_iris$Species2[testing_iris$Species == 'versicolor'] <- 2
testing_iris$Species2[testing_iris$Species == 'virginica'] <- 3
testing_iris$Species <- NULL
head(testing_iris)

normal <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

training_nor <- as.data.frame(lapply(training_iris, normal))
summary(training_nor)
testing_nor <- as.data.frame(lapply(testing_iris, normal))
summary(testing_nor)

model_net = neuralnet(Species2 ~ Sepal.Length + Sepal.Width + 
                        Petal.Length + Petal.Width,
                      data = training_nor, hidden = 1)
model_net
plot(model_net)

model_result <- compute(model_net, testing_nor[c(1:4)])
model_result$net.result

cor(model_result$net.result, testing_nor$Species2)

model_net2 = neuralnet(Species2 ~ Sepal.Length + Sepal.Width +
                         Petal.Length + Petal.Width, 
                       data = training_nor, hidden = 2, 
                       algorithm = "backprop", learningrate = 0.01)

model_result <- compute(model_net, testing_nor[c(1:4)])
cor(model_result$net.result, testing_nor$Species2)
