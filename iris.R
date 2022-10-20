#install packages
install.packages("tree")
install.packages("caret")

#activate packages
library(tree)
library(caret)

#import data

data(package='datasets')
data("iris")
names(iris)
str(iris)
summary(iris)
View(iris)
head(iris,10)
tail(iris,5)

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
plot(iris$Sepal.Length, iris$Sepal.Width)
qplot(Sepal.Length, Sepal.Width, data= iris, colour= Species)
dim(iris) # have 150 record observations 

#preprocessing
iris <- na.omit(iris)

#create training & testing data sets
set.seed(123)
train.index <-sample(1:nrow(iris),.8*nrow(iris)) # row 1-120 randomly picked

train.set <- iris[train.index,] #inductive
test.set <- iris[-train.index,] #exluded observations will test how accuratly new classes (deductively)
Species.test <- iris$Species[-train.index]

#train classification
dt <- tree(Species~., train.set)
dt
plot(dt)
text(dt)
summary(dt) #looking for size of terminal nodes & misclassification error

#test train tree
dt.test <- predict(dt, test.set, type= 'class')
table(dt.test, Species.test)

#calc model accuracy
(10+14+5)/30

#cross validation
cv.tree(dt,FUN = prune.misclass)
dt.pruned <- prune.misclass(dt,best= 5)
dt.pruned
plot(dt.pruned)
text(dt.pruned)
summary(dt.pruned)

#test pruned tree
dt.pruned.test <- predict(dt.pruned,test.set,type= 'class')
table(Species.test, dt.pruned.test)
#calc model accuracy
(10+14+5)/30

#overall: pruned tree is better, because tree is simpler