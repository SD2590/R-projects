install.packages('rpart')
library(rpart)

#data set is built in
str(kyphosis)
head(kyphosis)

#Build model for Decision Tree
tree <- rpart(Kyphosis ~ .,method='class',data=kyphosis)
printcp(tree)
plot(tree,uniform=T,main='Kyphosis tree')
text(tree,use.n=T,all=T)

#For better visualization
install.packages('rpart.plot')
library(rpart.plot)
prp(tree)


#Random forests improve predictive accuracy by generating a large number of bootstrapped trees
#(based on random samples of variables), classifying a case using each tree in this new "forest",
#and deciding a final predicted outcome by combining the results across all of the trees 
#(an average in regression, a majority vote in classification).

install.packages("randomForestSRC")
library(randomForestSRC)
rf.model <- rfsrc(Kyphosis ~ ., data=kyphosis)
print(rf.model)

