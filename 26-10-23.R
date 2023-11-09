#####################################################
# Applied Machine Learning
# W05 Tree-based Methods
# Gwendolin Wilke
# gwendolin.wilke@hslu.ch
#####################################################


library(tree)
library(ISLR)
library(ggplot2)


##############   Regression trees   ##########################


# load and inspect the Hitters data set
data("Hitters")
?Hitters # The dataset records and salaries for baseball players depending on their number of hits etc.
head(Hitters)

# remove incomplte observations 
sum(is.na(Hitters))
Hitters <- na.omit(Hitters)

# Salary is our target variable
# We log-transform it so that its distribution has more of a typical bell-shape.
# We need to rmember later on that Salary is logarithmized!
hist(Hitters$Salary) # Salary is measured in thousands of dollars
Hitters$Salary <- log(Hitters$Salary)
hist(Hitters$Salary)

attach(Hitters)

# sample 70% of the row indices for subsetting as training data
set.seed (1)
trainHit <- sample(1:nrow(Hitters), 0.7*nrow(Hitters))
Hitters.train <- Hitters[trainHit,]
Hitters.test <- Hitters[-trainHit,]

#### Fit a regression tree to predict Salary from *Years and Hits*. 
tree.salaryHitters <- tree(Salary ~ Years + Hits, data = Hitters) 
# tree() uses binary recursive partitioning.
# The split which maximizes the reduction in impurity is chosen, 
# the data set is then split and the process repeated. 
# Splitting continues until the terminal nodes are too small or too few to be split
# Tree growth is limited to a depth of 31 by the use of integers to label nodes.

summary(tree.salaryHitters)
# Output:
#   - There are 8 terminal nodes (leaves) of the tree.
#   - Here “residual mean deviance” is just mean squared error (RMS)

# Plot the regression tree 
plot(tree.salaryHitters)
text(tree.salaryHitters, cex=0.75) # cex: set character size to 0.75

# Plot the corresponding regions

# simple plot:
plot(Hitters$Years,Hitters$Hits,col='steelblue', pch=20, xlab="Years",ylab="Hits")
partition.tree(tree.salaryHitters,ordvars=c("Years","Hits"),add=TRUE,cex=1)

# plot with salary value in color code:
# prepare Salary data for plot 
salary.deciles = quantile(Salary,0:10/10)
cut.salary = cut(Hitters$Salary,salary.deciles,include.lowest=TRUE)
# plot the point cloud and regions
plot(Years,Hits,col=grey(10:2/11)[cut.salary],pch=20,
     xlab="Years",ylab="Hits")
partition.tree(tree.salaryHitters,ordvars=c("Years","Hits"),add=TRUE,cex=1)

tree.salaryHitters
# node): node number
# split: split criterion, e.g. Thal: normal, or Ca < 0.5
# n: number of observations in that branch
# deviance (the smaller the better)
# yval: overall prediction for the branch (mean value or Yes or No)
# (ybrob): the fraction of observations in that branch that take on values of (Yes No)
# * denotes terminal node

#### Fit a regression tree to predict Salary from *all other variables*.  
tree.salaryHitters <- tree(Salary ~ .-Salary, data = Hitters) # use all variables excep Salary
summary(tree.salaryHitters)

# Plot the regression tree 
plot(tree.salaryHitters)
text(tree.salaryHitters, cex=0.75) # cex: set character size to 0.75

# use the tree to make predictions on the *test set*
tree.salaryHitters.pred <- predict(tree.salaryHitters, newdata = Hitters.test)

# compare predictions of regression tree with true values (visually)
# We plot the predictions against ground truth. 
# A perfect prediction would give a line with intercept 0 and slope 1.
salaryHitters.test <- Hitters.test$Salary
plot(tree.salaryHitters.pred,salaryHitters.test)
abline (0 ,1) # compare with the function f(x)=x (intercept 0, slope 1)

# calculate the mean squared error on *test data*
salaryHitters.test <- Hitters[-trainHit ,"Salary"]
(tree.salaryHitters.MSE <- mean((tree.salaryHitters.pred - salaryHitters.test)^2)) # MSE = 0.146
sqrt(tree.salaryHitters.MSE) 
# square root of the MSE = 0.383
# --> test predictions are within around $383 of the true median logarithmized salary.

#####  Cost-Complexity Pruning
#  Goal: prune the trees to avoid high variance and overfitting. 
#  Positive effects: 
#     - smaller *test* errors (due to less overfitting).
#     - higher interpretability (due to smaller trees).


# use cross-validation to find the optimal parameter \alpha for cost-complexity pruning  
set.seed (3)
?cv.tree
cv.Hitters =cv.tree(tree.salaryHitters)
# Runs a K-fold cross-validation experiment to find the number of 
# misclassifications as a function of the cost-complexity parameter \alpha.

cv.Hitters
# $k: cost-complexity parameter (corresponds to \alpha)
# Notice that \alpha is increasing (corresponding to the pruning sequence).
# $size: number of terminal nodes of each tree
# Notice that the size is decreasing (corresponding to the pruning sequence).
# $dev: *cross-validation* error rate
# The full tree (with size 9, i.e. 9 terminal nodes) has lowest cross-validation error.

# plot the cross-validation error-rate as a function of both size and \alpha (k):
par(mfrow=c(1,2))
plot(cv.Hitters$size, cv.Hitters$dev, type="b") # type="b": plot both, points and lines
plot(cv.Hitters$k, cv.Hitters$dev, type="b")
par(mfrow=c(1,1))

# we do not need to prune the tree, since the full tree has minimal cross-validation error
# yet, to show how to prune, here's the code: 
prune.salaryHitters <- prune.tree(tree.salaryHitters, best=6) 
# prune.tree determines the nested cost-complexity sequence  
# best=6: get the 6-node tree in the cost-complexity sequence 

# Plot the pruned regression tree 
plot(prune.salaryHitters)
text(prune.salaryHitters, cex=0.75) # cex: set character size to 0.75

##############   Classification trees   ##########################

# load and inspect the Heart data set
# These data contain a binary outcome HD for 303 patients who presented with chest pain. 
# An outcome value of Yes indicates the presence of heart disease based on an angiographic test, 
# while No means no heart disease. There are 13 predic- tors including Age, Sex, Chol 
# (a cholesterol measurement), and other heart and lung function measurements. 
Heart <- read.csv("./Heart.csv")
attach(Heart)
head(Heart) # AHD = Yes means Heart Disease
Heart <- Heart[,-1] # Remove the row identifier (we dont use it as a predictor)

# the categorical variables need to be of type factor rather than character to work with tree()
str(Heart)
Heart$AHD = as.factor(Heart$AHD) 
Heart$ChestPain = as.factor(Heart$ChestPain) 
Heart$Thal = as.factor(Heart$Thal) 
plot(Heart$AHD)

# sample 70% of the row indices for subsetting as training data
set.seed (2)
trainHeart <- sample(1:nrow(Heart), 0.7*nrow(Heart))
Heart.train <- Heart[trainHeart,]
Heart.test <- Heart[-trainHeart,]

# train classification tree on *training data*
tree.AHDHeart <- tree(AHD ~ .-AHD, data = Heart.train)

plot(tree.AHDHeart)
text(tree.AHDHeart, cex=0.75, pretty=0)
# cex: set character size to 0.75
# pretty=0 instructs R to include the category names for any qualitative pre- dictors, rather than simply displaying a letter for each category.
# Ca is the most important indicator for Heart Disease.

summary(tree.AHDHeart)
# "Deviance": 
# For classification trees this is a scaled version of the entopy,
# measured as -2 * \left( \sum_{m}\sum{k} n_{mk} * log(p_{mk}) \right).
# Here, n_{mk} is the number of observations in the mth terminal node that belong to the kth class.
# A small deviance indicates a tree that provides a good fit to the (training) data.
# "Residual mean deviance":
# Deviance devided by (n - |T_{0}|)
# "Misclassification error rate": 
# training error rate is 8.5%.


tree.AHDHeart
# node): node number
# split: split criterion, e.g. Thal: normal, or Ca < 0.5
# n: number of observations in that branch
# deviance (the smaller the better)
# yval: overall prediction for the branch (Yes or No)
# (ybrob): the fraction of observations in that branch that take on values of (Yes No)
# * denotes terminal node

# use classification tree to predict *test data*
tree.AHDHeart.pred <- predict(tree.AHDHeart, Heart.test, type="class")

# confusion table to determine classification error on test data
(tree.AHDHeart.pred.ct <- table(tree.AHDHeart.pred, Heart.test$AHD))
(tree.AHDHeart.correct <- (tree.AHDHeart.pred.ct[1,1] + tree.AHDHeart.pred.ct[2,2])/sum(tree.AHDHeart.pred.ct)) # portion of correctly classified observations: 70.3%
(tree.AHD.testError <- 1 - tree.AHDHeart.correct) # test error


#####  Cost-Complexity Pruning
#  Goal: prune trees to avoid high variance and overfitting. 
#  Positive effects: 
#     - smaller *test* errors (due to less overfitting).
#     - higher interpretability (due to smaller trees).

# use cross-validation to find the optimal parameter \alpha for cost-complexity pruning  
set.seed (3)
cv.Heart =cv.tree(tree.AHDHeart, FUN = prune.misclass)
# Runs a K-fold cross-validation experiment to find the number of 
# misclassifications as a function of the cost-complexity parameter \alpha.
# "FUN = prune.misclass":
# The *classification error rate* should guide the 
# cross-validation and pruning process (as opposed to Gini index or entropy).
# If FUN is not specified, deviance is used as default (which is a version of entropy). 
# --> Remeber: If prediction accuracy is the goal, the error rate is 
# preferrable for pruning .

cv.Heart
# $k: cost-complexity parameter (corresponds to \alpha)
# Notice that \alpha is increasing (corresponding to the pruning sequence).
# \alpha=0.75 gives the lowest cross-validation error.
# $size: number of terminal nodes of each tree
# Notice that the size is decreasing (corresponding to the pruning sequence).
# $dev: *cross-validation* error rate
# The tree with size 8 (8 terminal nodes) has lowest cross-validation error.

# plot the cross-validation error-rate as a function of both size and \alpha (k):
par(mfrow=c(1,2))
plot(cv.Heart$size, cv.Heart$dev, type="b") # type="b": plot both, points and lines
plot(cv.Heart$k, cv.Heart$dev, type="b")
par(mfrow=c(1,1))

# do the actual pruning
prune.AHDHeart <- prune.misclass(tree.AHDHeart, best=7) 
# prune.misclass: 
# is an abbreviation for prune.tree(method = "misclass") for use with cv.tree.
# Here, prune.tree determines the nested cost-complexity sequence  
# best=7: get the 7-node tree in the cost-complexity sequence 

# plot the pruned tree
plot(prune.AHDHeart)
text(prune.AHDHeart,pretty=0)

# use pruned tree to predict *test data*
prune.AHDHeart.pred <- predict(prune.AHDHeart, Heart.test, type="class")

# confusion table to determine classification error on *test data*
(prune.AHDHeart.pred.ct <- table(prune.AHDHeart.pred, Heart.test$AHD))
(prune.AHDHeart.correct <- (prune.AHDHeart.pred.ct[1,1] + prune.AHDHeart.pred.ct[2,2])/sum(prune.AHDHeart.pred.ct)) # portion of correctly classified observations
(prune.AHDHeart.testError <- 1 - prune.AHDHeart.correct) # test error (pruned) 

# compare with *test error* of unpruned tree:
tree.AHD.testError # smaller error with pruning!


##############   Bagging ##########################

# We apply Bagging and Random Forests on Boston data set
# Here we apply bagging and random forests to the Boston data, using the randomForest package in R.
# The exact results obtained in this section may depend on the version of R and the version of the randomForest package installed on your computer.
# Recall that bagging is just a special case of random forests with m = p.

library(MASS)
library(randomForest)

# We will use the Boston data set included in the MASS library.
# It records Housing Values in Suburbs of Boston
?Boston # read the data set description

# Setup a training and test set for the Boston data set.
set.seed (1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test=Boston[-train ,"medv"]

# Apply bagging using the randomForest package in R. 
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,importance =TRUE)
# mtry = 13 means that we should use all 13 predictors for each split of the tree, 
# hence, do bagging.

# How well does the bagged model perform on the test set? 
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
# The test set MSE associated with the bagged regression tree is 13.16, 
# That's almost half that obtained using an optimally-pruned single tree 
# (investigate this on your own).

# Exercise: Change the number of trees grown by randomForest() 
# using the ntree argument. 
# For example, what happens to the MSE when we grow the tree from 13 to 25?

##############   Random Forests   ##########################

# Growing a random forest proceeds in exactly the same way, 
#     except that we use a smaller value of the mtry argument. 
# By default, randomForest() uses $p/3$ variables when building 
#     a random forest of regression trees, and $\sqrt{p}$ variables 
#     when building a random forest of classification trees.

# Building a random forest on the same data set using mtry = 6. 
# Comment on the difference from the test MSE from using the random 
# forest compared to bagging.

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train, mtry=6,importance =TRUE)
yhat.rf = predict(rf.boston ,newdata=Boston[-train ,])
mean((yhat.rf-boston.test)^2)
# We see that the test MSE for a random forest is 11.48; 
# this indicates that random forests yielded an improvement over bagging 
# in this case (versus 13.34)

# Investigating variable importance 
importance(rf.boston)
# Two measures of variable importance are reported:
# 1) The first is based upon the mean *decrease of accuracy*
# in predictions on the out of bag samples when a given variable 
# is excluded from the model.
# 2) THe second is a measure of the total *decrease in node impurity* 
# that results from splits over that variable, averaged over all trees.
varImpPlot (rf.boston)
# The results indicate that across all of the trees considered in the 
# random forest, the wealth level of the community (lstat) and the house size (rm) 
# are by far the two most important variables for median house prices (which makes sense).

##############   Boosting   ##########################

library(gbm)
# We use the gbmpackage, and within it the gbm() function, 
# to fit boosted regression trees to the Boston data set.

# Perform boosting on the training data set, treating this as a regression problem. 
set.seed (1)
boost.boston=gbm(medv~.,data=Boston[train,],
                 distribution="gaussian",n.trees=5000, interaction.depth=4)
# We run gbm() with the option distribution="gaussian" since this 
#     is a regression problem.
# If it were a binary classification problem, 
#     we would use distribution="bernoulli".
# "interaction.depth" refers to the maximum depth of variable interactions. 
#     1 implies an additive model, 
#     2 implies a model with up to 2-way interactions, etc.

summary(boost.boston)
# We see that lstat and rm are by far the most important variables (again).
# If you dont see lstst in your graph, make the plot window higher :-)

# Producing partial dependence plots for these two variables (lstat and rm). 
par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm") 
plot(boost.boston ,i="lstat")
# These plots illustrate the marginal effect of the selected variables 
# on the response after integrating out the other variables. 
# I.e., for every value of the selected variable, we calculate the predicted response 
# for every combination of values of the other variables. We then average ("integrate out") 
# over all these predicted responses. We do that for each value of teh selected variable, 
# which gives the graph.
# As we might expect, median house prices are increasing with rm and decreasing 
# with lstat.

# Now use the boosted model to predict medv on the test set. Report the MSE.
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
# The test MSE obtained is 11.8; similar to the test MSE for random 
# forests and superior to that for bagging.

# What happens if we vary the shrinkage parameter from its 
# default of 0.001 to 0.02? Report the test MSE.
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000, interaction.depth=4,shrinkage = 0.2, verbose = F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost -boston.test)^2)
# In this case, using $\lambda = 0.2$ leads to a slightly 
# lower test MSE than $\lambda = 0.001.$

par(mfrow=c(1,1))