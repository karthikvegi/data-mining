# example to demonstrate overfitting of a classification tree.  

# first create a random test set with random labels train a decision tree from the data.  Note
# there is no way to make a good classifier for this situation

library(rpart)
n = 1000					# number of samples
d = 5					 	# dimension of samples
X = matrix(rnorm(n*d),nrow=n);			# predictor vectors are random #'s
df = data.frame(X)				# R needs data frames for some operations
c = sample(1:2,n,replace=T)			# the classes for each vector (random)
#pairs(X,pch=c)
fit = rpart(c ~ X,method="class",minbucket=1,cp=0)	
# fit a decision tree with min terminal size 1 an no purity improvement requirement
plot(fit)	      	       		       # look at complex tree we built
pred = predict(fit,df,type="vector")		# test out the classifier on the training data
trainerrors = sum(pred != c)			# great result!!!  (so what)

# no create test data that is statistically identical to original data

Xtest = matrix(rnorm(n*d),nrow=n);
ctest = sample(1:2,n,replace=T)
df = data.frame(Xtest)
pred = predict(fit,df,type="vector")		# get predicted results from model learned above
testerrors = sum(pred != ctest)			# about 1/2 right! (as you should expect)


