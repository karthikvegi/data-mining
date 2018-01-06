# demonstration of computation of finding the "maximum-margin" 
# separating hyperplane of # linearly separable two-class dataset

library("quadprog");  # we do this by quadratic programming and need library

w = rnorm(2);  # choose a random weight vector (unknown to us)
b = rnorm(1);  # choose the b offset (also unknown)
N = 20;       # number of observations
X = matrix(0,nrow = N, ncol = 2);  #  the data matrix of predictors
y = rep(0,N);  # the vector of {-1,1} elements giving classes 
for (i in 1:N) {  # generate the data
  X[i,]  = rnorm(2);  # choose each predictor vector randomly
  y[i] = sign(t(w) %*% X[i,] + b)  # choose data to be linearly separable
  # note that our choice of the class is forced here if we want
  # to be sure the classes are linearly separable.  
  # we should expect that there are other separating hyperplanes than
  # the one defined by our choice of w and b
}
plot(X,pch=(y+2),col=(y+3),cex=1)   # observe data 


# now we want to phrase the problem as quadratic programming.  
# the generic formulation is: 
#  minimize x^t D x + d^t x s.t. A x >= b
# where 
#  x is a k-vector 
#  D is kxk matrix
#  d is k-vector
#  A is Nxk matrix
#  b is  N-vector
#
# We showed that "our" QP problem can be written out in this form where
#
# D is 0-1 matrix with only the first two diagonal elements 1
# d is 0
# The first two columns of A are the X matrix with sign flips according to y
# and the last column is y
# b is a vector of 1's

# construct the corresponding elements: D,d, A, b

D = diag(3);
D[3,3] = .001;  # first two components are the w elements and last one b
# minor kludge here,  need a positive definite matrix so add a small
# value for the last diagonal element
d = rep(0,3);
A = cbind(X*y,y);
b = rep(1,N)
result = solve.QP(D,d,t(A),b);  # painless QP result from package!!

what = result$solution[1:2];  # margin-maximizing w
bhat = result$solution[3];    # margin-maximizing b
abline(-bhat/what[2],-what[1]/what[2]);  # draw the separating line
# abline gives line in mx+b form so need to transform our line represention
# which is w^t x b = w_1 x + w_2 y + b = 0 to 
# y = -w_1/w_2 x - b/w_2