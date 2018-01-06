# here we generate non-separable data by first generating separable data as before, then random flipping a few class labels.
# Rather than penalizing the unsatisfied constraints by C \xi_i, as discussed in class, we penalize them by C \xi_i^2  (the
# squares of the slack variables 
# in some of the random cases the separating hyperpline is drawn off the field of view

library("quadprog");  # we do this by quadratic programming and need library

w = rnorm(2);  # choose a random weight vector (unknown to us)
b = rnorm(1);  # choose the b offset (also unknown)
N = 20;       # number of observations
X = matrix(0,nrow = N, ncol = 2);  #  the data matrix of predictors
y = rep(0,N);  # the vector of {-1,1} elements giving classes 
for (i in 1:N) {  # generate the data
  X[i,]  = rnorm(2);  # choose each predictor vector randomly
  y[i] = sign(t(w) %*% X[i,] + b)  # choose data to be linearly separable ....
  if (runif(1) < .1) y[i] = - y[i];  # randomly flip about .1 of the points
}
plot(X,pch=(y+2),col=(y+3),cex=1)   # observe data 

X = cbind(X^2,X[,1]*X[,2],X,rep(1,N));
feat = 6;

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


# construct the corresponding elements: D,d, A, b
dim = feat + N;

D = diag(1, dim); # the penalty is the sum of square of all variables except b which gets only a small penalty to
    	    	  # keep the D matrix positive definite
D[feat,feat] = .001;    # this is the kludge for bias term
d = rep(0,dim);
A = matrix(0,2*N,dim);
A[1:N,1:feat] = X*y;
A[1:N,(feat+1):dim] = diag(N);
A[(N+1):(2*N),(feat+1):dim] = diag(N);
b = rep(0,2*N)
b[1:N] = 1;
result = solve.QP(D,d,t(A),b);  # painless QP result from package!!

what = result$solution[1:feat];  # margin-maximizing w



x1 = seq(-3,3,by=.01);  # fix x1 to variables values and compute value of x2 as root of quadratic
#  solve for x2 in 
#  what[1]*x1^2 + what[2]*x2^2 + what[3]*x1*x2 + what[4]*x1 + what[5]*x2 + what[6] = 0
#  by reducing to quadratic in x2 by x1 to chosen values.  
a = what[2];
b = what[5] + what[3]*x1;
c = what[1]*(x1^2) + what[4]*x1 + what[6];
disc = (b^2)-(4*a*c);
t = (disc >= 0);
x2 = rep(0,length(x1));
x2[t] = (-b[t] + sqrt(b[t]*b[t]-4*a*c[t]))/(2*a);
lines(x1[t],x2[t]);
x2[t] = (-b[t] - sqrt(b[t]*b[t]-4*a*c[t]))/(2*a);
lines(x1[t],x2[t]);
