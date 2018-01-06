# example of a Gaussian classifer on the famous Iris data.  For each class we 
# compute the mean and covariance matrix, as well as the determinant
# of the covariace matrix (this appears in the denominator of
# the multivariate Gaussian density.  In implementing the
# Bayes classifier we take the three classes as having
# equal probability.  Thus we can ignore the "prior"
# contribution and classify according to the class
# that gives each data vector the greatest Gaussian
# probability density.  

data(iris);  # include the famous iris data
n = nrow(iris);
type = rep(0,n);

type[iris[,5] == "setosa"] = 1;  # type is now a vector of 1-3 for 3 types
type[iris[,5] == "versicolor"] = 2;
type[iris[,5] == "virginica"] = 3;

# there may be a slicker way to do the following calculations
# but this is straightforward

X1 = iris[type==1,1:4];
X2 = iris[type==2,1:4];
X3 = iris[type==3,1:4];

m1 = colMeans(X1);
m2 = colMeans(X2);
m3 = colMeans(X3);

S1 = solve(cov(X1));		# inverse of class covariance for class 1
S2 = solve(cov(X2));
S3 = solve(cov(X3));

det1 = 1/det(S1);
det2 = 1/det(S2);
det3 = 1/det(S3);

d = rep(0,3);
c = rep(0,n)
for (i in 1:n) {
    x = iris[i,1:4];
    d[1] =  log(det1) + sum((x-m1) * S1 %*% t(x-m1))
    d[2] =  log(det2) + sum((x-m2) * S2 %*% t(x-m2))
    d[3] =  log(det3) + sum((x-m3) * S3 %*% t(x-m3))
    c[i] = which.min(d);
}
error_rate = sum(c != type)/n
print(error_rate)


