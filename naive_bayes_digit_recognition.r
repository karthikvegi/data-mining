# using a naive bayes classifier for the digit recognition example from before.

# ---------------------
# create data as before

set.seed(1234);  # want random experiments to be exaclty repeatable
n = 200;	 # number examples
y = rep(0:9, length = 200);	# the true classes
temp = c(1,1,1,0,1,1,1,		# pattern for 0
         0,0,1,0,0,1,0,		# pattern for 1	
         1,0,1,1,1,0,1,		# pattern for 2
	 1,0,1,1,0,1,1,		# ...
	 0,1,1,1,0,1,1,
	 1,1,0,1,0,1,1,
	 0,1,0,1,1,1,1,
	 1,0,1,0,0,1,0,
	 1,1,1,1,1,1,1,	
	 1,1,1,1,0,1,0);
lights = matrix(temp,10,7,byrow=T);	# true light[i,] is bit pattern for i-1
temp1 = matrix(rbinom(n*7,1, .9), n, 7)
temp1 = ifelse(lights[y+1,] == 1, temp1, 1-temp1); #  digits with flipped bits
temp2 = matrix(rbinom(n*17,1,.5), n, 17)  # random lights (noisy worthless features)
x = cbind(temp1,temp2);  # variables 8 ... 24 are useless noise


#----------------------

prior = rep(1/10,10);		# bayes classifier requires prior dist.  assume all classes ='ly likely
p = matrix(0,nrow=10,ncol=24);   # p[c,j] will be estimate of P(X_j = 1 | Y = c)

for (c in 1:10) {  # for each class
    z = x[y==(c-1),];  # pull out data corresponding to class c
    p[c,] = colSums(z)/nrow(z);  # p[c,j] = P(X_j = 1 | Y = c)
}


set.seed(1236);  # want random experiments to be exaclty repeatable
n = 200;	 # number examples
y = rep(0:9, length = 200);	# the true classes
temp = c(1,1,1,0,1,1,1,		# pattern for 0
         0,0,1,0,0,1,0,		# pattern for 1	
         1,0,1,1,1,0,1,		# pattern for 2
	 1,0,1,1,0,1,1,		# ...
	 0,1,1,1,0,1,1,
	 1,1,0,1,0,1,1,
	 0,1,0,1,1,1,1,
	 1,0,1,0,0,1,0,
	 1,1,1,1,1,1,1,	
	 1,1,1,1,0,1,0);
lights = matrix(temp,10,7,byrow=T);	# true light[i,] is bit pattern for i-1
temp1 = matrix(rbinom(n*7,1, .9), n, 7)
temp1 = ifelse(lights[y+1,] == 1, temp1, 1-temp1); #  digits with flipped bits
temp2 = matrix(rbinom(n*17,1,.5), n, 17)  # random lights (noisy worthless features)
x = cbind(temp1,temp2);  # variables 8 ... 24 are useless noise



yhat = rep(0,nrow(x));	# the estimated class for each example
for (i in 1:nrow(x)) {  # for each example
    pc = prior;	     # begin with prior dist
    o = x[i,];	     # ith observation
    for (c in 1:10) {  # for each class
    	for (j in 1:24) { # for each feature
	    prob = ifelse(o[j], p[c,j] , 1-p[c,j]);	# if jth feat is 1 take p[c,j].  ow take 1-p[c,j]
	    pc[c] = pc[c] * prob;      # accumulate prob in pc[c]
	}
    }
    yhat[i] = which.max(pc)-1;  # take maximizing class
}
error_rate =  sum(yhat != y)/nrow(x)
print(error_rate);  # error rate when testing on training.  


# comment:  we are testing this classifier on the training, which is never the correct thing for
# honest estimate of generalization error.  however, can simulate additional data from model
# and get honest generalization error.  

# Big Q:   with 10*24 = 240 free parameters, why doesn' this overfit???
