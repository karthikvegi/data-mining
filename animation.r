# While there are only two spatial dimensions to use in plots, one can
# also make use of time.  This is useful for plotting things that evolve
# over time, or things that evolve over some other dimension which we
# *depict* as time in our plot

N = 100  # the number of objects we trace
T = 1000  # the number of time points (frames) we show
s = .0005
x = matrix(0,nrow=n,ncol=T);
y = matrix(0,nrow=n,ncol=T);
for (n in 1:N) {
    x[n,] = cumsum(cumsum(runif(T,min=-s,max=s)))  # x time series
    y[n,] = cumsum(cumsum(runif(T,min=-s,max=s)))  # y time series
}
plot(0,0,xlim=c(-1,1),ylim=c(-1,1), main="Life stories of your hometown");  # establish the plot
for (t in 1:T) {  # do the animation
   plot(x[,t],y[,t],pch=1:25,col=1:N,xlim=c(-1,1),ylim=c(-1,1));  # new plot each frame
#   points(x[,t],y[,t],pch=1:25,col=1:N,xlim=c(-1,1),ylim=c(-1,1));  # add to prevous plot
}
