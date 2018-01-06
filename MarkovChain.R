# Compute this as a markov chain with probabilities of each state transition
cell <- c(0,1,0,0,     
          0,0,0.5,0.5,
          0.5,0,0,0.5,
          1,0,0,0)
rnames <- c("A","B","C","D")
cnames <- c("A","B","C","D")

# number of states
cnt.states <- 4

# end state
end.state <- 10

# probabilities of state transitions
prob.trans=matrix(cell, nrow=4, ncol=4, byrow=TRUE, dimnames=list(rnames,cnames))
cat("Transition probabilities..\n")
print(prob.trans)

rnames <- c("A","B","C","D")
cnames <- rep(1:10)
prob.states <- matrix(0,4,10, dimnames=list(rnames, cnames))


# Given that we begin in A
prob.states[1,1] <- 1

# Loop until the end state
for (i in 2:end.state) {
  for (j in 1:cnt.states) {
    for (k in 1:cnt.states) {
      prev.prob <- prob.states[k,i-1] * prob.trans[j,k]
      prob.states[j,i] <- prob.states[j,i] + prev.prob 
    }
  }
} 

cat("\n Probability that the 10 state is A given the previous states..\n")
print(prob.states["A","10"])


