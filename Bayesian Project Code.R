# STAT 6390 Paper

# DP 
# Notation like Teh Jor Paper (2010)
N = 1000 # number of words in the dictionary
prob.N=rep(1/N,N)
#Base measure G0
gamma <- 3
H <- function(n) rmultinom(n, N, prob.N) #
# Stick breaking process
n <- 100
v <- rbeta(n, 1, gamma)
beta <- numeric(n)
beta[1] <- v[1]
beta[2:n] <- sapply(2:n, function(i) v[i] * prod(1 - v[1:(i-1)]))
theta.double.star <- H(n)

#A realization of G0
theta0 <- sample (theta.double.star, 3, prob=beta, replace=TRUE) 

# Now generate each random measure G_j
alpha <- 4
theta=matrix(NA,nrow=length(beta), ncol=10)
# for each beta_k, generates pi_jk
for (k in 1:length(beta)){
  v.star=rbeta(n, alpha*beta[k], alpha*(1-sum(beta[1:k])))
  pi= numeric(n)
  pi[1] = v.star[1]
  pi[2:n] <- sapply(2:n, function(i) v.star[i] * prod(1 - v.star[1:(i-1)]))
  theta[k,] = sample(theta.double.star, 10, prob=pi, replace=TRUE)
}




theta <- sample(theta.double.star, prob=pi.vector, replace=TRUE) 