# STAT 6390 Paper

# DP 
# Notation like Teh Jor Paper (2010)
# Put a limit on K
K = 100 
#Base measure G0
gamma <- 3
H <- function(n) rmultinom(n, K, rep(1/K,K)) # Prior distribution of topics
# Stick breaking process
v <- rbeta(K, 1, gamma)
beta <- numeric(K)
beta[1] <- v[1]
beta[2:K] <- sapply(2:K, function(i) v[i] * prod(1 - v[1:(i-1)]))
theta.double.star <- as.vector(H(1))

#A realization of G0
theta0 <- sample (theta.double.star, N, prob=beta, replace=TRUE) 

# Now generate each random measure G_j
alpha <- 4
theta=matrix(NA,nrow=length(beta), ncol=N)
# for each beta_k, generates pi_jk
for (k in 1:length(beta)){
  v.star=rbeta(n, alpha*beta[k], alpha*(1-sum(beta[1:k])))
  pi= numeric(n)
  pi[1] = v.star[1]
  pi[2:beta] <- sapply(2:length(beta), function(i) v.star[i] * prod(1 - v.star[1:(i-1)]))
  theta[k,] = sample(theta.double.star, N, prob=pi, replace=TRUE)
}

theta