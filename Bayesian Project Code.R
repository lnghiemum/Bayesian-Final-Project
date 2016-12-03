# STAT 6390 Final Project

# HDP 
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

# LDA implementation

# Generative process
# dictionary
dict <- c("a", "b", "c", "d", "e")
# 3 topics possible K = 3
beta = rep(1,5) # distribution over words
library(MCMCpack) #For Dirichlet distribution
phi = rdirichlet(3,beta)
# Document 1
alpha = rep(1,3)
theta = rdirichlet(1,alpha)
# Each single word
z= numeric(1000) # topic index assignment
w=character(1000)
for (i in 1:1000){
  z[i]=sample(1:3, 1, prob=theta)
  w[i]=dict[sample(1:5, 1, prob=phi[z[i],])]
}
z
w
# Now given d, use Gibbs sampling
K=3
alpha=rep(1,K)
beta=rep(1,5) # Number of words in dictionary
z.t = sample(1:K,1000, replace=TRUE) # Initial topic assignment
# One document, so n_d,k and n_k are the same
n_k = numeric(K)
for (i in 1:K){
  n_k[i] = length(z.t[z.t==i])
}
n_kw = matrix(0, nrow=1000, ncol=K) # Each row is a word, each column is a topic. Count how many times a word is assigned to a topic
for (i in 1:1000){ 
  n_kw[i,z.t[i]]=1
}
n_kw[1:3,]
for(iteration in 1:200){
  print(iteration)
  for (word in 1:1000){
    topic <- z.t[word]
    #n_k[topic] = n_k[topic]-1
    #n_kw[word,topic] = n_kw[word,topic]-1
    prob=numeric(3)
    for (k in 1:3){
      prob[k]=(n_k[k] + alpha[k])*(n_kw[word,topic]+beta[which(dict==w[word])])/(n_k[k]+sum(beta))
    }
    topic = sample(1:K,1,replace=TRUE, prob=prob)
    z.t[word]= topic
    n_k[topic] = n_k[topic]+1
    n_kw[word,topic] = n_kw[word,topic]+1
  }
}




