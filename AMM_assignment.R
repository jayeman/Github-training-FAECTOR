data = read.csv('467883.csv')

N = length(unique(data[ , 1]))
T = length(unique(data[ , 2]))

y = matrix(data[ , 3], N, T)
price = data[ , 4]
x2 = price[seq(1, length(price), N)]
x1 = rep(1, T)
X = cbind(x1, x2)

K = 3
theta = c(0.1, 0.2, 0.01, 0.02, 0.001, 0.002)
LogL <- function(theta, pi, y, X){
  theta = matrix(theta, K, 2, byrow=TRUE)
  # X_expand = cbind(rep(X[ , 1], rep(K, 25)), rep(X[ , 2], rep(K, 25)))
  # theta %*% t(X_expand)
  
  f_y = rep(0, N)
  for (i in 1:N){
    for (t in t:T){
      f_yi = 1
      prob1_cat = rep(0, K)
      for (r in 1:K) {
        prob1 = exp(t(theta[r, ]) %*% X[t, ]) / (1 + exp(t(theta[r, ]) %*% X[t, ]))
        prob1_cat[r] = prob1
      }
      f_yi = f_yi * sum(pi * (prob1_cat^y[i, t] * (1-prob1_cat)^(1-y[i,t])))
    }
    f_y[i] = f_yi
  }
  
  return(log(prod(f_y)))
}

LogL(theta, pi=rep(1/3, 3), y, X)

MStep <- function(W, y, X, theta){
  pi = colMeans(W)
  
}