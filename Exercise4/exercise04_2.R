

rdirichlet <- function(a) {
  y <- rgamma(length(a), a, 1)
  return(y / sum(y))
}



N = 5000
K = 5
a = 1.0
b = 2
mu_hat = 0
sd_hat = 10

alpha = 1

pi = rdirichlet(rep(alpha, K))

mu = rep(0L, K)
sigma = rep(0L, K)

for (k in 1:K) {
  sigma[k] = rgamma(1, a, b)
  mu[k] = rnorm(1, mean = mu_hat, sd = sd_hat)
}
z = rep(0, N)
x = rep(0, N)

for (i in 1:N) {
  z[i] = sample(1:K, size=1, replace = TRUE, prob = pi)
  x[i] = rnorm(1, mu[z[i]], sigma[z[i]])
}

hist(x, breaks = 50)