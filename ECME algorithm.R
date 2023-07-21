library(miscF)
set.seed(11)
mu1 <-100
mu2 <-105
sigma11 <- 100
sigma22 <- 125
rho12 <- 0.5
Sigma <- matrix(c(sigma11, rho12*sqrt(sigma11*sigma22),
                  rho12*sqrt(sigma11*sigma22), sigma22),
                nrow=2)
k <- 5
N <- 1000
require(mvtnorm)
X <- rmvt(N, sigma=Sigma, df=k, delta=c(mu1, mu2))

result <- mvt.ecme(X,1,999,err=1e-4)
result$Mu
result$Sigma
result$v
