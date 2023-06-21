source("ccc-monte-carlo-fun.R")



set.seed(100)
m <- 1                                                # Target correlation
n <- c(200, 500, 5000, 50000); names(n) <- n     # Number of samples
S <- 1000                                               # Number of simulations

# distribution parameters
  m1 <- -sqrt(.1)/2
  m2 <- -sqrt(.25)/2
  
  mu0 <- c(0,0)
  mu1 <- c(-m1,m1)
  mu2 <- c(-m2,m2)
  
  # covariance matrices
  mat_gen <- function(a,b,sca){
    matrix(c(a^2, sca*a*b, sca*a*b, b^2),ncol=2)
  }
  
  mat1 <- mat_gen(1,1,0.95)
  mat2 <- mat_gen(1.1,0.9,0.95)
  mat3 <- mat_gen(0.9,1.1,0.8)
  mat4 <- mat_gen(4/3,2/3,0.5)

# case 3 params
  mean_g <- mu1
  covar_g <- mat2
  rhog <- .887
# case 5
  mean_g <- mu2
  covar_g <- mat4
  rhog <- 0.5
  ccc <- .36
dist <- "norm"


# produce plot
res <- sapply(n,
              function(n, rhog, S){
                replicate(S, mean(infer(m,n,rhog,dist,mean_g,covar_g)[,1]))
              }, 
              rhog = rhog, S = S)
boxplot(res, xlab = "Sample size", ylab = "Correlation")
abline(h = ccc, col = "red")

png('convergence_eval.png')

boxplot(res, xlab = "Sample size", ylab = "Correlation")
abline(h = ccc, col = "red")

dev.off

