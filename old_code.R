```{r, echo=FALSE, message=FALSE, warning=FALSE, , fig.width=12, fig.height=13, eval = FALSE}
# if (!require(matrixNormal)) {install.packages('matrixNormal')}
# library(matrixNormal)
# 
# BVAR_fit <- function(Y, S0, Sigma0, p, A_prior, S1, S2, kappa1, kappa2, nu0){
#   N = ncol(Y)
#   T = nrow(Y)
#   K = 1+p*N
# 
#   X = matrix(NA, nrow = T, ncol = ncol(Y) * p + 1)
#   X[, 1] = 1
#   for (t in 2:T) {
#     for (lag in 1:p) {
#       if(t-lag<1){next}
#       else{X[t, (ncol(Y) * (lag-1) + 2):(ncol(Y) * lag + 1)] <- Y[t-lag, ]}
#      }
#   }
#   X[is.na(X)] = 0
# 
#   I_T = diag(1,T,T)
#   I_N = diag(1,N,N)
# 
#   Sigma_matrices = list()
#   A_matrices = list()
#   Sigma_matrices = c(Sigma_matrices, list(Sigma0))
#   A_matrices = c(A_matrices, list(diag(1, K, N)))
#   Va = diag(c(kappa2, kappa1*kronecker(seq(1,p)^(-2),rep(1, N))))
# 
# 
#   for  (s in 2:(S1+S2)){
#     V_bar = solve(t(X)%*%X + solve(Va))
#     A_bar = V_bar%*%(t(X)%*%Y + solve(Va)%*%A_prior)
#     A = rmatnorm(M=A_bar, V = Sigma_matrices[[s-1]], U =V_bar, s = 1)
#     nu_bar = T + nu0
#     S_bar = S0 + t(Y)%*%Y + t(A_prior)%*%solve(Va)%*%A_prior-t(A_bar)%*%solve(V_bar)%*%A_bar
#     sigma = rWishart(1, nu_bar, S_bar)
# 
#     Sigma_matrices[[s]] = sigma[,,1]
#     A_matrices = c(A_matrices, list(A))
#   }
#   Sigma_matrices = Sigma_matrices[(S1+1):(S1+S2)]
#   A_matrices = A_matrices[(S1+1):(S1+S2)]
#   return(list(A_matrices, Sigma_matrices))
# }
# 
# S0 = diag(1, ncol(random_walk_sample))
# Sigma0 = diag(1, ncol(random_walk_sample))
# p = 1
# A_prior = matrix(0, 1+p*ncol(random_walk_sample), ncol = ncol(random_walk_sample))
# S1 = 100
# S2 = 200
# kappa1 = 0.2^2
# kappa2 = 10^2
# nu0 = ncol(random_walk_sample) +3
# 
# posterior_samples = BVAR_fit(Y = random_walk_sample, S0, Sigma0 = Sigma0, p, A_prior, S1, S2, kappa1, kappa2, nu0)
# 
# sampled_posterior_A = posterior_samples[[1]]
# sampled_posterior_sigma = posterior_samples[[2]]
# 
# average_matrix_A <- matrix(0, nrow = nrow(sampled_posterior_A[[1]]), ncol = ncol(sampled_posterior_A[[1]]))
# for (mat in sampled_posterior_A) {
#   average_matrix_A <- average_matrix_A + mat
# }
# average_matrix_A <- average_matrix_A / length(sampled_posterior_A)
# print(round(average_matrix_A,3))
```