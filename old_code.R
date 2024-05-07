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
data = random_walk_sample
N = ncol(data)
T = nrow(data)
X = matrix(1,T,1)
S = S1 + S2
for (i in 1:p){
  X     = cbind(X,data[p+1:T-i,])
}

sigma_h_squared = rinvgamma(n = 1, alpha = vh0_prior, beta = sh0_prior)
rho = rtnorm(n = 1, mean = rho_prior, sqrt(Vrho_prior), a = -1, b = 1)
u_h = rnorm(n = T, mean = 0, sd = sqrt(sigma_h_squared))
h = array(dim = T)
u = array(dim = c(N,1,T))
h [1] = rho * 0 + u_h[1]
psi = rtnorm(n = 1, mean = psi_prior, sd = sqrt(Vpsi_prior), a = -1, b = 1)
epsilon = array(dim = c(N,1,T))
Omega = array(0, dim = c(T,T))

for (t in 2:T){
  h[t] = rho*h[t-1] + u_h[t]
} 

Sigma_prior = rWishart(n = 1, df = nu_prior, Sigma = S_prior)[,,1]
Sigma_prior = solve(Sigma_prior)

for (t in 1:T){
  u_var = exp(h[t])*Sigma_prior
  u[,,t] = mvrnorm(n = 1, mu = rep(0,N), Sigma = u_var)
  if (t == 1){
    epsilon[,,t] = u[,,t]
    Omega[t,t] = (1+psi^2)*exp(h[t])
  }
  else{
    epsilon[,,t] = u[,,t] + psi*u[,,t-1]
    Omega[t,t] = psi^2*exp(h[t-1]) + exp(h[t])
    Omega[t-1,t] = psi * exp(h[t-1])
    Omega[t,t-1] = psi * exp(h[t-1]) 
  }
}

V_bar = solve(t(X)%*%solve(Omega)%*%X + solve(V_prior))
A_bar = V_bar%*%(t(X)%*%solve(Omega)%*%data + solve(V_prior)%*%A_prior)
nu_bar = T + nu_prior
#S_bar  = S_prior + t(data)%*%data + t(A_prior)%*%diag(1/diag(V_prior))%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
S_bar = S_prior +  t(data)%*%solve(Omega)%*%data + t(A_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
S_bar_inv = solve(S_bar)
#Sigma_posterior = rWishart(S, df=nu_bar, Sigma=S_bar_inv)
Sigma_posterior = rWishart(S, df=nu_bar, Sigma=S_bar_inv)
Sigma_posterior = apply(Sigma_posterior,3,solve)
Sigma_posterior  = array(Sigma_posterior,c(N,N,S))
A_posterior = array(rnorm(prod(c(dim(A_bar),S))),c(dim(A_bar),S))
L = t(chol(V_bar))

for (s in 1:S){
  A_posterior[,,s]= A_bar + L%*%A_posterior[,,s]%*%chol(Sigma_posterior[,,s])
}
A_posterior = A_posterior[,,(S1+1):(S1+S2)]
Sigma_posterior = Sigma_posterior[,,(S1+1):(S1+S2)]














```{r, echo=FALSE, message=FALSE, warning=FALSE, , fig.width=12, fig.height=6, eval = FALSE}
library(progress)
library(psych)
library(truncnorm)

data = random_walk_sample
p = 1
kappa1 = 0.1^2
kappa2 = 10^2
N = ncol(data)
A_prior     = matrix(0, 1+p*N, ncol = N)
V_prior     = diag(c(kappa2,kappa1*((1:p)^(-2))%x%rep(1,N)))
S_prior     = diag(1, N)
nu_prior    = ncol(random_walk_sample)+1
S1 = 10
S2 = 20
S = S1 + S2
K = 1+p*N
T = nrow(data)
vh0_prior = 5
sh0_prior = 0.04
rho_prior = 0.9
Vrho_prior = 0.04
psi_prior = 0
Vpsi_prior = 1

data = random_walk_sample
X = matrix(1,T,1)
for (i in 1:p){
  X     = cbind(X,data[p+1:T-i,])
}




psi = 0
H_psi = matrix(0, nrow = T, ncol = T)
O_psi = diag(rep(1, T))
O_psi[1,1] = 1+psi^2
for (t in 1:T){
  H_psi[t,t] = 1
  if (t < T){
    H_psi[t+1, t] = psi
  }
}

Sigma_posterior = array(dim=c(N,N,S))
A_posterior = array(dim = c(K,N,S))
Omega_posterior = array(dim=c(T,T,S+1))

Omega_posterior = array(dim=c(T,T,S+1))
Omega_posterior[,,1] = H_psi %*% O_psi %*% t(H_psi)


for (s in 1:S){
  print(s)
  chol_Omega = chol(Omega_posterior[,,s])
  chol_Omega_inv = solve(chol_Omega)
  V_bar = solve(t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%X) + solve(V_prior))
  A_bar = V_bar%*%t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%data) + solve(V_prior)%*%A_prior
  
  nu_bar = T + nu_prior
  #S_bar  = S_prior + t(data)%*%data + t(A_prior)%*%diag(1/diag(V_prior))%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
  S_bar = S_prior +  t(chol_Omega_inv%*%data)%*%(chol_Omega_inv%*%data) + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
  S_bar_inv = solve(S_bar)
  #Sigma_posterior = rWishart(S, df=nu_bar, Sigma=S_bar_inv)
  Sigma_posterior[,,s] = rWishart(n = 1, df=nu_bar, Sigma=S_bar_inv)[,,1]
  Sigma_posterior[,,s] = solve(Sigma_posterior[,,s])
  A_posterior = array(rnorm(prod(c(dim(A_bar),S))),c(dim(A_bar),S))
  L = t(chol(V_bar))
  
  A_posterior[,,s]= A_bar + L%*%A_posterior[,,s]%*%chol(Sigma_posterior[,,s])
  
  #Metropolis Hasting Step 
  N_iter <- 100
  accept = 0 
  tau = 1.5
  psi_MH = array(dim = N_iter+1)
  psi_MH[1] = 0
  pb <- progress_bar$new(total = N_iter)
  for (t in 2:N_iter){
    pb$tick()
    #geneerate a candidate psi
    psi_current = rnorm(n = 1, mean = psi_MH[accept+1], sd = tau)
    if(abs(psi_current) > 1){
      alpha = 0
    }
    else{
      p_y_numerator = (1+psi_current^2)^(-N/2)*exp(-1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_inv%*%(data-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(data-X%*%A_posterior[,,s]))))
      numerator = p_y_numerator * dtruncnorm(x=psi_current, mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi_MH[accept+1], mean = psi_current, sd = tau)
      H_psi = matrix(0, nrow = T, ncol = T)
      O_psi = diag(rep(1, T))
      O_psi[1,1] = 1+psi_current^2
      for (i in 1:T){
        H_psi[i,i] = 1
        if (i < T){
          H_psi[i+1, i] = psi_current
        }
      }
      Omega_draw = H_psi %*% O_psi %*% t(H_psi)
      chol_Omega_draw = chol(Omega_draw)
      chol_Omega_draw_inv = solve(chol_Omega_draw)
      
      
      p_y_denominator = (1+psi_MH[accept+1]^2)^(-N/2)*exp(-1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_draw_inv%*%(data-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(data-X%*%A_posterior[,,s]))))
      denominator = p_y_denominator * dtruncnorm(x=psi_MH[accept+1], mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi_current, mean = psi_MH[accept+1], sd = tau)
      alpha = min(1, numerator/denominator)
      u = runif(1, min = 0, max = 1)
      #print(numerator/denominator)
      if (u <= alpha){
        accept = accept + 1
        psi_MH[accept+1] = psi_current
        
      }
    }
  }
  print('Acceptance rate: ')
  print(accept/N_iter)
  print(psi_MH)
  psi_MH = psi_MH[complete.cases(psi_MH)]
  print(psi_MH)
  psi_MH = psi_MH[floor(N_iter/3):length(psi_MH)]
  psi = sample(psi_MH, size = 1)
  H_psi = matrix(0, nrow = T, ncol = T)
  O_psi = diag(rep(1, T))
  O_psi[1,1] = 1+psi^2
  for (t in 1:T){
    H_psi[t,t] = 1
    if (t < T){
      H_psi[t+1, t] = psi
    }
  }
  #print((H_psi %*% O_psi %*% t(H_psi))[1:10, 1:10])
  Omega_posterior[,,s+1] = H_psi %*% O_psi %*% t(H_psi)
}
#Omega_posterior = Omega_posterior[,,(S1):(S1+S2+1)]
#A_posterior = A_posterior[,,(S1+1):(S1+S2)]
#Sigma_posterior = Sigma_posterior[,,(S1+1):(S1+S2)]

round(apply(A_posterior,1:2,mean),3)
round(apply(A_posterior,1:2,sd),3)
round(apply(Sigma_posterior,1:2,mean),3)
round(apply(Sigma_posterior,1:2,sd),3)
```