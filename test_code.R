
library(IMFData)
library(patchwork)
library(zoo)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tseries)
library(knitr)
library(MASS)
library(ggplot2)
library(reshape2)
library(progress)
library(psych)
library(truncnorm)
library(extraDistr)
set.seed(123)

n = 1000  
cov_matrix = diag(2)  

random_walk_sample = matrix(nrow = n, ncol = 2)
#initialize the random walk at 0,0
random_walk_sample[1, ] = c(0, 0)

for (i in 2:n) {
  random_walk_sample[i, ] = random_walk_sample[i - 1, ] + mvrnorm(n = 1, mu = c(0, 0), Sigma = cov_matrix)
}

data = random_walk_sample
N = ncol(data)
p = 1
kappa1 = 0.02^2
kappa2 = 10^2
A_prior             = matrix(0,1+p*N,N)
A_prior[2:(N + 1),] = diag(N)
V_prior     = diag(c(kappa2,kappa1*((1:p)^(-2))%x%rep(1,N)))
S_prior     = diag(1, N)
nu_prior    = ncol(random_walk_sample)+1
S1 = 100
S2 = 200

# posterior_samples = BVAR_MA_fit(data = random_walk_sample, p = p, S1= S1, S2 = S2, kappa1 = kappa1, kappa2 = kappa2, A_prior = A_prior, V_prior = V_prior, S_prior = S_prior, nu_prior = nu_prior)
# posterior_samples_A = posterior_samples[[1]]
# posterior_samples_Sigma = posterior_samples[[2]]
# print('Posterior mean of autoregressive parameter:')
# round(apply(posterior_samples_A,1:2,mean),3)
# print('Posterior standard deviation of autoregressive parameter:')
# round(apply(posterior_samples_A,1:2,sd),3)
# print('Posterior mean of covariance matrix:')
# round(apply(posterior_samples_Sigma,1:2,mean),3)
# print('Posterior standard deviation of covariance matrix:')
# round(apply(posterior_samples_Sigma,1:2,sd),3)


##########################################################################################################################################################################################
##########################################################################################################################################################################################
##########################################################################################################################################################################################
#BVAR_MA_fit <- function(data, p, S1, S2, kappa1, kappa2, A_prior, V_prior, S_prior, nu_prior, N){
  Y = (data[(p+1):nrow(data),c(1,2)])
  N = ncol(Y)
  Ty = nrow(Y)
  S = S1 + S2
  X = matrix(1,nrow(Y),1) 
  K = 1+p*N
  for (i in 1:p){
    X     = cbind(X, (data[(p+1):nrow(data)-i,c(1,2)]))
  }
  
  nu_bar = Ty + nu_prior
  #S_bar = S_prior +  t(Y)%*%Y + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
  #S_bar_inv = solve(S_bar)
  
  #Sigma_posterior = rWishart(S, df=nu_bar, Sigma=S_bar_inv)
  psi = 0
  H_psi = matrix(0, nrow = Ty, ncol = Ty)
  O_psi = diag(rep(1, Ty))
  O_psi[1,1] = 1+psi^2
  for (t in 1:Ty){
    H_psi[t,t] = 1
    if (t < Ty){
      H_psi[t+1, t] = psi
    }
  }
  A_posterior = array(rnorm(prod(c(c(K,N),S))),c(c(K,N),S))
  Sigma_posterior = array(dim = c(N,N,S))
  Omega_posterior = array(dim = c(Ty, Ty, S+1))
  Omega_posterior[,,1] = H_psi %*% O_psi %*% t(H_psi)
  
  
  pb <- progress_bar$new(total = S)
    
  for (s in 1:S){
    pb$tick()
    chol_Omega = chol(Omega_posterior[,,1]) #########change 
    chol_Omega_inv = solve(chol_Omega)
    #V_bar = solve(t(X)%*%X + solve(V_prior))
    V_bar = solve(t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%X) + solve(V_prior))
    L = t(chol(V_bar))
    #A_bar = V_bar%*%(t(X)%*%Y + solve(V_prior)%*%A_prior)
    A_bar = V_bar%*%(t(chol_Omega_inv%*%X)%*%(chol_Omega_inv%*%Y) + solve(V_prior)%*%A_prior)
    #S_bar = S_prior +  t(Y)%*%Y + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
    S_bar = S_prior +  t(chol_Omega_inv%*%Y)%*%(chol_Omega_inv%*%Y) + t(A_prior)%*%solve(V_prior)%*%A_prior - t(A_bar)%*%solve(V_bar)%*%A_bar
    S_bar_inv = solve(S_bar)
    Sigma_posterior[,,s] = solve(rWishart(1, df=nu_bar, Sigma=S_bar_inv)[,,1])
    A_posterior[,,s]= A_bar + L%*%A_posterior[,,s]%*%chol(Sigma_posterior[,,1]) ############change 
  }
  A_posterior = A_posterior[,,(S1+1):(S1+S2)]
  Sigma_posterior = Sigma_posterior[,,(S1+1):(S1+S2)]
  #return(list(A_posterior, Sigma_posterior))
#}
##################################################################
###################running function
##############################################################



  print('Posterior standard deviation of autoregressive parameter:')
  round(apply(A_posterior,1:2,sd),3)
  print('Posterior standard deviation of covariance matrix:')
  round(apply(Sigma_posterior,1:2,sd),3)
  print('Posterior mean of autoregressive parameter:')
  round(apply(A_posterior,1:2,mean),3)
  print('Posterior mean of covariance matrix:')
  round(apply(Sigma_posterior,1:2,mean),3)