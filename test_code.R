
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
tau = 0.2
psi_prior = 0 
Vpsi_prior = 1

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
  
  accept = 0
  
  pb <- progress_bar$new(total = S)
  
  for (s in 1:S){
    pb$tick()

    chol_Omega = chol(Omega_posterior[,,s]) #########change 
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
    ###############MH Step    ##################################
    psi_candidate = rnorm(n = 1, mean = psi, sd = tau)
    if(abs(psi_candidate) > 1){
      alpha = 0
    }
    else{
      H_psi = matrix(0, nrow = Ty, ncol = Ty)
      O_psi = diag(rep(1, Ty))
      O_psi[1,1] = 1+psi_candidate^2
      for (i in 1:Ty){
        H_psi[i,i] = 1
        if (i < Ty){
          H_psi[i+1, i] = psi_candidate
        }
      }
      
      
      Omega_draw = H_psi %*% O_psi %*% t(H_psi)
      chol_Omega_draw = chol(Omega_draw)
      chol_Omega_draw_inv = solve(chol_Omega_draw)
      p_y_numerator = ((1 + psi_candidate^2)/( 1 + psi^2))^(-N/2)*exp(-1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s])))
                                                                      +1/2*tr(solve(Sigma_posterior[,,s])%*%t(chol_Omega_draw_inv%*%(Y-X%*%A_posterior[,,s]))%*%(chol_Omega_inv%*%(Y-X%*%A_posterior[,,s]))) )
      numerator = p_y_numerator * dtruncnorm(x=psi_candidate, mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi, mean = psi_candidate, sd = tau)
      denominator =  dtruncnorm(x=psi, mean=psi_prior, sd = Vpsi_prior, a = -1, b = 1) *dnorm(x = psi_candidate, mean = psi, sd = tau)
      alpha = min(1, numerator/denominator)
    }
    u = runif(1, min = 0, max = 1)
      #print(numerator/denominator)
    if (u <= alpha){
      accept = accept + 1
      psi = psi_candidate
      Omega_posterior[,,s+1] = Omega_draw
      }
    else{
      Omega_posterior[,,s+1] = Omega_posterior[,,s]}
    ##################################################    #####
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
  print('Posterior standard deviation of row specific covariance matrix:')
  round(apply(Sigma_posterior,1:2,sd),3)
  
  print('Posterior mean of autoregressive parameter:')
  round(apply(A_posterior,1:2,mean),3)
  print('Posterior mean of column specific covariance matrix:')
  Omega_posterior_mean = round(apply(Omega_posterior,1:2,sd),3)
  Omega_posterior_mean[1:10, 1:10]
  print('Posterior mean of row specific covariance matrix:')
  round(apply(Sigma_posterior,1:2,mean),3)
