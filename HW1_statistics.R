#HW_1 

# task 2.2
custom_density <- function(x, lambda, mu) {
  ifelse(x >= mu, lambda * exp(-lambda * (x - mu)), 0)
}

custom_cdf <- function(x, lambda, mu) {
  ifelse(x < mu, 0, 1 - exp(-lambda * (x - mu)))
}

custom_inverse_cdf <- function(p, lambda, mu) {
  mu - log(1 - p) / lambda
}

generate_random_samples <- function(n, lambda, mu) {
  u <- runif(n)
  samples <- custom_inverse_cdf(u, lambda, mu)
  return(samples)
}

n = 1000
lambda = 1
mu = 2
nrep = 100
lambda_mle = rep(NA, nrep)
lambda_mm = rep(NA, nrep)
mu_mle = rep(NA, nrep)
mu_mm = rep(NA, nrep)

for (k in 1:nrep) {
  x <- generate_random_samples(n, lambda, mu)
  lambda_mle[k] = mean(x) - min(x)
  mu_mle[k] = min(x)
  lambda_mm[k] = sqrt((n - 1) / (var(x) * n))
  mu_mm[k] = mean(x) - 1 / lambda_mm[k]
}

boxplot(cbind(lambda_mle, lambda_mm), col = "wheat", xaxt = "n")
axis(side = 1, at = 1:2, label = c("ML", "MM"))
grid()

boxplot(cbind(mu_mle, mu_mm), col = "wheat", xaxt = "n")
axis(side = 1, at = 1:2, label = c("ML", "MM"))
grid()


# task 3.3

install.packages("fitdistrplus")
library(fitdistrplus)

alpha = 2
beta = 3
n = 1000
x = rgamma(n, alpha, beta)

res1 = fitdist(x, "gamma", method = "mle")
res1$estimate

res2 = fitdist(x, "gamma", method = "mme")
res2$estimate

#task 4.2 

install.packages("bayesrules")
library(bayesrules)
n=10
alpha=6
beta=3
lambda=rgamma(n,alpha, beta)
data=rep(NA,n)
for (k in 1:n){
  data[k]=rpois(1,lambda[k])
}
plot_gamma_poisson(shape=alpha, rate=beta,n=n, sum_y=sum(data))

# task 4.3
n=100 #1000 #10000 - подставляем что нужно
alpha=6
beta=3
lambda=rgamma(n,alpha, beta)
data=rep(NA,n)
for (k in 1:n){
  data[k]=rpois(1,lambda[k])
}
alpha0vec=seq(from=1,to=50,by=10) #перебор значений параметров альфа
beta0vec=seq(from=1,to=50,by=10) #перебор значений параметров бета
lambda_est=matrix(NA,nrow=length(alpha0vec),ncol=length(beta0vec))
for (i in 1:length(alpha0vec)){
  for (j in 1:length(beta0vec)){
    res=summarize_gamma_poisson(shape=alpha0vec[i],rate=beta0vec[j],sum_y=sum(data),n=n)
    lambda_est[i,j]=res$mean[2]
  }
}

# task 5.2
n=1000
K=3 #заменяем на различные значения K
mu=sample(1:10,3,prob=rep(1/10,10)) #заменяем значения mu на sample(1:3,3,prob=rep(1/3,3))
sigma=sample(1:5,3,prob=rep(1/5,5))
alpha=c(0.3,0.3,0.4)
Y=sample(1:3,size=n,prob=alpha,replace=TRUE)
table(Y)
x=rep(NA,n)
for (k in 1:n){
  x[k]=rnorm(1,mean=mu[Y[k]],sd=sigma[Y[k]])
}
library(mixtools)
d=normalmixEM(x,k=3)
plot(d,density=TRUE)
d$loglik

loglik_vector <- vector(mode='numeric', length=9)
for (nc in 1:9) {
  d=normalmixEM(x,k=nc+1)
  loglik_vector[nc] = d$loglik
}
loglik_vector