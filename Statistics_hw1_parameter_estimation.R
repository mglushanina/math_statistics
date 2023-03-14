#n1 

n = 999
M = 100
mu = 10
sig = 5
res=matrix(data=NA,nrow=M,ncol=4)
library(stats4)
library(ExtDist)
library(gmm)

for (k in 1:M) {
   x <- rLaplace(n=n, mu=mu, b=sig)
   f = function(a, b) {
     -sum(log(dLaplace(x=x, mu=a, b=b)))
   }
   g=function(?heta,u){
     b=theta[1]
     a=theta[2]
     m1=b-u #матожидание 
     m2=2*a^2 - (u - b)^2#Ex^2 
     return(cbind(m1,m2))
   }
   moments <- gmm(g,x,c(1,2))
   fit <- mle(f,start=list(a=0.5,b=1.5),method="L-BFGS-B",lower=c(0.1,0.1))
   param1_ml <- fit@?oef[1]
   param2_ml <- fit@coef[2]
   param1_mm <- moments$coefficients[1]
   param2_mm <- moments$coefficients[2]
   res[k,] = cbind(param1_ml, param2_ml, param1_mm, param2_mm)
}


res = data.frame(res)
res$mu_mle <- res$X1
res$sig_mle <- res$X2
res$mu_mm?<- res$X3
res$sig_mm <- res$X4

boxplot(res$mu_mle, res$sig_mle, res$mu_mm, res$sig_mm, names=c('mu_mle','sig_mle','mu_mm','sig_mm'))

#n3 

M=100
n=1000 #10000 #100000  
theta=5 
res2=matrix(data=NA,nrow=M,ncol=1)
vars=matrix(data=NA, nrow=M, ncol=3)
x_ha?f = log(1/2)/theta
sigma_2 = (2*(27.36))^2
for (k in 1:M) {
  gamma <- rgamma(n=n, shape=1, scale=theta)
  medx <- median(gamma)
  res_f <- sqrt(n)*(medx - x_half)
  disp <- var(gamma)
  vars [k, 3] <- disp #здесь в зависимости от значения n подставляются ??азные значения колонок
  res2[k,] <- res_f 
} 

compare <- rnorm(n=n, mean=0, sd=1/sigma_2)

qqplot(res2, compare, xlab = "sample distribution", ylab = "theoretical distribution", main = "Q-Q Plot")


data <- data.frame(v1=vars[,1],v2=vars[,2],v3=vars[,3])?library(reshape)
meltData <- melt(data)
boxplot(data=meltData, value~variable)



#n4

data(ibm)
x = ibm 
y=(diff(log(x)))^2
q_norm = function(theta) { 
  -sum(log(dnorm(x=y, mean=0, sd=theta)^2))
}
q_exp = function(lambda) { 
  -sum(log(dexp(x=y, rate=lam?da)))
}

norm_coef <-  mle(q_norm,start=list(theta=1),method="L-BFGS-B",lower=c(0.1))
exp_coef <- mle(q_exp,start=list(lambda=1),method="L-BFGS-B",lower=c(0.1))

try1 <- pnorm(y, mean=0, sd=norm_coef@coef[1])^2
try2 <- pexp(y, rate=exp_coef@coef[1])
par(mf?ow=c(1,3)) 
plot(ecdf(try1))
plot(ecdf(try2))
plot(ecdf(y))
#лучше описывает нормальное распределение 

