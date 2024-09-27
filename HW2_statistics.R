#HW2 

#task 1.1 
data(RTdata)

x <- RTdata$rt1
br_names <- c('Scott', 'Sturges', 'FD')
for (br in br_names) {
  hist(x, breaks=br, main=paste('Histogram with breaks by', br))
}

# task 1.2
kernels <- c("gaussian", "epanechnikov", "rectangular",
             "triangular", "biweight",
             "cosine", "optcosine")
bws <- c("nrd0", "nrd", "ucv", "bcv", "SJ")
random_color <- function() {
  color <- paste0("#", sprintf("%06X", sample(0:0xFFFFFF, 1)))
  return(color)
}

color <- vector(mode = 'character', length=7)
for (i in 1:7) {
  color[i] <- random_color()
}

#dif kernels
plot(density(x, kernel=kernels[1], bw='nrd0'))
for (i in seq_along(kernels)) {
  lines(density(x, kernel=kernels[i], bw='nrd0'), col=color[i])
}
legend('topright', legend=kernels, col=color, pch = 16)

#dif bandwidths
plot(density(x, kernel=kernels[1], bw='ucv'))
for (i in seq_along(bws)) {
  lines(density(x, kernel=kernels[1], bw=bws[i]), col=color[i])
}
legend('topright', legend=bws, col=color, pch = 16)

#task 1.3
rng <- seq(11, 20)
dens_mat <- matrix(nrow=12, ncol=512)
for (i in seq_along(bws)) {
  dens <- density(x, kernel=kernels[1], bw=bws[i])$y
  dens_mat[i, ] <- dens
}
for (i in seq_along(kernels)) {
  dens <- density(x, kernel=kernels[i], bw='nrd0')$y
  dens_mat[i+5, ] <- dens
}

min_res <- 100000
idx <- 0
num_b <- 0
for (k in rng) {
  h <- hist(x, breaks=k, plot=FALSE)$density
  for (i in 1:12) {
    l <- (h - dens_mat[i,])
    res <- sum(l^2)/length(l)
    if (res < min_res){
      min_res = res
      idx=i
      num_b=k
    }
  }
}

#лучший кернел - гауссиан и bw.nrd, гистограмма 11
hist(x, breaks=11, prob = TRUE)
lines(density(x, kernel='gaussian', bw='nrd'))


#task 3.2 
custom_density <- function(x) {
  ifelse(x > 0, sqrt(2/(pi)) * exp(-x^2/2), 0)
}

custom_cdf <- function(x) {
  ifelse(x <= 0, 0, integrate(custom_density, 0, x)$value)
}

custom_inverse_cdf <- function(p) {
  uniroot(function(x) custom_cdf(x) - p, lower = 0, upper = 10)$root
}

generate_random_samples <- function(n) {
  u <- runif(n)
  samples <- sapply(u, custom_inverse_cdf)
  return(samples)
}

x <- generate_random_samples(1000)
library(ReIns)

est_h=hill(x)
est_m=moment(x)
k=seq(from=400,to=1000,by=1)
plot(est_h$gamma[k]~k,type="l")
plot(est_m$gamma[k]~k,type="l")
library(evmix)
pickandsplot(x)

# task 3.2(c)
maxes <- vector(mode='numeric', length=1000)
for (i in 1:1000) {
  maxes[i] <- max(generate_random_samples(x))
}
library(evd)
wb <- rweibull(n = 1000, shape = 2, scale = 1)
fr <- rfrechet(n=1000, shape=2, scale=1)
gm <- rgumbel(n=1000)
qqplot(maxes, wb)
qqplot(maxes, fr)
qqplot(maxes, gm)
