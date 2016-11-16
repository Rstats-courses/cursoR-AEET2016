## ----knitr_setup, include=FALSE, cache=FALSE-----------------------------

library(knitr)

### Chunk options ###

## Text results
opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, eval = TRUE, size = 'footnotesize')

## Code decoration
opts_chunk$set(tidy = FALSE, comment = NA, highlight = TRUE, prompt = FALSE, crop = TRUE)

# ## Cache
opts_chunk$set(cache = TRUE, cache.path = "knitr_output/cache/")

# ## Plots
opts_chunk$set(fig.path = "knitr_output/figures/")
opts_chunk$set(fig.align = 'center')

### Hooks ###
## Crop plot margins
knit_hooks$set(crop = hook_pdfcrop)

## Reduce font size
# see http://stackoverflow.com/a/39961605
knit_hooks$set(smallfont = function(before, options, envir) {
  if (before) return(paste0("\n \\", options$size, "\n\n"))
  else return("\n\n \\normalsize \n")
  })


## ----out.width='4in', out.height='2in'-----------------------------------
include_graphics("images/Clark2005_title.png")

## ----echo=TRUE, eval=FALSE, cache=FALSE----------------------------------
## library(arm)
## library(R2jags)
## library(ggmcmc)
## library(shinystan)
## library(rube)
## library(lme4)
## library(rstan)
## library(rstanarm)

## ----reg1, echo=FALSE, fig.align='left', fig.height=5, fig.width=4-------
data(iris)
setosa <- iris[iris$Species == "setosa", ]
plot(setosa[,3], setosa[,4], xlab = "x", ylab = "y", ylim = c(0,0.65), 
     pch=19, las = 1, cex.lab = 1.5)
abline(lm(setosa[,4] ~ setosa[,3]), lwd = 3)

## ----out.width='2in', out.height='2in'-----------------------------------
include_graphics("images/normal_distr.png")

## ----echo=TRUE-----------------------------------------------------------
trees <- read.csv("trees.csv")
summary(trees[, 1:3])

## ----echo=FALSE----------------------------------------------------------
plot(trees$dbh, trees$height, pch=20, las=1, cex.lab=1.4, xlab="DBH (cm)", ylab="Height (m)")

## ----lm, echo=TRUE, message=FALSE----------------------------------------
simple.lm <- lm(height ~ dbh, data = trees)
arm::display(simple.lm)  # summary of key model elements

## ----echo=TRUE-----------------------------------------------------------
summary(trees$dbh)
trees$dbh.c <- trees$dbh - 25

## ----echo=FALSE, fig.align='left', fig.width=4, fig.height=4-------------
plot(trees$dbh.c, trees$height, pch=20, las=1, cex.lab=1.4, xlab="DBH (cm)", ylab="Height (m)")
abline(lm(height ~ dbh.c, data=trees), col="red", lwd=3)

## ----echo=FALSE, smallfont=TRUE, cache=FALSE-----------------------------
library(arm)
simple.lm <- lm(height ~ dbh.c, data = trees)
display(simple.lm)

## ----echo=TRUE-----------------------------------------------------------
bayes.lm <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau=precision (inverse var)
    mu[i] <- alpha + beta*dbhc[i]    # expected height ~ dbhc
  }

}

## ----echo=TRUE, smallfont=TRUE-------------------------------------------
bayes.lm <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse var)
    mu[i] <- alpha + beta*dbhc[i]    # expected height ~ dbhc
  }
  
  # PRIORS (vague or weakly informative)
  alpha ~ dunif(1, 100)     # prior avg height of 25-cm-DBH tree
  beta ~ dunif(0, 10)       # how much do we expect height to scale with DBH?
  tau <- 1/(sigma*sigma)    # tau = 1/sigma^2
  sigma ~ dunif(0, 50)      # residual standard deviation
}

## ----echo=1, fig.height=3, fig.width=3, fig.align='left'-----------------
plot(density(rnorm(1000, 0, 1000)),   
     main="", xlab="Height (m)")

## ----echo=1, fig.height=3, fig.width=3, fig.align='left'-----------------
plot(density(rnorm(1000, 2, 0.5)),   
      main="", xlab="Height (m)")

## ----echo=TRUE-----------------------------------------------------------
bayes.lm <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha + beta*dbhc[i]    # centred diameter
  }
  
  # PRIORS (vague or weakly informative)
  alpha ~ dunif(1, 100)     # prior for average height of a 25-cm-DBH tree
  beta ~ dunif(0, 10)       # how much do we expect height to scale with DBH?
  tau <- 1/(sigma*sigma)    # tau = 1/sigma^2
  sigma ~ dunif(0, 50)      # residual standard deviation
}

## ----echo=TRUE-----------------------------------------------------------
data <- list(height = trees$height,  # response
             dbhc = trees$dbh.c)     # predictor

## ----echo=TRUE-----------------------------------------------------------
params <- c("alpha", "beta", "sigma")

## ----echo=TRUE, cache=FALSE----------------------------------------------
library(R2jags)

## ----echo=TRUE, results='hide'-------------------------------------------
m1 <- jags(data,  
           model.file = bayes.lm,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,   # JAGS will create inits for each chain
           n.iter = 10,    # number of iterations
           n.burnin = 5)   # iterations to discard (before convergence)

## ----echo=1, fig.height=5, fig.width=6-----------------------------------
traceplot(m1, ask = FALSE, mfrow = c(2, 2))
par(mfrow=c(1,1))

## ----echo=TRUE, results='hide'-------------------------------------------
m1 <- jags(data,  
           model.file = bayes.lm,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 10000,    # 10000 MCMC iterations
           n.burnin = 5000)   # discard first half (5000 iterations)

## ----echo=1--------------------------------------------------------------
traceplot(m1, ask = FALSE, mfrow = c(2, 2))
par(mfrow=c(1,1))

## ----echo=FALSE----------------------------------------------------------
print(m1, intervals=c(0.025, 0.975))

## ------------------------------------------------------------------------
display(simple.lm)

## ----fig.height=7, fig.width=9-------------------------------------------
plot(m1)

## ----echo=TRUE-----------------------------------------------------------
suppressPackageStartupMessages(library(ggmcmc))
m1.mcmc <- as.mcmc(m1)   # Get list of MCMC values
m1.tidy = ggs(m1.mcmc)   # Produce tidy data frame
ggmcmc(m1.tidy)

## ------------------------------------------------------------------------
include_graphics("images/ggmcmc_out1.png")

## ------------------------------------------------------------------------
include_graphics("images/ggmcmc_out2.png")

## ----eval=FALSE, echo=TRUE-----------------------------------------------
## library(shinystan)
## launch_shinystan(as.shinystan(m1.mcmc))

## ----echo=FALSE, eval=FALSE----------------------------------------------
## curve(dunif(x, 1, 100), from = 1, to = 100,
##       lwd = 2, col = "red", main="Height of average 25-cm DBH tree (alpha)", xlab="Height (m)", ylab = "",
##       las = 1, ylim = c(0, 0.1))
## lines(density(m1$BUGSoutput$sims.list$alpha), lwd=2, col="blue")
## legend("topright", c("prior", "posterior"), col = c("red", "blue"), lty = 1, bty = "n")
## 

## ----echo=TRUE, cache=FALSE----------------------------------------------
library(rube)
priPost(post = m1$BUGSoutput$sims.list$alpha[, 1], 
        dist = "Uniform", pripar = c(1, 100))

## ----echo=TRUE-----------------------------------------------------------
priPost(post = m1$BUGSoutput$sims.list$sigma[, 1], 
        dist = "Uniform", pripar = c(0, 50))

## ----echo=FALSE----------------------------------------------------------
alpha.avg <- mean(m1$BUGSoutput$sims.list$alpha)
beta.avg <- mean(m1$BUGSoutput$sims.list$beta)
mu <- alpha.avg + beta.avg*trees$dbh.c
plot(trees$height, mu, 
     xlim = c(0, 60), ylim = c(0, 60),
     xlab = "Observed height (m)", ylab = "Predicted height (m)")
abline(a = 0, b = 1)

## ----echo=TRUE, fig.height=3, fig.width=4--------------------------------
alpha.avg <- mean(m1$BUGSoutput$sims.list$alpha)
beta.avg <- mean(m1$BUGSoutput$sims.list$beta)
mu <- alpha.avg + beta.avg*trees$dbh.c
plot(trees$height, mu, 
     xlim = c(0, 60), ylim = c(0, 60),
     xlab = "Observed height (m)", ylab = "Predicted height (m)")
abline(a = 0, b = 1)

## ----echo=FALSE----------------------------------------------------------
residuals <- trees$height - mu
hist(residuals)

## ----echo=TRUE-----------------------------------------------------------
bayes.lm.N <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha + beta*dbhc[i]    # centred diameter
  }
  
  # PRIORS 
  alpha ~ dnorm(0, 0.01)      # prior for intercept
  beta ~ dnorm(0, 0.01)       # prior for beta (slope)
  tau <- 1/(sigma*sigma)      # tau = 1/sigma^2
  sigma ~ dunif(0, 50)        # residual standard deviation
}

## ----echo=TRUE, results='hide'-------------------------------------------
m1b <- jags(data,  
           model.file = bayes.lm.N,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,  
           n.burnin = 2000)  

## ----echo=FALSE----------------------------------------------------------
print(m1b, intervals=c(0.025, 0.975))

## ----echo=TRUE-----------------------------------------------------------
bayes.lm.N2 <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha + beta*dbhc[i]    # centred diameter
  }
  
  # PRIORS 
  alpha ~ dnorm(10, 10)      # prior for intercept
  beta ~ dnorm(0, 0.01)      # prior for beta (slope)
  tau <- 1/(sigma*sigma)     # tau = 1/sigma^2
  sigma ~ dunif(0, 50)       # residual standard deviation
}

## ----echo=TRUE, results='hide'-------------------------------------------
m1c <- jags(data,  
           model.file = bayes.lm.N2,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,  
           n.burnin = 2000)  

## ----echo=FALSE----------------------------------------------------------
print(m1c, intervals=c(0.025, 0.975))

## ----echo=TRUE-----------------------------------------------------------
priPost(post = m1c$BUGSoutput$sims.list$alpha[,1],
        dist = "Normal", pripar = c(10, 0.1))

## ----echo=FALSE----------------------------------------------------------
#plot <- as.numeric(levels(trees$plot))[trees$plot]
plot.id <- factor(trees$plot)
plot(trees$dbh[plot.id==1], trees$height[plot.id==1], 
     pch=20, las=1, cex.lab=1.4, xlab="DBH (cm)", ylab="Height (m)", col=1,
     ylim=c(0,60))
for(i in 2:10){
  points(trees$dbh[plot.id==i], trees$height[plot.id==i], pch=20, col=i)
}

## ----echo = 1------------------------------------------------------------
lm.plot <- lm(height ~ factor(plot) + dbh.c, data = trees)
display(lm.plot)

## ----single_interc, echo=FALSE, fig.height=5, fig.width=4----------------
plot(height ~ dbh, data=trees, las=1, xlab="DBH (cm)", ylab="Height (m)", ylim = c(0, 60), 
     main = "Pooling all plots")
abline(lm(height ~ dbh, data=trees), lwd=4, col="red")

## ----varying_interc, echo=FALSE, fig.height=5, fig.width=4---------------
lm2 <- lm(height ~ factor(plot) + dbh, data = trees)
plot(trees$dbh[plot.id==1], trees$height[plot.id==1], 
     pch=20, las=1, xlab="DBH (cm)", ylab="Height (m)", col=1,
     ylim=c(0,60), main = "Different intercept for each plot")
abline(a=coef(lm2)[1], b=coef(lm2)[11], col=1, lwd=2)
for(i in 2:10){
  points(trees$dbh[plot.id==i], trees$height[plot.id==i], pch=20, col=i)
  abline(a=coef(lm2)[1] + coef(lm2)[i], b=coef(lm2)[11], col=i, lwd=2)
}

## ----echo=TRUE-----------------------------------------------------------
varint.nopool <- function(){
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha[plot[i]] + beta*dbhc[i]     # centred diameter
  }
  # PRIORS
  #alpha ~ dnorm(0, .001)      # previous model
  for (j in 1:10){
    alpha[j] ~ dnorm(0, .001)  # Plot effects drawn from Normal distribution 
                               # with large **fixed** variance
  }
  beta ~ dnorm(0, .001)
  tau <- 1/(sigma*sigma)       # tau = 1/sigma^2
  sigma ~ dunif(0, 50)
}

## ----echo=TRUE, results='hide'-------------------------------------------
data <- list(height = trees$height, 
             dbhc = trees$dbh.c, 
             plot = trees$plot)
m2 <- jags(data,  
           model.file = varint.nopool,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,  
           n.burnin = 2000) 

## ----echo=TRUE-----------------------------------------------------------
m2 <- jags.parallel(data,  
           model.file = varint.nopool,  
           parameters.to.save = params,  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,  
           n.burnin = 2000) 

## ----echo=FALSE----------------------------------------------------------
print(m2, intervals=c(0.025, 0.975))

## ------------------------------------------------------------------------
display(lm.plot)

## ----fig.height=7, fig.width=9, echo=FALSE-------------------------------
plot(m2)

## ----coefplot1, echo=FALSE-----------------------------------------------
mean.alpha <- apply(m2$BUGSoutput$sims.list$alpha, 2, mean)
sd.alpha <- apply(m2$BUGSoutput$sims.list$alpha, 2, sd)
coefplot(mean.alpha, sd.alpha, cex.var=1.2, cex.pts=1.5, vertical=FALSE,
         main="Average height of a 25-cm DBH tree in each plot", 
         xlab="Trees per plot", ylim=c(20,40), ylab="Height (m)", cex.lab=1.2,
         var.las=1, varnames=summary(factor(trees$plot)))

## ----eval=TRUE-----------------------------------------------------------
include_graphics("images/mixed_models.pdf")

## ----mixed, echo=2, cache=FALSE------------------------------------------
library(lme4)
mixed <- lmer(height ~ dbh.c + (1|plot), data = trees)
display(mixed)

## ----echo=TRUE-----------------------------------------------------------
coef(mixed)

## ----echo=TRUE-----------------------------------------------------------
varint.pool <- function(){
    # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha[plot[i]] + beta*dbhc[i]     # centred diameter
  }
    # PRIORS
  for (j in 1:10){
    alpha[j] ~ dnorm(grandmu, tauplot)  # Now we are estimating the plot variance!
  }
  grandmu ~ dnorm(0, .001)     # Overall mean height across all plots
  tauplot <- 1/(sigmaplot*sigmaplot)
  sigmaplot ~ dunif(0, 20)     # between-plot variance
  beta ~ dnorm(0, .001)
  tau <- 1/(sigma*sigma)       
  sigma ~ dunif(0, 50)         # residual variance
}

## ----echo=TRUE-----------------------------------------------------------
data <- list(height = trees$height,  
             dbhc = trees$dbh.c,  
             plot = trees$plot)
m3 <- jags.parallel(data,  
           model.file = varint.pool,  
           parameters.to.save = c("alpha", "beta", "sigma", "grandmu", "sigmaplot"),  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,  
           n.burnin = 2000) 

## ----echo=FALSE----------------------------------------------------------
print(m3, intervals=c(0.025, 0.975))

## ----fig.height=7, fig.width=9, echo=FALSE-------------------------------
plot(m3)

## ----coefplot1b, echo=FALSE----------------------------------------------
mean.alpha <- apply(m2$BUGSoutput$sims.list$alpha, 2, mean)
sd.alpha <- apply(m2$BUGSoutput$sims.list$alpha, 2, sd)
coefplot(mean.alpha, sd.alpha, cex.var=1.2, cex.pts=2, vertical=FALSE,
         main="Average height of a 25-cm DBH tree in each plot", 
         xlab="Trees per plot", ylim=c(20,40), ylab="Height (m)", cex.lab=1.2,
         var.las=1, varnames=summary(factor(trees$plot)))
mean.alpha <- apply(m3$BUGSoutput$sims.list$alpha, 2, mean)
sd.alpha <- apply(m3$BUGSoutput$sims.list$alpha, 2, sd)
coefplot(mean.alpha, sd.alpha, cex.var=1.2, cex.pts=2, vertical=FALSE,
         add=TRUE, offset=0.15, col="red",
         var.las=1, varnames=summary(factor(trees$plot)))


## ----echo=FALSE, fig.height=5, fig.width=8-------------------------------
par(mfrow=c(1,2))
plot(height ~ dbh, data=trees, las=1, xlab="DBH (cm)", ylab="Height (m)", ylim = c(0, 50), 
     main = "Pooling all plots")
abline(lm(height ~ dbh, data=trees), lwd=4, col="red")

lm2 <- lm(height ~ factor(plot) + dbh, data = trees)
plot(trees$dbh[plot.id==1], trees$height[plot.id==1], 
     pch=20, las=1, xlab="DBH (cm)", ylab="Height (m)", col=1,
     ylim=c(0,50), main = "Different intercept for each plot")
abline(a=coef(lm2)[1], b=coef(lm2)[11], col=1, lwd=2)
for(i in 2:10){
  points(trees$dbh[plot.id==i], trees$height[plot.id==i], pch=20, col=i)
  abline(a=coef(lm2)[1] + coef(lm2)[i], b=coef(lm2)[11], col=i, lwd=2)
}
par(mfrow=c(1,1))

## ----read_plotdata, echo=TRUE, message=FALSE-----------------------------
plotdata <- read.csv("plotdata.csv")
temp.c <- plotdata$temp - 15

## ----echo=TRUE-----------------------------------------------------------
group.preds <- function(){
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    
    mu[i] <- alpha[plot[i]] + beta*dbhc[i]    
  }
  # PRIORS
  for (j in 1:10){
    alpha[j] ~ dnorm(grandmu + beta.temp*tempc[j], tauplot)   
  }
  beta.temp ~ dnorm(0, .001)   # slope for temperature effects
  grandmu ~ dnorm(0, .001)     
  tauplot <- 1/(sigmaplot*sigmaplot)
  sigmaplot ~ dunif(0, 20)    
  beta ~ dnorm(0, .001)
  tau <- 1/(sigma*sigma)       
  sigma ~ dunif(0, 50)     
}

## ----echo=TRUE, message=FALSE--------------------------------------------

data <- list(height = trees$height, 
             dbhc = trees$dbh.c, 
             plot = trees$plot, 
             tempc = temp.c)
params = c("alpha","beta","sigma", "grandmu", "sigmaplot", "beta.temp")
m4 <- jags.parallel(data,
           model.file = group.preds,
           parameters.to.save = params,
           n.chains = 3,
           inits = NULL,
           n.iter = 4000,
           n.burnin = 2000) 


## ----echo=FALSE, message=FALSE-------------------------------------------
mean.alpha <- apply(m4$BUGSoutput$sims.list$alpha,2, mean)
plot(plotdata$temp, mean.alpha, pch=20, xlab="Temperature", 
     main="Average height of a 25-cm DBH tree in the plot", ylab="Height (m)", 
     cex.lab=1.2, las=1, cex=1.2)


## ----echo=FALSE----------------------------------------------------------
mean.alpha <- apply(m2$BUGSoutput$sims.list$alpha,2, mean)
sd.alpha <- apply(m2$BUGSoutput$sims.list$alpha,2, sd)
coefplot(mean.alpha, sd.alpha, cex.var=1, cex.pts=1.5, vertical=FALSE,
         main="Average height of a 25-cm DBH tree in each plot", 
         xlab="Trees per plot", ylim=c(20,40), ylab="Height (m)", cex.lab=1,
         var.las=1, varnames=summary(factor(trees$plot)))
mean.alpha <- apply(m4$BUGSoutput$sims.list$alpha,2, mean)
sd.alpha <- apply(m4$BUGSoutput$sims.list$alpha,2, sd)
coefplot(mean.alpha, sd.alpha, cex.var=1, cex.pts=1.5, vertical=FALSE,
         add=TRUE, offset=0.15, col="red",
         var.las=1, varnames=summary(factor(trees$plot)))


## ----fig.align='left', fig.height=4, fig.width=4, echo=2-----------------
x=seq(from=10, to=20, length.out=100)
data1=rnorm(100, 2 + 1.6*x, 5)
hist(data1)

## ----fig.align='left', fig.height=4, fig.width=4, echo=1-----------------
data2=rnorm(100, 2 + 1.6*x, 5)
hist(data2)

## ----echo=FALSE----------------------------------------------------------
plot(trees$dbh, trees$dead, xlab="DBH", ylab="Dead", pch=20)

## ----echo=TRUE-----------------------------------------------------------
logreg <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(dead)){
    dead[i] ~ dbern(pdeath[i])
    logit(pdeath[i]) <- mu + beta*dbhc[i]
  }
  
  # PRIORS
  mu ~ dnorm(0, .001)
  beta ~ dnorm(0, .001)
}

## ----echo=TRUE, results='hide'-------------------------------------------
data <- list(dead = trees$dead,   
             dbhc = trees$dbh.c)
m5 <- jags(data,  
           model.file = logreg,  
           parameters.to.save = c("mu", "beta"),  
           n.chains = 3,  
           inits = NULL,  
           n.iter = 4000,
           n.burnin = 2000) 

## ----echo=FALSE----------------------------------------------------------
print(m5, intervals = c(0.025, 0.975))
# plot(trees$dbh, trees$dead, xlab="DBH", ylab="Dead", pch=20)
# curve(plogis(-3.46+0.05*x), from=5, to=50, add=TRUE, lwd=2)

## ----echo=1--------------------------------------------------------------
logreg <- glm(dead ~ dbh.c, data = trees, family = binomial)
display(logreg)

## ----echo=TRUE-----------------------------------------------------------
bayes.lm <- function(){
  
  # LIKELIHOOD
  for (i in 1:length(height)){
    height[i] ~ dnorm(mu[i], tau)    # tau = precision (inverse of variance)
    mu[i] <- alpha + beta*dbhc[i]    # centred diameter
  }
  
  # PRIORS (vague or weakly informative)
  alpha ~ dunif(1, 100)     # prior for average height of a 25-cm-DBH tree
  beta ~ dunif(0, 10)       # how much do we expect height to scale with DBH?
  tau <- 1/(sigma*sigma)    # tau = 1/sigma^2
  sigma ~ dunif(0, 50)      # residual standard deviation
}

## ----echo=TRUE-----------------------------------------------------------
bayes.lm.stan <- "
 
  data {
    int<lower=0> N;
    vector[N] dbhc;
    vector[N] height;
  }

  parameters {
    real<lower=0, upper=100> alpha;
    real<lower=0, upper=10> beta;
    real<lower=0, upper=50> sigma;
  }
  
  model {
    height ~ normal(alpha + beta * dbhc, sigma);
  }
"

## ----message=FALSE, echo=TRUE, results='hide'----------------------------
library(rstan)
lm.stan <- stan(model_code = bayes.lm.stan, 
                data = list(height = trees$height, dbhc = trees$dbh.c, 
                            N = nrow(trees)),
                pars = c("alpha", "beta", "sigma"),
                chains = 3,
                iter = 2000,
                init = "random")

## ----echo=FALSE----------------------------------------------------------
lm.stan

## ----echo=FALSE, out.width="4in", out.height="3in"-----------------------
include_graphics("images/rstanarm.png")

## ----message=FALSE, echo=TRUE, results='hide'----------------------------
library(rstanarm)
easybayes <- stan_glm(height ~ dbh.c, data = trees, family = gaussian,
                      prior_intercept = normal(30, 30),
                      prior = normal(0, 10))

## ------------------------------------------------------------------------
easybayes

## ------------------------------------------------------------------------
include_graphics("images/thinkinator.png")

