#This script introduce to mixed models, specially checking assumptions.

#Mixed models assumptions:
#Assumptions. Mixed models make some important assumptions (we’ll check these later for our examples)

#The observed y are independent, conditional on some predictors x
#The response y are normally distributed conditional on some predictors x
#The response y has constant variance, conditional on some predictors x
#There is a straight line relationship between y and the predictors x and random effects z
#Random effects z are independent of y.
#Random effects z are normally distributed

#packages used
#install.packages("DHARMa")
library(nlme)
library(lme4)
library(MASS)
library(visreg)
library("DHARMa")

#The data
Estuaries <- read.csv("data/Estuaries.csv", header = T)
head(Estuaries)

#nlme vs lme4----
ft.estu <- lme(Total ~ Modification, random = ~ 1 |Estuary, data = Estuaries, method = "REML")

ft.estu4 <- lmer(Total ~ Modification + (1|Estuary),data = Estuaries, REML=T)
#assumptions:
qqnorm(residuals(ft.estu))
scatter.smooth(residuals(ft.estu)~fitted(ft.estu))

qqnorm(residuals(ft.estu4))
scatter.smooth(residuals(ft.estu)~fitted(ft.estu4))

summary(ft.estu)
summary(ft.estu4)

intervals(ft.estu)
confint(ft.estu4)

boxplot(Estuaries$Total ~ Estuaries$Modification)
#useful specilly when responses use a link function.
visreg(ft.estu4, "Modification", by = "Estuary", scale="response")

#when data do not hold the assumptions:----

ft.Hydroid <- lme(Hydroid ~ Modification, random = ~ 1 |Estuary, 
                  data = Estuaries, method = "REML")
qqnorm(residuals(ft.Hydroid))
scatter.smooth(residuals(ft.Hydroid)~fitted(ft.Hydroid))
boxplot(Estuaries$Modification, residuals(ft.Hydroid, type = "norm"))

ft.Hydroid <- lme(Hydroid ~ Modification, random = ~ 1 |Estuary, 
                  data = Estuaries, method = "REML", 
                  weights=varIdent(form=~1|Modification))
qqnorm(residuals(ft.Hydroid, type = "normalized"))
scatter.smooth(residuals(ft.Hydroid, type = "normalized") ~ fitted(ft.Hydroid))

#Plan B:

#binary data:
Estuaries$HydroidPres <- Estuaries$Hydroid > 0

fit.bin <- glmer(HydroidPres ~ Modification + (1|Estuary), 
                 family=binomial, data=Estuaries)

#residual plots are useless here: 
plot(residuals(fit.bin)~fitted(fit.bin),main="residuals v.s. Fitted")
qqnorm(residuals(fit.bin))

#But there is always a solution
simulationOutput <- simulateResiduals(fittedModel = fit.bin, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
testZeroInflation(simulationOutput)

#more on: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html

#Check for overdispersion
#the ratio of residual deviance to degrees of freedom < 1
summary(fit.bin)
boxplot(Estuaries$HydroidPres ~ Estuaries$Modification)
visreg(fit.bin, "Modification")
visreg(fit.bin, "Modification", scale="response")
visreg(fit.bin, "Modification", by = "Estuary", scale="response")

#Poisson
fit.pois <- glmer(Hydroid ~ Modification + (1|Estuary) ,
                  family=poisson, data=Estuaries)
#residuals plots are useless here

simulationOutput <- simulateResiduals(fittedModel = fit.pois, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
testZeroInflation(simulationOutput)

summary(fit.pois)
visreg(fit.pois, "Modification", by = "Estuary", scale="response")

#Plan C: negbin!
hist(Estuaries$Schizoporella.errata)

fit.nb <- glmer.nb(Schizoporella.errata ~ Modification + (1|Estuary), data=Estuaries)

simulationOutput <- simulateResiduals(fittedModel = fit.nb, n = 999)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
testZeroInflation(simulationOutput)

summary(fit.nb)
visreg(fit.nb, "Modification", by = "Estuary", scale="response")

#but:
fit.pois2 <- glmer(Schizoporella.errata ~ Modification + (1|Estuary), 
                   family = poisson, data=Estuaries)

simulationOutput <- simulateResiduals(fittedModel = fit.pois2, n = 250)
plotSimulatedResiduals(simulationOutput = simulationOutput)
testUniformity(simulationOutput = simulationOutput)
testZeroInflation(simulationOutput)

summary(fit.pois2)
visreg(fit.pois2, "Modification", by = "Estuary", scale="response")

#This shows how to get the random slopes and CI's for each level in a hierarchical model----

#dataset used
head(iris)

#what we want to investigate
#Is there a general relationship? and how it differs by species
plot(iris$Sepal.Width ~ iris$Petal.Width, col = iris$Species, las =1)

#Our model with random slope and intercept
m2 <- lmer(data = iris, Sepal.Width ~ Petal.Width + (1 + Petal.Width|Species))
summary(m2)

#extract fixed effects
a=fixef(m2)
a

#extract random effects
b=ranef(m2, condVar=TRUE)
b

# Extract the variances of the random effects
str(b)
qq <- attr(b[[1]], "postVar") 
qq
e=(sqrt(qq)) 
e=e[2,2,] #here we want to access the Petal.Weigth, which is stored in column 2 in b[[1]].
e

#calculate CI's
liminf=(b[[1]][2]+a[2])-(e*2)
liminf

mean_=(b[[1]][2]+a[2])
mean_

limsup=(b[[1]][2]+a[2])+(e*2)
limsup

#Plot betas and its errors
dotchart(mean_$Petal.Width,
         labels = rownames(mean_), cex = 0.5,
         xlim = c(0.4,1.4),
         xlab = "betas")

#add CI's...
for (i in 1:nrow(mean_)){
  lines(x = c(liminf[i,1], 
              limsup[i,1]), y = c(i,i))    
}

#make final plot
plot(iris$Sepal.Width ~ iris$Petal.Width, col = iris$Species, las = 1)
#and plot each random slope
abline(a = b[[1]][1,1]+a[1], b= mean_$Petal.Width[1], col = "black")
abline(a = b[[1]][2,1]+a[1], b= mean_$Petal.Width[2], col = "red")
abline(a = b[[1]][3,1]+a[1], b= mean_$Petal.Width[3], col = "green")
#and general response
abline(a, lty = 2)




