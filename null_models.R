#This script plays with null models as a way to practice more loops.

#A simple null model used to test correlations----

#The data
abundance <- c(1,3,4,7,8,13)
body_size <- c(9,6,3,3,1,1)
#plot it and do correlation

#The null model: 
#idea: Brake all procesess except for the proces of interest and compare the 
#observed with the expected data.

#In this case we want to test if this correlation may be due to randomness.
#We brake then the "body_size" process and left only randomness. use `sample()`


#we do it 1000 times



#plot null and observed data


#test p-value ('pnorm')


#Another example----
#We observe a pattern: How uneven are abundance distributions?
abundance <- c(1,3,4,7,8,13)
#calculate pielou's evenees (shannon/logarithm of number of species)
#Shannon The proportion of species i relative to the total number of species 
#(pi) is calculated, and then multiplied by the natural logarithm of 
#this proportion (lnpi). The resulting product is summed across species, 
#and multiplied by -1:



#is this eveness higher than expected?
#calculate evenness of a random assembly
#tip: we need to sample 36 individuals and assign them a species randomly
#then, we can group per species.


#Calculate J of the simulated community


# now make it 1000 times


#calculate p-value

#null model 2. 
#We want to test now if body size is driving the eveness patterns.
#we create a null model where body_size is the only responsible of the 
#observed pattern
abundance <- c(1,3,4,7,8,13)
body_size <- c(9,6,5,3,2,1)
#Do one iteration

# make a loop

#And test significance



#Other implmented null models in R
library(vegan)
library(bipartite)

#vegan example
## Use quantitative null models to compare
## mean Bray-Curtis dissimilarities
data(dune)
meandist <- function(x) mean(vegdist(x, "bray"))
mbc1 <- oecosimu(dune, meandist, "r2dtable")
mbc1


#bipartite example
data(Safariland)
head(Safariland)
nullmodel(Safariland, N=2, method=1)
nullmodel(Safariland>0, N=2, method=4)
# analysis example:
obs <- unlist(networklevel(Safariland, index="weighted nestedness"))
nulls <- nullmodel(Safariland, N=100, method=1)
null <- unlist(sapply(nulls, networklevel, index="weighted nestedness")) #takes a while ...

plot(density(null), xlim=c(min(obs, min(null)), max(obs, max(null))), 
     main="comparison of observed with null model Patefield")
abline(v=obs, col="red", lwd=2)    

praw <- sum(null>obs) / length(null)
ifelse(praw > 0.5, 1-praw, praw)    # P-value



#further reading:

#neutral model Hubell: http://artax.karlin.mff.cuni.cz/r-help/library/untb/html/expected.abundance.html
#EcoSIm: http://www.uvm.edu/~ngotelli/EcoSim/Niche%20Overlap%20Tutorial.html
#vegan: http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/oecosimu.html