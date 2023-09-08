---
title: "exploring compositionality"
shorttitle: "compo"
author:
- name: Greg Gloor
output:
  BiocStyle::html_document:
    toc_float: true
---

# Main arguments about compositionality

- the total number of reads can vary between samples (CGPT ANDERS 2010)
- modelling as CoDa leads to loss of info (McMurdie 2014)
- CoDa can't handle sparsity leading to misleading 
- CoDa does not model the underlying bio process (Weiss, 2017, but see Fostr 2021 that no model does this!)

So the major criticism is that the number of reads varies between samples. Observing the change in correlation structure is the simplest test for compositionality. 

```{r make-random, }
# make 50 random samples with 4 parts
set.seed(13) # set the random seed
T <- rnorm(50, mean=100, sd=25)
L <- rnorm(50, mean=10000, sd=2500) 
R <- rnorm(50, mean=1000, sd=250)
A <- rnorm(50, mean=5, sd=2.5) 
ran.dat <- data.frame(cbind(T,L,R,A) )
ran.dat[ran.dat <=0 ] <- 0.1 # need positive real data

plot(ran.dat)
# A and R look related (spurious)
# 
plot(ran.dat$R, ran.dat$A)
abline(lm(ran.dat$A ~ ran.dat$R))
cor(ran.dat)
# not significant
# this is the underlying random data
```

As you should be able to see, with this simple dataset, we have no association between any pair of parts.

```{r rarefy-fun}
rarify <- function(mat, n.samples=1000, max.count=FALSE){
  r.int <- round(mat)
  out <- matrix(data=NA, nrow=nrow(mat), ncol=ncol(mat))
  for(i in 1:nrow(mat)){
    jnk.vec <- c(rep("T", r.int[i,1]), rep("L", r.int[i,2]), rep('R', r.int[i,3]), rep("A", r.int[i,4]))
    # fixed upper limit
    n.samples <- round(rnorm(1, n.samples))
    
    # no upper limit
    if(max.count == F){
      n.samples = round(sum(mat[i,]) * .75) 
    } else if(max.count == T){
      n.samples = round(sum(mat[i,]) * .75)
    n.samples <- round(rnorm(1, n.samples))

    sam <- sample(jnk.vec, n.samples)
    n.T = length(which(sam=="T"))
    n.L = length(which(sam=="L"))
    n.R = length(which(sam=="R"))
    n.A = length(which(sam=="A"))
    out[i,] <- c(n.T, n.L, n.R, n.A)
  }
  return(out)
}

r.mat <- as.data.frame(rarify(ran.dat, 6000))
plot(r.mat)
```
#
# convert to proportions by row
# t() on the apply with margin 1 because R sucks
rd.p <- as.data.frame(t(apply(ran.dat, 1, function(x) x/sum(x))))

# a dumber version of apply
make.prop <- function(mat){
  # take a matrix or data frame and return a 
  # per row matrix of proportions
  out <- matrix(data=NA, nrow=nrow(mat), ncol=ncol(mat))
   for(i in 1:nrow(mat)){
    # convert to a single vector of numbers
    out[i,] <- as.numeric(mat[i,]/sum(mat[i,]))
    #print(mat[i,])
    # print(out[i,])

  }
  return(out)
}


mp.p <- make.prop(ran.dat)

plot(mp.p[,1], rd.p[,1])

md.p <- as.data.frame(mp.p)
colnames(md.p) <- c("T","L","R","A")

plot(md.p)

# lets downsample

# make a vector the size of the counts for each sample, assuming each measurement is an integer


