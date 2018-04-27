# External code for datasets
# read in all at once, kept in one file to reduce duplication
# https://www.zevross.com/blog/2014/07/09/making-use-of-external-r-code-in-knitr-and-r-markdown/

## @knitr shape_data
num.one = 90 # base number of features

m.dub <- prop.m <- clr.m <- m.dub.u <- prop.m.u <- clr.m.u <-
    matrix(data=NA, nrow=20, ncol=num.one + 10)

in.put <- c(10,10000,1,5,10,20,50,100,200,1000) # arbitrary data

# ensure feature 3 is arbitrarily large and get the constrained total
total.sum <- sum(in.put + 1) * 1000

# one feature increases exponentially and one feature decreases to compensate
for(i in 0:19){
	# un-constrained data, feature 1 exponentially increases
	m.dub[i+1,] <- m.dub.u[i+1,] <- in.put * c(2^i, rep(1,num.one + 9))
	prop.m.u[i+1,] <- m.dub.u[i+1,]/sum(m.dub.u[i+1,])
	clr.m.u[i+1,] <- 2^(log2(prop.m.u[i+1,]) - mean(log2(prop.m.u[i+1,])))
    # now constrain the data, feature 3 decreases to compensate
	m.dub[i+1,3] <- total.sum - sum(m.dub[i+1,])
	prop.m[i+1,] <- m.dub[i+1,] / sum(m.dub[i+1,])
	clr.m[i+1,] <- log2(prop.m[i+1,]) - mean(log2(prop.m[i+1,]))
}


## @knitr TLRA_data
set.seed(13)
T <- rnorm(50, mean=100, sd=25)
L <- rnorm(50, mean=10000, sd=2500)
R <- rnorm(50, mean=1000, sd=250)
A <- rnorm(50, mean=5, sd=2.5)
ran.dat <- cbind(T,L,R,A)
ran.dat[ran.dat <=0 ] <- 0.1


## @knitr biplot_data
set.seed(7)
# runif(n, min, max)
# rnorm(n, mean, sd)
A <- c(rnorm(20, 20, 5), rnorm(10, 10, 2.5))
B <- c(rnorm(20, 10, 2.5), rnorm(10, 20, 5))
C <- B * runif(30, 4000, 6000) # perturb B by random uniform amount
D <- B ^ runif(30, 1.4, 1.5) # power by by small amount
E <- rnorm(30, 25, 6.25)
set.seed(6)
F <- rnorm(30, 25, 6.25)
G <- rnorm(30, 100, 25)
H <- rnorm(30, 1000, 250)
I <- rnorm(30, 2000, 500)

a_i <- cbind(A,B,C,D,E,F,G,H,I)
a_i[a_i<0] <-0.01

# convert to proportions
a_i.prop <- t(apply(a_i, 1, function(x){x/sum(x)}))

# clr transform
a_i.clr <- t(apply(a_i.prop, 1, function(x){log(x) - mean(log(x))}))

