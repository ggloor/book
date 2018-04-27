# External code for datasets

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
