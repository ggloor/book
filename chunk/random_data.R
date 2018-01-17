# random data with three features
# used for distance methods


library(Ternary)


# make random data - random uniform is OK
# samples by row, features by column

set.seed(42)
a <- as.integer(runif(100, min=100, max=200))
b <- as.integer(runif(100, min=1000, max=10000))
c <- as.integer(runif(100, min=500, max=2000))

rpkm <- function(x){
	# x is a matrix with samples by rows
	# scale each feature in each sample by dividing by the total
	# and multiplying by a constant - 1000 in this case
	# feature 2 is further divided by 2 to model a single long feature
	# feature 3 is further multipled by 2 to model a single short feature
	# rpkm = K*C/(N*L). K is constant, C is count of feature, N is total
	# reads per sample, L is length in bp
	# reduces to C/(NL) as all other values are scaling constants

	rpkm <- apply(x, 1, function(r) r / (sum(r)))
	return(t(rpkm * c(1,0.5,2)))
}

abc <- cbind(a,b,c)
plot(a,b)

#
abc.prop.clr <- t(apply(abc, 1, function(x) log(x) - mean(log(x) )))
abc.prop <- t(apply(abc, 1, function(x) x/sum(x) ))
abc.prop.clr <-  t(apply(abc.prop, 1, function(x) log(x) - mean(log(x) )))
abc.des <- t(des.norm(t(abc)))
abc.prop.des <- t(des.norm(t(abc.prop)))
abc.rpkm <- rpkm(abc)
abc.prop.rpkm <- rpkm(abc.prop)

dist.abc <- as.matrix(dist(abc))
dist.abc.prop <- as.matrix(dist(abc.prop))
dist.abc.clr <- as.matrix(dist(abc.clr))
dist.abc.prop.clr  <- as.matrix(dist(abc.prop.clr))
dist.abc.des <- as.matrix(dist(abc.des))
dist.abc.prop.des <- as.matrix(dist(abc.prop.des))
dist.abc.rpkm <- as.matrix(dist(abc.rpkm))

TernaryPlot(points, abc)
