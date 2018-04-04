# gives the same answer as DESeq estimateSizeFactors
# DESeq normalization method
# from https://github.com/hbc/knowledgebase/wiki/Count-normalization-methods
# one cannot fully comprehend something until
# insert quote from Richard MacDuff from Dirk Gently here
des.norm <- function(data){ # x is a matrix with samples by column
	# calculate the feature-wise geometric mean
	G <- apply(data, 1, function(x) exp(mean(log(x)))  )
	# calculate the ratio between G and all features
	rat <- apply(data, 2, function(x) x/G)
	# get the median ratio per sample
	norm <- apply(rat, 2, median)

	# scale each read by the median ratio per sample
	norm.counts <- t(apply(data, 1, function(x) x/norm))
	return(norm.counts)
}

# from ALDEx2
# tldr: from the mind of Andrew Fernandes
rdirichlet <- function (n, alpha)
{
  if(length(n) > 1) n <- length(n)
  #if(length(n) == 0 || as.integer(n) == 0) return(numeric(0))
  #n <- as.integer(n)
  if(n < 0) stop("value(n) can not be negative in rtriang")

  if(is.vector(alpha)) alpha <- t(alpha)
  l <- dim(alpha)[2]
  x <- matrix(rgamma(l * n, t(alpha)), ncol = l, byrow=TRUE)  # Gere le recycling
  return(x / rowSums(x))
}

# Jensen-Shannon distance
# actually square root of Jensen-Shannon Divergence
# from http://enterotype.embl.de/enterotypes.html

dist.JSD <- function(inMatrix, pseudocount=0.000001, ...) {
	KLD <- function(x,y) sum(x *log(x/y))
	JSD<- function(x,y) sqrt(0.5 * KLD(x, (x+y)/2) + 0.5 * KLD(y, (x+y)/2))
	matrixColSize <- ncol(inMatrix)
	matrixRowSize <- nrow(inMatrix)
	colnames <- colnames(inMatrix)
	resultsMatrix <- matrix(0, matrixColSize, matrixColSize)

  inMatrix = apply(inMatrix,1:2,function(x) ifelse (x==0,pseudocount,x))

	for(i in 1:matrixColSize) {
		for(j in 1:matrixColSize) {
			resultsMatrix[i,j]=JSD(as.vector(inMatrix[,i]),
			as.vector(inMatrix[,j]))
		}
	}
	colnames -> colnames(resultsMatrix) -> rownames(resultsMatrix)
	as.dist(resultsMatrix)->resultsMatrix
	attr(resultsMatrix, "method") <- "dist"
	return(resultsMatrix)
 }
