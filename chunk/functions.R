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
