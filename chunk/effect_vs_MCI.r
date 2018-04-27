library(ALDEx2)
data(selex)
mc.instances <- numMCInstances(x)
n.features <- numFeatures(x)
n.samples <- length(getSampleIDs(x))

effect.matrix =  matrix(data=NA, nrow = 1600, ncol = 12)
ol.matrix =  matrix(data=NA, nrow = 1600, ncol = 12)
setA <- rep("N", 7)
setB <- rep("S", 7)

conds <- c(setA, setB)

for(i in 1:12){

    x <- aldex.clr(selex, conds=conds, mc.samples=2^i)
	print(2^i)
	x.e <- aldex.effect(x, conds)
	effect.matrix[,i] <- x.e$effect
	ol.matrix[,i] <- x.e$overlap

	# tabulate the values
	# bh.cor <- bh.cor + ( adj.cor / mc.instances )
	# mean.p <- mean.p + cor.rho / mc.instances
}

colnames(effect.matrix) <- paste("r", 2^seq(1:12), sep="_")
rownames(effect.matrix) <- rownames(selex)

 plot(2^seq(1:11),effect.matrix[1,12]-effect.matrix[1,1:11], log="x", type="l", ylim=c(-10, 10), col=rgb(0,0,0,0.1))

 for(i in 2:1600){
  if(effect.matrix[i,1] > 2) points(2^seq(1:11),effect.matrix[i,12] - effect.matrix[i,1:11],pch=19,col=rgb(1,0,0,0.2))
  if(effect.matrix[i,1] <= 2) points(2^seq(1:11),effect.matrix[i,12] - effect.matrix[i,1:11], pch=19,col=rgb(0,0,0,0.1))
  }
  abline(h=1, col="blue", lty=2)
  abline(h=-1, col="blue", lty=2)

   plot(2^seq(1:11),ol.matrix[1,12]-ol.matrix[1,1:11], log="x", pch=19, ylim=c(-0.5, 0.5), col=rgb(0,0,0,0.1))

 for(i in 2:1600){
  if(ol.matrix[i,1] <0.01) points(2^seq(1:11),ol.matrix[i,12]-ol.matrix[i,1:11], pch=19,col=rgb(1,0,0,1))
  if(ol.matrix[i,1] >=0.01) points(2^seq(1:11),ol.matrix[i,12]-ol.matrix[i,1:11], pch=19,col=rgb(0,0,0,0.1))
  }
  abline(h=2, col="blue", lty=2)
