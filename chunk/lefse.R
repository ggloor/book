a <- read.table("129.dms", sep="\t")

d <- data.frame(a[6:nrow(a),2:ncol(a)], stringsAsFactors=F)
d <- apply(d, 2, as.numeric)

rownames(d) <- a[6:nrow(a), 1]

order_d <- data.frame(d[,a[2,2:ncol(a)] == "gut"],
                       d[,a[2,2:ncol(a)] == "ear"],
                       d[,a[2,2:ncol(a)] == "oral"],
                       d[,a[2,2:ncol(a)] == "nasal"],
                       d[,a[2,2:ncol(a)] == "vagina"],
                       d[,a[2,2:ncol(a)] == "skin"]
                       )

nms_d <- c(paste("G", which(a[2,2:ncol(a)] == "gut"), sep=""),
     paste("E", which(a[2,2:ncol(a)] == "ear"), sep=""),
     paste("O", which(a[2,2:ncol(a)] == "oral"), sep=""),
     paste("N", which(a[2,2:ncol(a)] == "nasal"), sep=""),
     paste("V", which(a[2,2:ncol(a)] == "vagina"), sep=""),
     paste("S", which(a[2,2:ncol(a)] == "skin"), sep="")
     )
colnames(order_d) <- nms_d

# so LEfSe checks all taxonomic levels at once ugh
# pull out only those taxa that have 5 levels of annotation

z <- sapply(strsplit(rownames(order_d), "\\|"), length)

d.6 <- order_d[z == 6,]

d.clr <- apply(d.6, 2, function(x) log(x) - mean(log(x)))

pcx <- prcomp(t(d.clr))

biplot(pcx, cex=c(1,0.3), var.axes=F)

##### remove Oral and Gut and replot
##### the vaginal samples separate, but not by much
order_d <- data.frame(d[,a[2,2:ncol(a)] == "ear"],
                       d[,a[2,2:ncol(a)] == "nasal"],
                       d[,a[2,2:ncol(a)] == "vagina"],
                       d[,a[2,2:ncol(a)] == "skin"]
                       )

nms_d <- c(paste("N", which(a[2,2:ncol(a)] == "nasal"), sep=""),
     paste("V", which(a[2,2:ncol(a)] == "vagina"), sep=""),
     paste("S", which(a[2,2:ncol(a)] == "skin"), sep="")
     )
colnames(order_d) <- nms_d

# so LEfSe checks all taxonomic levels at once ugh
# pull out only those taxa that have 5 levels of annotation

z <- sapply(strsplit(rownames(order_d), "\\|"), length)

d.6 <- order_d[z == 6,]

d.clr <- apply(d.6, 2, function(x) log(x) - mean(log(x)))

pcx <- prcomp(t(d.clr))

biplot(pcx, cex=c(1,0.3), var.axes=F)

#####now separate by oxygen
order_d <- data.frame(d[,a[1,2:ncol(a)] == "Low_O2"],
                       d[,a[1,2:ncol(a)] == "High_O2"],
                       d[,a[1,2:ncol(a)] == "Mid_O2"]
                        )

nms_d <- c(paste("L", which(a[1,2:ncol(a)] == "Low_O2"), sep=""),
     paste("H", which(a[1,2:ncol(a)] == "High_O2"), sep=""),
     paste("M", which(a[1,2:ncol(a)] == "Mid_O2"), sep="")
     )
colnames(order_d) <- nms_d

z <- sapply(strsplit(rownames(order_d), "\\|"), length)

d.6 <- order_d[z == 6,]

d.clr <- apply(d.6, 2, function(x) log(x) - mean(log(x)))

pcx <- prcomp(t(d.clr))

biplot(pcx, cex=c(1,0.3), var.axes=F)

