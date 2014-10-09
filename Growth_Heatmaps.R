# Growth Rate Heatmaps

#HMG <- read.delim("/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/HEM1_heatmap.txt", row.names=1)
#HMGh <- read.delim("/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/hem1-_heatmap.txt", row.names=1)
HMG <- read.delim("/Users/JacobMayfield/Documents/CBS Paper 2011/HEM1_heatmap.txt", row.names=1)
HMGh <- read.delim("/Users/JacobMayfield/Documents/CBS Paper 2011/hem1-_heatmap.txt", row.names=1)

attach(HMG)
attach(HMGh)
HMGM=data.matrix(HMG, rownames.force="ID")
HMGMh=data.matrix(HMGh, rownames.force="ID")

library(gplots)

#pdf(file="/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/HEM1_heatmap.pdf", height=15, width=15)
pdf(file="/Users/JacobMayfield/Documents/CBS Paper 2011/HEM1_heatmap.pdf", height=15, width=15)

brks <- seq(0,150,5)
clr=NULL
clrs=NULL
for (i in 1:20)
	{
		clr.i <- colorpanel(21, "blue", "yellow", "grey")[i]
		clrs[i] <- cbind(clr.i)
	}
colors <- c(clrs,
            rep(colorpanel(21, "blue", "yellow", "grey")[21], 10))
heatmap.2(HMGM, Rowv=T, Colv=F, dendrogram="row", col=colors, breaks=brks, scale="none", key=TRUE, symkey=FALSE, density.info="none", trace="none", labRow=row.names(HMG), margins=c(10,15), cexRow=1.25, cexCol=1.25, keysize=0.6)
dev.off()

#pdf(file="/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/hem1-_heatmap.pdf", height=15, width=15)
pdf(file="/Users/JacobMayfield/Documents/CBS Paper 2011/hem1-_heatmap.pdf", height=15, width=15)

brksh <- seq(0,115,5)
clrh=NULL
clrsh=NULL
for (i in 1:20)
	{
		clrh.i <- colorpanel(21, "blue", "yellow", "grey")[i]
		clrsh[i] <- cbind(clrh.i)
	}
colorsh <- c(clrsh,
            rep(colorpanel(21, "blue", "yellow", "grey")[21], 3))
heatmap.2(HMGMh, Rowv=T, Colv=F, dendrogram="row", col=colorsh, breaks=brksh, scale="none", key=TRUE, symkey=FALSE, density.info="none", trace="none", labRow=row.names(HMGh), margins=c(10,15), cexRow=1.25, cexCol=1.25, keysize=0.6)
dev.off()


# Impute zeros
zeros <- function(d) 
 	{
 	nz <- apply(d, 1, function(z) !any(z==0))
 	dnz <- d[nz, ]
 	m <- apply(d,2, function(z) min(z[z!=0]))
 	dm <- apply(d,2, function(z)
 		{
 			m <- min(z[z!=0])
 			z[z==0]<-runif(sum(z==0),0,m)
			z})
 			list(dnz=dnz,dm=dm,m=m,nz=colMeans(d==0))
 	}

d1 <- HMGM
z1 <- zeros(d1)
HMGM.z <- data.frame(z1[2])

#Add to heatmap.2
as.matrix(HMGM.z)

# breaks = seq(start,stop,by)
# COLS <- c(rep(heat.colors(4)[1], 10),       # 0.00-0.01 bin  (10 bins)
          rep(heat.colors(4)[2], 40),       # 0.01-0.05 'bin' (40 bins)
          rep(heat.colors(4)[3], 25),       # 0.05-0.075 'bin' (25 bins)
          rep(heat.colors(4)[4], 25),       # 0.075-0.1 'bin' (25 bins)
          rep("gray", (length(BRKS)-100)-1))# 0.10-1.0 'bin' (100 bins)