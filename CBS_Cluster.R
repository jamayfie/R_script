###########################################################################
# Name:     CBS_Cluster.R
# Author:   Jacob Mayfield
# Date:     2/11/2013
# Description:  
#		Script for soft cluster analysis of CBS growth rate data using Mfuzz.
#		Growth rate data from Table S1 is used for analysis.
#		Nonfunctional alleles are added to S1 with growth rates of zero.
#
#		Allele names are appended with additional information after the underscore:
#		MA=major allele, H=High, I=intermediate, L=low according to heatmap
#		HR=rescue by the hem1 deletion (does not grow in HEM1 strain)
#		Remediation according to heatmap indicated by growth rate (H,I,L)
#		followed by B6=B6, He=Heme  or B6&He=Both
#		NF=nonfunctional (does not grow)
#
# 		The script generates two figures and a table
# 		1) PCA analysis of clusters
# 		2) Plot of cluster centers and clusters
#		3) Table of growth rates, modified with cluster membership values
#
# 		For further analysis, the useful tidbits are:
# 		1) Clusters.  Although semi-arbitrary because I choose the number of clusters
#		   and the "fuzzyness", membership is not arbitrary.  Here I selected
#		   parameters based on reasonable performance.  With 4 clusters, nonfunctional,
#		   low growth, intermediate growth, and high growth alleles cluster together.
#		   Remedial alleles fit in between.
# 		2) Membership values.  This is the real benefit.  A quantitative measurement.
#		   The membership value is akin to Pearson's R, with values closer to 1
#		   indicating better membership.  > 0.7 is good, > 0.5 tenuous.  With soft
#		   clustering, an allele may belong to more than one cluster (remedial)
#		3) Table.  I put together a quick table that includes growth rate, membership
#		   to each cluster, the "best" cluster for each allele, clinical results, and
#		   enzyme assay data.
#
# Usage: CBS_Cluster.R file to execute the script by cut and paste or CMD + Return
###########################################################################

library(Mfuzz)

# I hacked the mfuzz package to allow specification of the axes on plots:
source("/Users/JacobMayfield/Documents/R Scripts/mfuzz.plot.2.R")

CBS_GR <- read.table("/Users/JacobMayfield/Documents/Orb/XCMSAna/Table_S1.txt")

CBS_e <- ExpressionSet(as.matrix(CBS_GR[,1:5]))
featureNames(CBS_e) <- row.names(CBS_GR)
# exprs(CBS_e) shows the data

# Not standardized, but already a percentage
# CBS_s <- standardise(CBS_e)

# Setting the soft clustering parameters m and c
# Estimate the fuzzyness
m1 <- mestimate(CBS_e)
# m1= 2.8

# C is determined empirically as the largest value that give empty clusters.
# Obtain a table of uniform partitions using partcoef
coef <- partcoef(CBS_e, crange=seq(2,20,2))
pcF <- coef[[1]];F.n <- coef[[2]];F.min <- coef[[3]]
pcF > 1.01 * F.min
# Stable clusters are seen at < c=4 and m > 1.15.
# Go with c=4, m=m1

clsize <- 4
CBS_cl <- mfuzz(CBS_e, c=clsize, m=m1)

pdf(file="CBS_clusters.pdf", paper="letter")

overlap.plot(CBS_cl, over=overlap(CBS_cl), thres=0.05)
	par(new=T) # Add a title to PCA plot
	plot(1, type="n", axes=F, xlab="", ylab="", main="PCA of cluster membership")
# Reiteration shows reasonable cluster size of 3 to 8

clusters <- sapply(1:dim(CBS_cl[[4]])[2], function(i) subset(CBS_cl[[4]][sort.list(CBS_cl[[4]][,i], decreasing=T),i], CBS_cl[[4]][sort.list(CBS_cl[[4]][,i], decreasing=T),i] > 0.5), simplify=F)
clusters

# Generate a graphical report including cluster centers, members, and PCA.
# Note: plot(CBS_cl[[1]][2,],type="l") shows the center of cluster 2 alone
mfuzz.plot.2(CBS_e, cl=CBS_cl, mfrow=c(4,2), time.labels=c(0.5,1,2,4,400), xlab="B6", ylab="Rate", new.window=T)

dev.off()

# Add membership values to a growth rate data

cl.mem <- data.frame(cbind(row.names(CBS_cl[[4]]), signif(CBS_cl[[4]],5), CBS_cl[[3]]))
names(cl.mem) <- c("cl.names", paste("Cluster", (1:clsize)), "Top cluster")
GR.names <- data.frame(row.names(CBS_GR), CBS_GR)
names(GR.names)

GR.clusters <- merge(GR.names, cl.mem, by.x="row.names.CBS_GR.", by.y="cl.names", sort=F)
write.csv(GR.clusters, file="CBS.rate.clusters.csv", quote=F)