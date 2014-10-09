HMG <- read.delim("/Users/JacobMayfield/Documents/CBS Paper 2011/HEM1_heatmap2.txt", row.names=1)
HMGh <- read.delim("/Users/JacobMayfield/Documents/CBS Paper 2011/hem1-_heatmap.txt", row.names=1)

attach(HMG)
attach(HMGh)
HMGM=data.matrix(HMG, rownames.force="ID")
HMGMh=data.matrix(HMGh, rownames.force="ID")

library(gplots)

#pdf(file="/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/HEM1_heatmap.pdf", height=15, width=15)
pdf(file="/Users/JacobMayfield/Documents/CBS Paper 2011/HEM1_heatmap.pdf", height=15, width=15)

heatmap.2(HMGM, Rowv=T, Colv=F, dendrogram="row", col=colorpanel(30, "blue", "grey", "yellow"), scale="column", key=TRUE, symkey=FALSE, density.info="none", trace="none", labRow=row.names(HMG), margins=c(10,15), cexRow=1.25, cexCol=1.25, keysize=0.6)
dev.off()

#pdf(file="/Users/jamayfie/Notebook 2.0/Paper 2011_CBS/hem1-_heatmap.pdf", height=15, width=15)
pdf(file="/Users/JacobMayfield/Documents/CBS Paper 2011/hem1-_heatmap.pdf", height=15, width=15)

heatmap.2(HMGMh, Rowv=T, Colv=F, dendrogram="row", col=colorpanel(30, "blue", "grey", "yellow"), scale="column", key=TRUE, symkey=FALSE, density.info="none", trace="none", labRow=row.names(HMGh), margins=c(10,15), cexRow=1.25, cexCol=1.25, keysize=0.6)
dev.off()
