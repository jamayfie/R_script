# Auto loan calculator

# P=principal
# T=trade-in
# D=downpayment
# I=annual interest rate
# N=months of loan term

# Monthly payment
MP <- function(p, t, d, i=1.9, n, model) {
	j <- i/1200
	M <- round(((p-t-d)*(j/(1-(1+j)^(-n)))),0)
	out <- data.frame(c(p, t, d, i, n, M), row.names=c("price", "trade", "down", "interest", "term", "MONTHLY"))
	names(out) <- model
	out
	}
	
models <- c("2.5i_m", "2.5p_m", "2.5p_cvt")

price <- c(23500, 24500, 26500)
trade <- 9700
dp <- seq(1000, 3000, by=1000)
rate <- 1.9
term <- c(36, 48)

grid <- expand.grid(price, trade, dp, rate, term)
names(grid) <- c("price", "trade", "down", "interest", "term")

	
table <- lapply(1:dim(grid)[1], function(i) MP(p=grid[i,1], t=grid[i,2], d=grid[i,3], i=grid[i,4], n=grid[i,5], model=rep(models, dim(grid)[1]/length(price))[i]))

txm <- data.frame(matrix(unlist(table), ncol=dim(grid)[1]), row.names=row.names(table[[1]]))
names(txm) <- rep(models, dim(grid)[1]/length(models))
txm

m <- subset(txm, select=names(txm) %in% models[1])
pm <- subset(txm, select=names(txm) %in% models[2])
pcvt <- subset(txm, select=names(txm) %in% models[3])

library(gplots)
library(gridExtra)

pdf(file="Car.R.pdf", paper="legal")

t1 <- tableGrob(m, gpar.coltext=gpar(fontsize=10, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=10, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=9, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)
t2 <- tableGrob(pm, gpar.coltext=gpar(fontsize=10, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=10, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=9, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)
t3 <- tableGrob(pcvt, gpar.coltext=gpar(fontsize=10, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=10, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=9, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)

grid.arrange(main=textGrob("Monthly Payments", just="top", gp=gpar(font=2, fontsize=12)), t1, t2, t3, sub=textGrob(""))


dev.off()



# Repeat with actuals #############


models <- c("DI", "MSR", "plus")

price <- c(24954, 26485, 27212)
trade <- c(8700, 9700)
dp <- seq(2000, 4000, by=1000)
rate <- 1.9
term <- c(36, 48)

grid <- expand.grid(price, trade, dp, rate, term)
names(grid) <- c("price", "trade", "down", "interest", "term")

	
table <- lapply(1:dim(grid)[1], function(i) MP(p=grid[i,1], t=grid[i,2], d=grid[i,3], i=grid[i,4], n=grid[i,5], model=rep(models, dim(grid)[1]/length(price))[i]))

txm <- data.frame(matrix(unlist(table), ncol=dim(grid)[1]), row.names=row.names(table[[1]]))
names(txm) <- rep(models, dim(grid)[1]/length(models))
txm

inv <- subset(txm, select=names(txm) %in% models[1])
msrp <- subset(txm, select=names(txm) %in% models[2])
plus <- subset(txm, select=names(txm) %in% models[3])

library(gplots)
library(gridExtra)

pdf(file="Car.R.pdf", paper="legal")

t1 <- tableGrob(inv, gpar.coltext=gpar(fontsize=8, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=8, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=8, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)
t2 <- tableGrob(msrp, gpar.coltext=gpar(fontsize=8, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=8, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=8, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)
t3 <- tableGrob(plus, gpar.coltext=gpar(fontsize=8, col="black", fontface="bold"), gpar.rowtext=gpar(fontsize=8, col="black", fontface="bold"), gpar.coretext=gpar(fontsize=8, col="black"), gpar.corefill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.rowfill=gpar(fill="grey95", col="grey95", alpha=0.5), gpar.colfill=gpar(fill=NA, col=NA), h.odd.alpha=0, clip=F)

grid.arrange(main=textGrob("Monthly Payments", just="top", gp=gpar(font=2, fontsize=12)), t1, t2, t3, sub=textGrob(""))


dev.off()