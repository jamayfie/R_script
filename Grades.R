# R script for binning grades using the Freedman-Diaconis Rule.
# Cut and paste into a .R file to make it executable.

# Read in a table of student IDs and grade totals.  Row names forces the ID to become the name.
totals <- read.table(header=T, "/Users/JacobMayfield/Documents/310 General Microbiology/Grading/CT_R.txt", row.names=1)
attach(totals)
totalsEC <- read.table(header=T, "/Users/JacobMayfield/Documents/310 General Microbiology/Grading/CTEC_R.txt", row.names=1)
attach(totalsEC)

# for on screen display, use:
# par(mfrow=c(3, 1))
pdf(file="/Users/JacobMayfield/Documents/310 General Microbiology/Grading/Grade_Adjustment.pdf", height=15, width=15)
par(mfrow=c(3, 1), mar=c(10,10,10,10))
# histogram by grade percentages, as defined by standard scale
# Note that the histogram bar areas are not correct becuase bin sizes are not equal.
# This is a graphic error only, and does not alter calculations.
letters <- c("F", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")
hist(as.numeric(totals[,1]), breaks=c(0, 600*.599, 600*.6699, 600*0.6999, 600*0.7299, 600*0.7699, 600*0.7999, 600*0.8299, 600*0.8699, 600*0.8999, 600*0.9299, 600), labels=letters, freq=T, xlim=c(0,600), ylim=c(0,35), xlab="Grade", ylab="Number of Students", main="Unadjusted Grades")

#histogram by Freedman-Diaconis using Tukey's five number estimator
# Modified to use Tukey's five number estimator instead of quartiles (more acurate)
letters3 <- c("F", "D", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A", "A+")

# This determines optimal bin size
size <- 600/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333))

# Histogram is drawn using optimal bin size, rather than the default
# The default rounds bin size to produce integers
# Rounding shrinks the bin size, and makes the adjustment to grades less generous
# Check out the "old code" below to see how this works.
hist(as.numeric(totals[,1]), breaks=c(0, 600-(size*13), 600-(size*12), 600-(size*11), 600-(size*10), 600-(size*9), 600-(size*8), 600-(size*7), 600-(size*6), 600-(size*5), 600-(size*4), 600-(size*3), 600-(size*2), 600-(size), 600), labels=letters3, freq=T, xlim=c(0,600), ylim=c(0,30), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule")

# Same as above, but gives the distribution after extra credit.
hist(as.numeric(totalsEC[,1]), breaks=c(0, 600-(size*13), 600-(size*12), 600-(size*11), 600-(size*10), 600-(size*9), 600-(size*8), 600-(size*7), 600-(size*6), 600-(size*5), 600-(size*4), 600-(size*3), 600-(size*2), 600-(size), 600), labels=letters3, freq=T, xlim=c(0,600), ylim=c(0,30), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule with EC")
dev.off()

# Breakpoints only.
# This give the numbers only, without graphing, and was used for calculations.
hist(as.numeric(totals[,1]), breaks=c(0, 600*.599, 600*.6699, 600*0.6999, 600*0.7299, 600*0.7699, 600*0.7999, 600*0.8299, 600*0.8699, 600*0.8999, 600*0.9299, 600), plot=F)
hist(as.numeric(totals[,1]), breaks=c(0, 600-(size*13), 600-(size*12), 600-(size*11), 600-(size*10), 600-(size*9), 600-(size*8), 600-(size*7), 600-(size*6), 600-(size*5), 600-(size*4), 600-(size*3), 600-(size*2), 600-(size), 600), plot=F)
hist(as.numeric(totalsEC[,1]), breaks=c(0, 600-(size*13), 600-(size*12), 600-(size*11), 600-(size*10), 600-(size*9), 600-(size*8), 600-(size*7), 600-(size*6), 600-(size*5), 600-(size*4), 600-(size*3), 600-(size*2), 600-(size), 600), plot=F)

#############################
## Old code.  Bin size is rounded to an integer (20 instead of 22)
#letters2 <- c("F", "F", "F", "F", "F", "F", "D", "D", "D+", "C-", "C", "C", "C+", "B-", "B", "B", "B+", "A-", "A", "A")
#hist(as.numeric(totals[,1]), breaks=600/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333)), labels=letters2, xlim=c(0,600), ylim=c(0,30), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule")

#hist(as.numeric(totals[,1]), breaks=600/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333)), plot=F)





