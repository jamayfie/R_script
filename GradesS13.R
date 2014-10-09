# R script for binning grades using the Freedman-Diaconis Rule.
# Cut and paste into a .R file to make it executable.

# Read in a table of student IDs and grade totals.  Row names forces the ID to become the name.
# Remove outliers from the table (students who dropped and have very low F's).
totals <- read.table(header=T, "/Users/JacobMayfield/Documents/310 General Microbiology/Files/Totals_R.txt", row.names=1)
attach(totals)
totalsEC <- read.table(header=T, "/Users/JacobMayfield/Documents/310 General Microbiology/Files/TotalsEC_R.txt", row.names=1)
attach(totalsEC)

# pdf(file="/Users/JacobMayfield/Documents/310 General Microbiology/Grading/Grade_Adjustment.pdf", height=15, width=15)
# par(mfrow=c(3, 1), mar=c(10,10,10,10))
# for on screen display, use:
# par(mfrow=c(3, 1))

# histogram by grade percentages, as defined by standard scale
# Note that the histogram bar areas are not correct becuase bin sizes are not equal.
# This is a graphic error only, and does not alter calculations.
letters <- c("F", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")
# Set total points
tp <- 500
hist(as.numeric(totals[,1]), breaks=c(0, tp*.599, tp*.6699, tp*0.6999, tp*0.7299, tp*0.7699, tp*0.7999, tp*0.8299, tp*0.8699, tp*0.8999, tp*0.9299, tp), labels=letters, freq=T, xlim=c(0,tp), ylim=c(0,25), xlab="Grade", ylab="Number of Students", main="Unadjusted Grades")


# This determines optimal bin size by Freedman-Diaconis using Tukey's five number estimator
size <- tp/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333))

#histogram by Freedman-Diaconis using Tukey's five number estimator
# Modified to use Tukey's five number estimator instead of quartiles (more acurate)
# Note: FDR specifies a bin size based on Full Quantiles.
# The bins still have to be specified.
# In this case, I make 11 bins, with a 12th that holds all F's.
# To match a straight scale, A+ is not included, but this will likely penalize students.
letters3 <- c("F", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+")

# Histogram is drawn using optimal bin size, rather than the default
# The default rounds bin size to produce integers
# Rounding shrinks the bin size, and makes the adjustment to grades less generous
# Check out the "old code" below to see how this works.
hist(as.numeric(totals[,1]), breaks=c(0, tp-(size*11), tp-(size*10), tp-(size*9), tp-(size*8), tp-(size*7), tp-(size*6), tp-(size*5), tp-(size*4), tp-(size*3), tp-(size*2), tp-(size), tp), labels=letters3, freq=T, xlim=c(0,tp), ylim=c(0,40), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule")

# Same as above, but gives the distribution after extra credit.
hist(as.numeric(totalsEC[,1]), breaks=c(0, tp-(size*11), tp-(size*10), tp-(size*9), tp-(size*8), tp-(size*7), tp-(size*6), tp-(size*5), tp-(size*4), tp-(size*3), tp-(size*2), tp-(size), tp), labels=letters3, freq=T, xlim=c(0,tp), ylim=c(0,40), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule with EC")
# dev.off()

# Breakpoints only.
# This give the numbers only, without graphing, and was used for calculations.
# Breakpoints for straight scale:
sbreaks <- hist(as.numeric(totals[,1]), breaks=c(0, tp*.599, tp*.6699, tp*0.6999, tp*0.7299, tp*0.7699, tp*0.7999, tp*0.8299, tp*0.8699, tp*0.8999, tp*0.9299, tp), plot=F)
# Breakpoints for FDR scale:
FDRbreaks <- hist(as.numeric(totals[,1]), breaks=c(0, tp-(size*11), tp-(size*10), tp-(size*9), tp-(size*8), tp-(size*7), tp-(size*6), tp-(size*5), tp-(size*4), tp-(size*3), tp-(size*2), tp-(size), tp), plot=F)
FDRbreaksEC <- hist(as.numeric(totalsEC[,1]), breaks=c(0, tp-(size*11), tp-(size*10), tp-(size*9), tp-(size*8), tp-(size*7), tp-(size*6), tp-(size*5), tp-(size*4), tp-(size*3), tp-(size*2), tp-(size), tp), plot=F)


# Output table of breaks
Breaks <- as.data.frame(round(cbind(c(sbreaks$breaks), FDRbreaks$breaks[1:12])))
row.names(Breaks) <- c(letters3)
names(Breaks) <- c("Breaks", "Breaks_FDR")
Counts <- as.data.frame(cbind(c(sbreaks$counts, "NA"), FDRbreaks$counts, FDRbreaksEC$counts))
names(Counts) <- c("Counts", "Counts_FDR", "Counts_w/EC")
cbind(Breaks,Counts)

#############################
## Old code.  Bin size is rounded to an integer (20 instead of 22)
#letters2 <- c("F", "F", "F", "F", "F", "F", "D", "D", "D+", "C-", "C", "C", "C+", "B-", "B", "B", "B+", "A-", "A", "A")
#hist(as.numeric(totals[,1]), breaks=tp/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333)), labels=letters2, xlim=c(0,tp), ylim=c(0,30), xlab="Grade", ylab="Number of Students", main="Grades Adjusted by Freedman-Diaconis Rule")

#hist(as.numeric(totals[,1]), breaks=tp/(2*(fivenum(as.numeric(totals[,1]), na.rm=F)[4]-fivenum(as.numeric(totals[,1]), na.rm=F)[2])*(length(totals[,1])^-0.33333)), plot=F)





