#-----------------------------------------------------------------------------
# Initial Setup
#-----------------------------------------------------------------------------
source('data_wrangling.R')
library(ggplot2)

g <- ggplot(df, aes(x=Country.Code, y=Y2012)) +
	geom_bar(stat='identity', alpha=.9) + 
	xlim(df$Country.Code)

#-----------------------------------------------------------------------------
# Manual Setting Both `breaks` and `labels`
#-----------------------------------------------------------------------------
breaks <- c(0, seq(from=5e11, to=1.5e12, by=5e11))
labels <- c('0', expression(5%*%10^11), expression(1.0%*%10^12), expression(1.5%*%10^12))
g + scale_y_continuous(breaks=breaks, labels=labels) + theme_grey(20)

#-----------------------------------------------------------------------------
# Examples Using scales
#-----------------------------------------------------------------------------
library(scales)
g + scale_y_continuous(labels=dollar) + theme_grey(20)

#-----------------------------------------------------------------------------
# Advanced Formatter
#-----------------------------------------------------------------------------
fancy_scientific_format <- function(l) { 
	# This function is adapted from Brian Diggs's solution
	# https://groups.google.com/d/msg/ggplot2/a_xhMoQyxZ4/OQHLPGsRtAQJ
	# turn in to character string in scientific notation 
	l <- format(l, scientific=TRUE) 
	l <- gsub("0.0+e[+-]0+$", "0", l)  # use '0' instead '0 x 10^0'
	# quote the part before the exponential to keep all the digits 
	l <- gsub("^(.*)e", "'\\1'e", l) 
	l <- gsub("e[+]", "e", l)  # remove the extra '+'
	l <- gsub("e", "%*%10^", l)   # turn the 'e+' into plotmath format
	l
}

fancy_scientific <- function(l){
	parse(text=fancy_scientific_format(l))   # return this as an expression 
}
 
super_fancy_dollar <- function(l){
	parse(text=gsub("^'", "'$", fancy_scientific_format(l)))
}

g + scale_y_continuous(labels=fancy_scientific)
g + scale_y_continuous(breaks=c(0, 1e12, 1.3e12), labels=fancy_scientific)
g + scale_y_continuous(labels=super_fancy_dollar) + theme_grey(20)

#-----------------------------------------------------------------------------
# For Development
#-----------------------------------------------------------------------------
test_formatter <- function(l){
	print(mode(l))
	print(l)
}