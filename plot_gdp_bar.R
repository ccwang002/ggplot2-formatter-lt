source('data_wrangling.R')
library(ggplot2)

g <- ggplot(df, aes(x=Country.Code, y=Y2012)) +
	geom_bar(stat='identity', alpha=.9) + 
	xlim(df$Country.Code)

library(scales)
g + scale_y_continuous(labels=dollar)

fancy_scientific_format <- function(l) { 
	# This function is adapted from Brian Diggs's solution
	# https://groups.google.com/d/msg/ggplot2/a_xhMoQyxZ4/OQHLPGsRtAQJ
	# turn in to character string in scientific notation 
	l <- format(l, scientific=TRUE) 
	l <- gsub("0.0+e[+-]0+$", "0", l)  # use '0' instead '0 x 10^0'
	# quote the part before the exponent to keep all the digits 
	l <- gsub("^(.*)e", "'\\1'e", l) 
	l <- gsub("e[+]", "e", l)  # remove the extra '+'
	l <- gsub("e", "%*%10^", l)   # turn the 'e+' into plotmath format 
}

fancy_scientific <- function(l){
	parse(text=fancy_scientific_format(l))   # return this as an expression 
}

super_fancy_dollar <- function(l){
	parse(text=gsub("^'", "'$", fancy_scientific_format(l)))
}

g + scale_y_continuous(labels=super_fancy_dollar)
