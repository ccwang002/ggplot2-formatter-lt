library(ggplot2)
library(reshape2)
library(scales)
fancy_scientific <- function(l) { 
	# This function is adapted from Brian Diggs's solution
	# https://groups.google.com/d/msg/ggplot2/a_xhMoQyxZ4/OQHLPGsRtAQJ
	# turn in to character string in scientific notation 
	l <- format(l, scientific=TRUE) 
	l <- gsub("0.0+e[+-]0+$", "0", l)  # use '0' instead '0 x 10^0'
	# quote the part before the exponent to keep all the digits 
	l <- gsub("^(.*)e", "'\\1'e", l) 
	l <- gsub("e[+]", "e", l)  # remove the extra '+'
	l <- gsub("e", "%*%10^", l)   # turn the 'e+' into plotmath format 
	parse(text=l)   # return this as an expression 
} 

# read in simplified data
df.total <- read.csv('description.csv', stringsAsFactors=FALSE)

df.total <- ddply(df.total, "New.ID", transform, mapped.genome.only = Mapped.to.genome-mapped.in.miRBase.20)

# convert Wide to Long
df.long <- melt(df.total, 
								id.vars=c('New.ID', 'Total.reads'),  # which should be fixed 
								measure.vars=c('mapped.in.miRBase.20', 'mapped.genome.only', 'Unmapped.to.genome'),
								variable.name='Map.condition',   
								value.name='read.count')

# custom y-axis ticks positions
breaks <- c(0, seq(from=5e6, to=2e7, by=5e6))

# manually denote y-axis label for each tick, which can also be done
# by the following fancy_scientific() formatter
labels <- c('0', expression(5%*%10^6), expression(1.0%*%10^7),
						expression(1.5%*%10^7), expression(2.0%*%10^7))

# label percentage
df.long <- ddply(df.long, "New.ID", transform, 
								 label_y=sum(read.count) - cumsum(read.count) + 0.5 * read.count)
df.long <- ddply(df.long, "New.ID", transform,
								 percentage=read.count / sum(read.count)) 

library(plyr)
# plot stacked bar
g <- ggplot(df.long, aes(x=New.ID, y=read.count, fill=Map.condition, order=desc(Map.condition))) + 
	geom_bar(stat='identity', alpha=.9) +
	scale_y_continuous(breaks=breaks, labels=labels) + 
	labs(x='', y='Total Reads') + 
	theme_classic(18)

library(RColorBrewer)
# Grey theme: scale_fill_grey(start=1, end=.5) 
fills <- rev(brewer.pal(4, 'Blues'))[-c(1)]
g + geom_text(aes(y=label_y, label=percent(percentage))) +
	scale_fill_manual(values=fills, name="Map Condition", 
										labels=c("miRBase 20", "Genome only", "Unmapped")) +
	theme(legend.position=c(1,1), legend.justification=c(1,1)) +
	theme(legend.background=element_rect(color="black"))
