#-----------------------------------------------------------------------------
# Read in CSV
#-----------------------------------------------------------------------------
CSV_PATH <- 'NY_GDP_PPP_WorldBank/ny.gdp.mktp.pp.cd_Indicator_en_csv_v2.csv'
df <- read.csv(CSV_PATH, stringsAsFactors=FALSE, skip=2, row.names=NULL)

#-----------------------------------------------------------------------------
# Skim through data
#-----------------------------------------------------------------------------
# sort the GDP(PPP) of year 2012 and choose some country
#df <- df[order(df$X2012, decreasing=TRUE), ]

#-----------------------------------------------------------------------------
# Select part of the data
#-----------------------------------------------------------------------------
# we only want data of year 2012
cols.keep <- c(
	'Country.Name',
	'Country.Code',
	'X2012'
) # define rows to keep
country.interested <- c(
#	'United States',
#	'China',	
#	'Japan',
	'Korea, Rep.',
	'Hong Kong SAR, China',
	'Singapore',
	'Vietnam'
)
df <- df[
	!is.na(df$X2012) & df$Country.Name %in% country.interested, 
	cols.keep, drop=TRUE
]  # drop rows if no year 2012 data

#-----------------------------------------------------------------------------
# Clean Up & Misc. 
#-----------------------------------------------------------------------------
colnames(df)[3] <- 'Y2012'

# We add Taiwan GDP(PPP) manually here, based on the estimation by IMF (2012).
# See wiki for more information
df <- rbind(df, c('Taiwan', 'TWN', 902e+9))
df$Y2012 <- as.numeric(df$Y2012)
df <- df[order(df$Y2012, decreasing=TRUE), ]
row.names(df) <- NULL