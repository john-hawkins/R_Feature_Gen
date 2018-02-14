source('LogOdds.R')
source('Ridit.R')
source('Freqs.R')
source('ApplyEncoder.R')

# ##########################################################################################
#   A simple example script of how to use the encoders
#
#   Create a data frame wtih several categorical variables and a target variable that is 
#   a binary variable containing either 0 or 1
#
# ##########################################################################################

df <- data.frame( 'cat1'=c('X', 'X', 'X', 'X', 'Y', 'Y', 'Y', 'X', 'X', 'X', 'Y', 'X', 'X', 'X', 'Y', 'X')
		, 'cat2'=c('a', 'b', 'a', 'b', 'b', 'b', 'b', 'a', 'b', 'b', 'a', 'b', 'a', 'b', 'a', 'a')
		, 'label'=c( 1,   0,   1,  1,   1,   0,   1,   0,   0,   0,   1,   0,   1,   0,   0,   0  ) 
	)

columns <- c('cat1', 'cat2')
target <- 'label'

lev.orders <- list( cat1=c('X', 'Y'), cat2=c('a', 'b') )

# #####################################################################
# Split the data randomly (or out-of-time if the data requires it)
# Here I am just assuming the order is sufficiently random
# ####################################################################
splitPoint <- round(nrow(df)/2)
sampleData <- df[1:splitPoint,]
encodeData <- df[(splitPoint+1):nrow(df),]

# ####################################################################
# Create the encoders
# ####################################################################

lot 		<- getLogOddsTable(sampleData, columns, target)
rit 		<- getRiditEncoder(sampleData, columns, lev.orders, target)
freq 		<- getFrequencyEncoder(sampleData, columns)

applied 	<- applyLogOddsTable(encodeData, columns, lot)
applied2 	<- applyEncoder(applied, columns, rit, 'value', suffix='ridit')
applied3 	<- applyEncoder(applied2, columns, freq, 'freq', suffix='freq')




