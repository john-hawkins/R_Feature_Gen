require(foreach)

# ###################################################################################
# Encode categorical variables with the frequency in the data 
# ##################################################################################

getFrequencyEncoder <- function( df, columns ) {
  
  freq.tab <- list()

  for(col in columns) {
    tempTab <- data.table( table(df[[col]]) )

    total <- sum( tempTab$N )
    tempTab$freq <- tempTab$N / total

    setkey(tempTab, V1)
    freq.tab[[col]] <- tempTab
  }

  freq.tab

}

