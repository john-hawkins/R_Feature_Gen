library(data.table)
library(foreach)

# ###################################################################################
# Log Odds Encoding Table
#
# Generate a table of log odds encodings for all levels of all the specified columns
#
# You need to generate this on a sample of the training data.
# ###################################################################################

getLogOddsTable <- function(df, columns, target) {

  # MAKE A COPY OF THE DATASET CAST AS A DATA TABLE
  dt <- data.table(df)

  lo.tab <- list()

  for(col in columns) {
    temp <- dt[, list(np = sum(get(target)==1), nn = sum(get(target) == 0)), by = list(value = get(col))]
    temp[, lo := log( (np+1) / (nn+1) )] 
    temp[, value := as.character(value)]
    setkey(temp, value)
    lo.tab[[col]] <- temp
  }
  lo.tab
}

# #################################################################################################
#     Apply the Log Odds Encoding Table
#
#  Note: this encoding has a dedicated apply because we
#  want to be able to incude the Naive Bayes column.
#
# #################################################################################################

applyLogOddsTable <- function(df, columns, lo.tab, suffix='lodds', include.naive.bayes=TRUE) {

  addColumns <- foreach(col = columns, .combine=cbind) %dopar% {
    temp <- lo.tab[[col]]
    vals <- temp[ as.character( df[[col]] ), lo]
    vals <- ifelse(is.na(vals), 0, vals)
    vals
  } 

  new.names <- lapply(columns, function(n) { paste(n, '.', suffix, sep='') } )
  colnames(addColumns) <- new.names

  final.df <- cbind(df, addColumns)
  if(include.naive.bayes) { 
   final.df$naiveBayes <- apply( final.df[, colnames(addColumns)], 1, function(lo) sum(lo))
  }
  final.df
}

