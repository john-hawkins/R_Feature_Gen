require(foreach)

# ################################################################################
#  Take a data structure that contains encodings for categorical variables and 
#  applies them to a data set.
#
#   df: 	The data frame to encode
#   columns:	The list of columns
#   encTable:	The table contaning encodings
#   encName:	The name of the encoding value in the table
#   suffix:	Suffix to add to the column name when adding the encoding
#
# #######################################################################

applyEncoder <- function( df, columns, encTable, encName, suffix="enc") {

  addCols <- foreach(col = columns, .combine=cbind) %dopar% {
    tp <- encTable[[col]]
    temp <- data.table(tp)
    vec <- temp[ as.character(df[[col]]), get(encName) ]
                vec <- ifelse(is.na(vec),0,vec)
                vec
  }

  if(suffix == "") {
     returnDf <- cbind(df[, setdiff(names(d), columns), with=F], addCols)
  } else {
     colnames(addCols) <- paste(columns, suffix, sep="_")
     returnDf <- cbind(df, addCols)
  }

  returnDf
}



