require(xgboost)
require(data.table)
require(magrittr)

# #################################################
# generate a training matrix for xgboost
# ###############################################

getXgboostMatrix <- function( df, feats, target ) {
 
  # cast data frame to a data table
  dt <- data.table(df)

  xgb.DMatrix(
    data = data.matrix( dt[,
             feats[[1]],
             with = FALSE]),
    label = dt[, get(target)],
    missing = NA
  )

}

getXgboostMatrixFromFormula <- function( df, formula ) {
  temp <- as.character(formula)
  target <- temp[[2]]
  feats <- gsub( "\\n| ", "", temp[[3]]) %>%  strsplit( ., "\\+" )
  getXgboostMatrix(df, feats, target) 
}




