/*
 * Generate a data structure that represents the Ridit encoding
 * of a specific ordinal column in the data.
 *
 * df: 		A data frame containing a sample of the data
 * colname: 	The column you want to encode
 * target: 	The target column that will be used for the Ridit calculation
 * levs: 	A vector of levels for the ordinal column, arranged in increasing order
 *
 * NOTE: The target must be a binary column with 0 and 1 values only.
 * You MUST not use this with the entire training set. Use a sample
 * to generate these encodings and apply that to the remaining training data
 * Otherwise you will have target leakage in your training data.
 */

getRiditEncoder <- function(df, colname, target, levs) {
    # Get the total number of target positives
    total <- sum(df[,target])

    # Group all of the data by the key column
    grpd <- df %>% group_by_(colname)

    # Then sum the target by group
    sum.grpd <- grpd %>% summarise( tot=sum(get(target)) )

    # Calculate the proportion of the target in each group
    sum.grpd$prop <- sum.grpd$tot / total

    # Calculate the Ridit encoding. 
    # Roughly equivalent to the expected cumulative proportion of 
    # the target instances seen as you move through the data ordered by the
    # ordinal variable being encoded ( ordering specified in levs )
    cumul <- 0
    mapd = c()
    for (i in 1:length(levs)) {
        prop <- sum.grpd[ get(colname) == levs[[i]], ]$prop
        mapd[levs[[i]]]  <- cumul +  0.5 * prop
        cumul <- cumul + prop
    }

    # Return a mapping of the levels to the Ridit score
    mapd
}


applyRiditEncoding <- function(df, colname, riditEncoder) {

}


