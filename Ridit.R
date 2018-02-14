require(data.table)
require(dplyr)
require(reshape)

# #########################################################################################
 # Generate a data structure that represents the Ridit encoding
 # of a specific ordinal column in the data.
 #
 # df: 		A data frame containing a sample of the data
 # colname: 	The column you want to encode
 # target: 	The target column that will be used for the Ridit calculation
 # levs: 	A vector of levels for the ordinal column, arranged in increasing order
 #
 # NOTE: The target must be a binary column with 0 and 1 values only.
 # You MUST not use this with the entire training set. Use a sample
 # to generate these encodings and apply that to the remaining training data
 # Otherwise you will have target leakage in your training data.
# #######################################################################################


# #######################################################################
# Get a data structure that contains encoding for multiple culumns
# #######################################################################
getRiditEncoder <- function(df, columns, lev.order, target) {

    # Get the total number of target positives
    total <- sum(df[,target])

    ridit.tab <- list()

    for(colname in columns) {
       levs <- lev.order[colname][[1]]

       # Group all of the data by the key column
       grpd <- df %>% group_by_(colname)

       # Then sum the target by group
       sum.grpd <- grpd %>% summarise( tot=sum(get(target)) )

       # Calculate the proportion of the target in each group
       sum.grpd$prop <- sum.grpd$tot / total
       # Cast to a data table so de-refernced column names will work
       sg <- data.table(sum.grpd)

       # Calculate the Ridit encoding.
       # Roughly equivalent to the expected cumulative proportion of
       # the target instances seen as you move through the data ordered by the
       # ordinal variable being encoded ( ordering specified in levs )
       cumul <- 0
       mapd = c()
       for (i in 1:length(levs)) {
           prop <- sg[ get(colname) == levs[i], ]$prop
           mapd[levs[[i]]]  <- cumul +  0.5 * prop
           cumul <- cumul + prop
       }
       # Put it into long form so it will work with the generic encoder function
       final <- melt(mapd)
       setDT(final, keep.rownames = TRUE)
       colnames(final) <- c('V1', 'value')
       setkey(final, V1)
       ridit.tab[[colname]] <- final
    }
    ridit.tab
}

# ##########################################################
# Old Version - only works for a single column
# ##########################################################
getRiditTable <- function(df, colname, target, levs) {
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
