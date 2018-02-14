Log Odds
=====

Log Odds encoding for categorical variables.

This encoding is target specific, so it will require that you sample the data, rather than using
your entire training data for generating the encoding scheme (otherwise you will have label leakage).

LogOdds.md

This module will use a sample of data to calculate the log odds of the outcome for every value
that can be taken by every categorical column that you specify.


The encoding table can then be applied to encode new data, and can optionally include a naive bayes
column that is simply the sum of all the log odds columns.


### Usage

You need to pass the function the sample data frame, the column you want to encode, the column containing
the target.

You then use that encoding scheme to encode the column in the rest of your training data.


``` 
source("LogOdds.R")

lo.enc 		<- getLogOddsTable( data_frame, vector_of_column_names, target_column_name )

new.train.df   	<- applyLogOddsTable( training_data_frame, vector_of_column_names, lo.enc, suffix_for_new_columns, include_naive_bayes )

```



