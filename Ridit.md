Ridit
=====

Ridit encoding for ordinal variables.

This encoding is target specific, so it will require that you sample the data, rather than using
your entire training data for generating the encoding scheme (otherwise you will have label leakage).

The encoding is equivalent to the expected cumulative proportion of the target instances that will
have been seen in a data set order by the ordinal column under consideration.

For the purposes of this function we only encode categorical variables that have an underlying order.
So if you want to apply it to a float or integer column you need to bin it before hand.

Additionally this encoding can only be used for binary classification, and it expects the target
column to contain zeros and ones.


### Usage

You need to pass the function the sample data frame, the column you want to encode, the column containing
the target and a vector of containing the ordering of the values. 

You then use that encoding scheme to encode the column in the rest of your training data.


``` 

ridit.enc <- getRiditEncoding( df, colname, target, levels )

new.train.df   <- applyRiditEncoding( train.df, colname, ridit.enc )

```



