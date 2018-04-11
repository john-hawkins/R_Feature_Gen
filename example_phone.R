source('phoneFeatures.R')

# ##########################################################################################
#   A simple example script of how to use the phone number feature functions
#
#   Create a data frame with phone numbers and covert them into features
#
# ##########################################################################################

df <- data.frame( 'phone'=c('0437999000', '0405111000', '0400111000', '0410000111', '+61411000111', '+61469111000')
                , 'label'=c( 1,   0,   0,  0,   1,   0 )
        )

df$phone	<- as.character(df$phone)

newdf		<- addPhoneFeatures(df, 'phone')

newdf
