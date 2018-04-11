require(data.table)

extractPhoneStub <- function(phone) {
  if( substr(phone, 1, 3) == '+61' ) {
     substr(phone, 4, 6)
  } else if(substr(phone, 1, 1) == '0') {
     substr(phone, 2, 4)
  } else {
     substr(phone, 1, 3)
  }

}
extractAllPhoneStubs <- Vectorize(extractPhoneStub)


# THESE MOBILE PHONE FEATURES ARE BASED ON THE ORIGINAL ISSUER OF THE PHONE 
# IN AUSTRALIA. MANY OF THESE NUMBERS WILL HAVE BEEN MOVED BETWEEN CARRIERS
# SO THESE FEATURES ARE ONLY A CRUDE INDICATION OF WHO SOMEONE WAS LIKELY TO
# HAVE GOTTENT THEIR ORIGINAL CONTRACT
##############################################################################

addPhoneFeatures <- function(dfin, phone_col) {

  df	<- data.table(dfin)

  df[, phoneStub :=  extractAllPhoneStubs( df[[phone_col]] ) ]

  telstraMobStubs <- c(	"400", "407", "408", "409", "417", "418", 
			"419", "427", "428", "429", "437", "438", 
			"439", "447", "448", "455", "456", "457", 
			"458", "459", "467", "472", "473", "474", 
			"475", "476", "477", "484", "487", "488",
			"490", "491", "497", "498", "499")

  df[, phoneIsTelstra := ifelse(
          ( phoneStub %in% telstraMobStubs),
          1,
          0
  )]

  optusMobStubs <- c(	"401", "402", "403", "411", "412", "413", 
			"421", "422", "423", "431", "432", "434", 
			"435", "466", "468", "478", "479", "481", 
			"482")

  df[, phoneIsOptus := ifelse(
          ( phoneStub %in% optusMobStubs),
          1,
          0
  )]

  vodaMobStubs <- c(	"404", "405", "406", "414", "415", "416", 
			"424", "425", "426", "430", "433", "449", 
			"450", "451", "452")

  df[, phoneIsVoda := ifelse(
          ( phoneStub %in% vodaMobStubs),
          1,
          0
  )]

  df[, phoneIsLyca := ifelse(
          ( phoneStub %in% c("470", "469") ),
          1,
          0
  )]

  df[]
}


