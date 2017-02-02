library(stringr)

# Table to look up the values
metadata <- read.table('data/LC80330322013269LGN00_MTL.txt', sep='\t')
metadata <- data.frame(str_split_fixed(metadata$V1, "=", 2))

# Values from Metadata file
RADIANCE_MULT_BAND_4 <- 9.9423E-03
RADIANCE_MULT_BAND_5 <- 6.0842E-03
RADIANCE_MULT_BAND_10 <- 3.3420E-04

RADIANCE_ADD_BAND_4 <- -49.71141
RADIANCE_ADD_BAND_5 <- -30.42090
RADIANCE_ADD_BAND_10 <- 0.10000

K1_CONSTANT_BAND_10 <- 774.89
K2_CONSTANT_BAND_10 <- 1321.08