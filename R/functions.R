source('R/metavalues.R')

DNtoTOA <- function(image, band){
  toa <- eval(parse(text = paste0("RADIANCE_MULT_BAND_", as.character(band)))) *
    image + eval(parse(text = paste0("RADIANCE_ADD_BAND_", as.character(band))))
  return(toa)
}

ndviFunc <- function(red, NIR) {
  ndvi <- (NIR - red) / (NIR + red)
  return(ndvi)
}

calcLST <- function(TIR){
  lst <- K2_CONSTANT_BAND_10/log(K1_CONSTANT_BAND_10/TIR + 1) #lst in Kelvin
  celsius <- lst - 273.15 #conversion to degrees Celsius
  return(celsius)
}

RMSE <- function(pred, obs){
  rmse <- sqrt(mean((pred-obs)^2, na.rm=TRUE))
  rmse <- round(rmse, 2)
  return(rmse)
}