## Name: Maarten van Doornik
## Subject: Final project Geoscripting
## Title: Predicting Soil Organic Matter in the Great Plains
## Date: 2 February 2017

library(raster)
library(rgdal)
library(gstat)
library(randomForest)
library(rasterVis)
library(dynatopmodel)

# unzip data files
unzip('./data/band4.zip', exdir='./data')
unzip('./data/band5.zip', exdir='./data')
unzip('./data/band10.zip', exdir='./data')
unzip('./data/DEM.zip', exdir='./data')
unzip('./data/WBC.zip', exdir='./data')

source('R/functions.R')

# Define extent study area
e <- extent(c(559484, 561151, 4379521, 4380704))

# load files
red <- raster("data/LC80330322013269LGN00_B4.TIF")
NIR <- raster("data/LC80330322013269LGN00_B5.TIF")
TIR10 <- raster("data/LC80330322013269LGN00_B10.TIF")
refpoints <- readOGR(dsn='data', layer='WBC')
dem <- raster('data/DEMrastercenter1.tif')

# Only keep relevant attributes
refpoints <- subset(refpoints, select=c("OM_in__", "Validation"))

#split reference data into calibration and validation
calibration <- subset(refpoints, Validation == 'Calibration')
validation <- subset(refpoints, Validation == 'Validation')

# Crop files and resample if necessary
red_area <- crop(red, e)
NIR_area <- crop(NIR, e)
TIR10_area <- crop(TIR10, e)
dem_area <- crop(dem, e)
dem_area_res <- resample(dem_area, red_area)

# Convert Digital Numbers (DN) to Top Of Atmosphere (TOA) reflectance values
toa_band4_red <- DNtoTOA(red_area, 4)
toa_band5_NIR <- DNtoTOA(NIR_area, 5)
toa_band10_TIR <- DNtoTOA(TIR10_area, 10)

# make rasterBrick for ndvi calculation
landsat <- brick(toa_band4_red, toa_band5_NIR)
names(landsat) <- c("band4_red", "band5_NIR")

#calculate ndvi
ndvi <- overlay(x=landsat[["band4_red"]], y=landsat[["band5_NIR"]], fun=ndviFunc)

# Caculate land surface temperature
LST_celsius <- calcLST(toa_band10_TIR)

# Source for calculating LST: http://www.gis-blog.com/calculation-of-land-surface-temperature-lst-from-landsat-8-using-r/

# Calculate Topographic Wetness Index (TWI)
upslope <- upslope.area(dem_area_res, atb=TRUE)
twi <- upslope$atb

#extract values
calibration$ndvi <- extract(ndvi, calibration)
calibration$lst <- extract(LST_celsius, calibration)
calibration$twi <- extract(twi, calibration)

# Make RasterBrick of independent variables
br <- brick(ndvi, LST_celsius, twi)
names(br) <- c("ndvi", "lst", "twi")

#randomForest
califrame <- data.frame(calibration)
modelRF <- randomForest(x=califrame[, c(3, 4, 5)], y=califrame$OM_in__, importance=T)
SOM_rf <- predict(br, modelRF)

# Multiple linear regression
lmSOM <- lm(OM_in__~ndvi+lst+twi, data=calibration)
SOM_regression <- predict(br, lmSOM)

# Regression Kriging
calibration$resid <- lmSOM$residuals
vgres  <- variogram(resid~1, calibration)
#plot(vgres)
vgmres <- vgm(nugget=3, psill=10, range=300, model="Exp")
vgmres <- fit.variogram(vgres, vgmres, fit.method=7)
#plot(vgres, vgmres)

gridframe <- as(br, "SpatialGridDataFrame")
proj4string(gridframe) <- proj4string(calibration)
SOM_rk <- krige(resid~1, calibration, newdata=gridframe,
                  model=vgmres, beta=0, nmax = 14)

SOM_rk$var1.pred <- SOM_rk$var1.pred +
  as(SOM_regression, "SpatialGridDataFrame")$layer
SOM_rk <- raster(SOM_rk)

#visualize and compare output maps
outputs <- brick(SOM_rf, SOM_regression, SOM_rk)
names(outputs) <- c("RandomForest", "Linear regression", "Regression Kriging")
#cols <- colorRampPalette(c('grey', 'yellow', 'brown'))
#levelplot(outputs, col.regions=cols(255))

#calculate RMSE values
validation$SOM_rf <- extract(SOM_rf, validation)
validation$SOM_regression <- extract(SOM_regression, validation)
validation$SOM_rk <- extract(SOM_rk, validation)

rmse_rf <- RMSE(validation$SOM_rf, validation$OM_in__)
rmse_regression <- RMSE(validation$SOM_regression, validation$OM_in__)
rmse_rk <- RMSE(validation$SOM_rk, validation$OM_in__)

# make overview of resulting RMSE values
RMSE_overview <- data.frame(c(rmse_rf, rmse_regression, rmse_rk), row.names=c('RandomForest', 'Linear regression', 'Regression Kriging'))  
colnames(RMSE_overview) <- 'RMSE'
