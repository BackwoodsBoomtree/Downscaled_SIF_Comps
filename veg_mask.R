library(terra)

# Mask out land where maximum monthly NDVI is <0.1.

# Monthly data
ndvi_mask <- rast("G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc", subds = "NDVI")

# Get max annual values for masking
ndvi_mask <- max(ndvi_mask, na.rm = TRUE)
ndvi_mask[ndvi_mask < 0.2] <- NA

plot(ndvi_mask, col = viridis(100))

ndvi_mask[ndvi_mask >= 0.2] <- 1

plot(ndvi_mask)

writeRaster(ndvi_mask, "G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif", overwrite = TRUE)
