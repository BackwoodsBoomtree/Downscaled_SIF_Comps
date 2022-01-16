library(terra)

# Mask out land where maximum monthly NDVI is <0.1.

# Monthly data
ndvi_mask <- rast("G:/TROPOMI/esa/gridded/20km/monthly/TROPOMI.ESA.SIF.201805-202111.global.monthly.20km.CF20.VIs.nc", subds = "NDVI")[[21:32]]

# Get max annual values for masking
ndvi_mask <- max(ndvi_mask, na.rm = TRUE)
ndvi_mask[ndvi_mask < 0.2] <- NA

plot(ndvi_mask, col = viridis(100))

ndvi_mask[ndvi_mask >= 0.2] <- 1

plot(ndvi_mask)

writeRaster(ndvi_mask, "G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif", overwrite = TRUE)
