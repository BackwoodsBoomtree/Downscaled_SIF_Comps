library(terra)
library(pals)

tropomi_file <- "G:/TROPOMI/esa/gridded/20km/8day/TROPOMI.ESA.SIF.201805-202111.global.8day.20km.CF20.VIs.nc"
lrange       <- (78:123) # 32:77 is for 2019; 78:123 is for 2020
out_dir      <- "G:/SIF_comps/min_max/20km/cf20"
out_name     <- "8day.20km.CF20.2020"

# Build stacks for each variable
tropomi_sif     <- rast(tropomi_file, subds = "SIF_743")[[lrange]]
tropomi_n       <- rast(tropomi_file, subds = "n")[[lrange]]
tropomi_sif_std <- rast(tropomi_file, subds = "SIF_743_std")[[lrange]]
tropomi_nirv    <- rast(tropomi_file, subds = "NIRv")[[lrange]]
tropomi_nirvr   <- rast(tropomi_file, subds = "NIRv_RAD")[[lrange]]
tropomi_ndvi    <- rast(tropomi_file, subds = "NDVI")[[lrange]]


# Calculate SEM
# sif_sem <- tropomi_sif / (sqrt(tropomi_n))

# Get min and max rasters
tropomi_sif_max <- which.max(tropomi_sif)
tropomi_sif_min <- which.min(tropomi_sif)

tropomi_nirv_max <- which.max(tropomi_nirv)
tropomi_nirv_min <- which.min(tropomi_nirv)

tropomi_nirvr_max <- which.max(tropomi_nirvr)
tropomi_nirvr_min <- which.min(tropomi_nirvr)

tropomi_ndvi_max <- which.max(tropomi_ndvi)
tropomi_ndvi_min <- which.min(tropomi_ndvi)

# Save min_max rasters
writeRaster(tropomi_sif_max, paste0(out_dir, "/", "SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(tropomi_sif_min, paste0(out_dir, "/", "SIF.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(tropomi_nirv_max, paste0(out_dir, "/", "NIRv.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(tropomi_nirv_min, paste0(out_dir, "/", "NIRv.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(tropomi_nirvr_max, paste0(out_dir, "/", "NIRv_Rad.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(tropomi_nirv_min, paste0(out_dir, "/", "NIRv_Rad.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(tropomi_ndvi_max, paste0(out_dir, "/", "NDVI.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(tropomi_ndvi_min, paste0(out_dir, "/", "NDVI.min.", out_name, ".tif"), overwrite = TRUE)

# Difference from SIF (negative means NIRv earlier than SIF)
diff_max_nirv  <-  tropomi_nirv_max  - tropomi_sif_max
diff_max_nirvr <-  tropomi_nirvr_max - tropomi_sif_max
diff_max_ndvi  <-  tropomi_ndvi_max  - tropomi_sif_max

diff_min_nirv  <-  tropomi_nirv_min  - tropomi_sif_min
diff_min_nirvr <-  tropomi_nirvr_min - tropomi_sif_min
diff_min_ndvi  <-  tropomi_ndvi_min  - tropomi_sif_min

# Reclassify keeping the sign of the differences in 8-day periods (-22 to 22),
# Where positive means VI is later, negative means VI is sooner
reclass_sign <- matrix(c((45:24), (-45:-24), (-1:-22), (1:22)), ncol = 2)

nirv_sif_max_reclass_sign  <- classify(diff_max_nirv, reclass_sign)
nirvr_sif_max_reclass_sign <- classify(diff_max_nirvr, reclass_sign)
ndvi_sif_max_reclass_sign  <- classify(diff_max_ndvi, reclass_sign)

nirv_sif_min_reclass_sign  <- classify(diff_min_nirv, reclass_sign)
nirvr_sif_min_reclass_sign <- classify(diff_min_nirvr, reclass_sign)
ndvi_sif_min_reclass_sign  <- classify(diff_min_ndvi, reclass_sign)

# Save min_max raster differences with sign
writeRaster(nirv_sif_max_reclass_sign, paste0(out_dir, "/", "NIRv-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(nirv_sif_min_reclass_sign, paste0(out_dir, "/", "NIRv-SIF.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(nirvr_sif_max_reclass_sign, paste0(out_dir, "/", "NIRv_Rad-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(nirvr_sif_min_reclass_sign, paste0(out_dir, "/", "NIRv_Rad-SIF.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(ndvi_sif_max_reclass_sign, paste0(out_dir, "/", "NDVI-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(ndvi_sif_min_reclass_sign, paste0(out_dir, "/", "NDVI-SIF.min.", out_name, ".tif"), overwrite = TRUE)

# # Reclassify into absolute differences in 8-day periods (difference is positive (no sign) 0 to 23)
# reclass_abs <- matrix(c((45:24), (1:22)), ncol = 2)
# 
# nirv_sif_max_reclass_abs  <- classify(abs(diff_max_nirv), reclass_abs)
# nirvr_sif_max_reclass_abs <- classify(abs(diff_max_nirvr), reclass_abs)
# ndvi_sif_max_reclass_abs  <- classify(abs(diff_max_ndvi), reclass_abs)
# 
# nirv_sif_min_reclass_abs  <- classify(abs(nirv_sif_min), reclass_abs)
# nirvr_sif_min_reclass_abs <- classify(abs(nirvr_sif_min), reclass_abs)
# ndvi_sif_min_reclass_abs  <- classify(abs(ndvi_sif_min), reclass_abs)
# 

# Take a peek
vir.col  <- viridis(46)
diff.col <- coolwarm(47)

# Plot min max
plot(tropomi_sif_max, col = vir.col)
plot(tropomi_sif_min, col = vir.col)

plot(tropomi_nirv_max, col = vir.col)
plot(tropomi_nirv_min, col = vir.col)

plot(tropomi_nirvr_max, col = vir.col)
plot(tropomi_nirvr_min, col = vir.col)

plot(tropomi_ndvi_max, col = vir.col)
plot(tropomi_ndvi_min, col = vir.col)

# Plot min_max differences with SIF

plot(nirv_sif_max_reclass_abs, col = viridis(23))
plot(nirvr_sif_max_reclass_abs, col = viridis(23))
plot(ndvi_sif_max_reclass_abs, col = viridis(23))

plot(nirv_sif_max_reclass_sign, col = diff.col)
plot(nirvr_sif_max_reclass_sign, col = diff.col)
plot(ndvi_sif_max_reclass_sign, col = diff.col)


plot(sif_sem[[1]])

brks <- seq(0, 0.5, by = 0.1)
plot(sif_sem[[23]], breaks = brks, col = viridis(5))
