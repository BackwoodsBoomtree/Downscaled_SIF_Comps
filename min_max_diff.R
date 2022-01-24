
library(terra)
library(pals)

out_dir      <- "G:/SIF_comps/min_max_diff/20km/clearsky_vis"
out_name     <- "CF60.monthly.20km.2020"

tropomi_sif_max   <- rast("G:/SIF_comps/min_max/20km/cf60/SIF.max.monthly.20km.CF60.2020.tif")
tropomi_sif_min   <- rast("G:/SIF_comps/min_max/20km/cf60/SIF.min.monthly.20km.CF60.2020.tif")
tropomi_nirv_max  <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv.max.monthly.20km.clearsky.2020.tif")
tropomi_nirv_min  <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv.min.monthly.20km.clearsky.2020.tif")
tropomi_nirvr_max <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad.max.monthly.20km.clearsky.2020.tif")
tropomi_nirvr_min <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad.min.monthly.20km.clearsky.2020.tif")
tropomi_ndvi_max  <- rast("G:/SIF_comps/min_max/20km/clearsky/NDVI.max.monthly.20km.clearsky.2020.tif")
tropomi_ndvi_min  <- rast("G:/SIF_comps/min_max/20km/clearsky/NDVI.min.monthly.20km.clearsky.2020.tif")

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
writeRaster(nirv_sif_max_reclass_sign, paste0(out_dir, "/", "NIRv.clearsky-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(nirv_sif_min_reclass_sign, paste0(out_dir, "/", "NIRv.clearsky-SIF.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(nirvr_sif_max_reclass_sign, paste0(out_dir, "/", "NIRv_Rad.clearsky-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(nirvr_sif_min_reclass_sign, paste0(out_dir, "/", "NIRv_Rad.clearsky-SIF.min.", out_name, ".tif"), overwrite = TRUE)

writeRaster(ndvi_sif_max_reclass_sign, paste0(out_dir, "/", "NDVI.clearsky-SIF.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(ndvi_sif_min_reclass_sign, paste0(out_dir, "/", "NDVI.clearsky-SIF.min.", out_name, ".tif"), overwrite = TRUE)

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

# Take a peek
diff.col <- coolwarm(47)
# vir.col  <- viridis(46)

# Plot differences

plot(nirv_sif_max_reclass_sign, col = diff.col)
plot(nirvr_sif_max_reclass_sign, col = diff.col)
plot(ndvi_sif_max_reclass_sign, col = diff.col)

# plot(nirv_sif_max_reclass_abs, col = viridis(23))
# plot(nirvr_sif_max_reclass_abs, col = viridis(23))
# plot(ndvi_sif_max_reclass_abs, col = viridis(23))