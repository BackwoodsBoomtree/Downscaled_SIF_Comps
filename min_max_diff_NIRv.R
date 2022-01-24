
library(terra)
library(pals)

out_dir      <- "G:/SIF_comps/min_max_diff/20km/nirvr"
out_name     <- "monthly.20km.2020"

tropomi_nirvr_cs_max <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad.max.monthly.20km.clearsky.2020.tif")
tropomi_nirvr_cs_min <- rast("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad.min.monthly.20km.clearsky.2020.tif")

tropomi_nirvr_cf20_max <- rast("G:/SIF_comps/min_max/20km/cf20/NIRv_Rad.max.monthly.20km.CF20.2020.tif")
tropomi_nirvr_cf20_min <- rast("G:/SIF_comps/min_max/20km/cf20/NIRv_Rad.min.monthly.20km.CF20.2020.tif")


# Difference
diff_max_nirvr <-  tropomi_nirvr_cs_max - tropomi_nirvr_cf20_max
diff_min_nirvr <-  tropomi_nirvr_cs_min - tropomi_nirvr_cf20_min

# Reclassify keeping the sign of the differences in 8-day periods (-22 to 22),
# Where positive means VI is later, negative means VI is sooner
reclass_sign <- matrix(c((45:24), (-45:-24), (-1:-22), (1:22)), ncol = 2)

diff_max_nirvr_sign <- classify(diff_max_nirvr, reclass_sign)
diff_min_nirvr_sign <- classify(diff_min_nirvr, reclass_sign)

# Save min_max raster differences with sign
writeRaster(diff_max_nirvr_sign, paste0(out_dir, "/", "NIRv_Rad.clearsky-NIRv_Rad.CF20.max.", out_name, ".tif"), overwrite = TRUE)
writeRaster(diff_min_nirvr_sign, paste0(out_dir, "/", "NIRv_Rad.clearsky-NIRv_Rad.CF20.min.", out_name, ".tif"), overwrite = TRUE)

