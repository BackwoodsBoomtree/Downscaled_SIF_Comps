library(terra)
library(pals)

tropomi_file <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"
out_dir      <- "G:/SIF_comps/min_max/1deg/clearsky"
out_name     <- "monthly.1deg.clearsky.cold.2021"

# Build stacks for each variable
tropomi_sif     <- rast(tropomi_file, subds = "SIF_743")
# tropomi_n       <- rast(tropomi_file, subds = "n")
# tropomi_sif_std <- rast(tropomi_file, subds = "SIF_743_std")
tropomi_nirv    <- rast(tropomi_file, subds = "NIRv")
tropomi_nirvr   <- rast(tropomi_file, subds = "NIRv_Rad")
tropomi_ndvi    <- rast(tropomi_file, subds = "NDVI")


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


plot(sif_sem[[1]])

brks <- seq(0, 0.5, by = 0.1)
plot(sif_sem[[23]], breaks = brks, col = viridis(5))
