library(terra)
library(pals)

tropomi_file <- "G:/TROPOMI/esa/gridded/1deg/8day/TROPOMI.ESA.SIF.201805-202109.global.8day.1deg.clearsky.nc"
lrange       <- (32:77) # 32:77 is for 2019

tropomi_sif     <- rast(tropomi_file, subds = "SIF_743")[[lrange]]
tropomi_sifdc   <- rast(tropomi_file, subds = "SIF_Corr_743")[[lrange]]
tropomi_n       <- rast(tropomi_file, subds = "n")[[lrange]]
tropomi_sif_std <- rast(tropomi_file, subds = "SIF_743_std")[[lrange]]
tropomi_nirv    <- rast(tropomi_file, subds = "NIRv")[[lrange]]
tropomi_nirvr   <- rast(tropomi_file, subds = "NIRv_RAD")[[lrange]]
tropomi_ndvi    <- rast(tropomi_file, subds = "NDVI")[[lrange]]


# Calculate SEM
sif_sem <- tropomi_sif / (sqrt(tropomi_n))

# Get min and max rasters
tropomi_sif_max <- which.max(tropomi_sif)
tropomi_sif_min <- which.min(tropomi_sif)

tropomi_sifdc_max <- which.max(tropomi_sifdc)
tropomi_sifdc_min <- which.min(tropomi_sifdc)

tropomi_nirv_max <- which.max(tropomi_nirv)
tropomi_nirv_min <- which.min(tropomi_nirv)

tropomi_nirvr_max <- which.max(tropomi_nirvr)
tropomi_nirvr_min <- which.min(tropomi_nirvr)

tropomi_ndvi_max <- which.max(tropomi_ndvi)
tropomi_ndvi_min <- which.min(tropomi_ndvi)

# Difference from SIF
nirv_sif_max  <- tropomi_nirv_max - tropomi_sif_max
nirvr_sif_max <- tropomi_nirvr_max - tropomi_sif_max
ndvi_sif_max  <- tropomi_ndvi_max - tropomi_sif_max

nirv_sif_min  <- tropomi_nirv_min - tropomi_sif_min
nirvr_sif_min <- tropomi_nirvr_min - tropomi_sif_min
ndvi_sif_min  <- tropomi_ndvi_min - tropomi_sif_min

# Reclassify into absolute differences in 8-day periods
reclass_abs <- matrix(c((45:24), (1:22)), ncol = 2)

nirv_sif_max_reclass_abs  <- classify(abs(nirv_sif_max), reclass_abs)
nirvr_sif_max_reclass_abs <- classify(abs(nirvr_sif_max), reclass_abs)
ndvi_sif_max_reclass_abs  <- classify(abs(ndvi_sif_max), reclass_abs)

nirv_sif_min_reclass_abs  <- classify(abs(nirv_sif_min), reclass_abs)
nirvr_sif_min_reclass_abs <- classify(abs(nirvr_sif_min), reclass_abs)
ndvi_sif_min_reclass_abs  <- classify(abs(ndvi_sif_min), reclass_abs)

# Reclassify keeping the sign of the differences in 8-day periods
reclass_sign <- matrix(c((45:24), (-45:-24), (-1:-22), (1:22)), ncol = 2)

nirv_sif_max_reclass_sign  <- classify(nirv_sif_max, reclass_sign)
nirvr_sif_max_reclass_sign <- classify(nirvr_sif_max, reclass_sign)
ndvi_sif_max_reclass_sign  <- classify(ndvi_sif_max, reclass_sign)

nirv_sif_min_reclass_sign  <- classify(nirv_sif_min, reclass_sign)
nirvr_sif_min_reclass_sign <- classify(nirvr_sif_min, reclass_sign)
ndvi_sif_min_reclass_sign  <- classify(ndvi_sif_min, reclass_sign)



# Plot
plot(tropomi_sif_max, col = vir.col)
plot(tropomi_sif_min, col = vir.col)

plot(tropomi_nirv_max, col = vir.col)
plot(tropomi_nirv_min, col = vir.col)

plot(tropomi_nirvr_max, col = vir.col)
plot(tropomi_nirvr_min, col = vir.col)

plot(nirvr_sif_max, col = vir.col)


plot(nirv_sif_max_reclass_abs, col = viridis(23))
plot(nirvr_sif_max_reclass_abs, col = viridis(23))
plot(ndvi_sif_max_reclass_abs, col = viridis(23))

plot(nirv_sif_max_reclass_sign, col = diff.col)
plot(nirvr_sif_max_reclass_sign, col = diff.col)
plot(ndvi_sif_max_reclass_sign, col = diff.col)


plot(sif_sem[[1]])

brks <- seq(0, 0.5, by = 0.1)
plot(sif_sem[[23]], breaks = brks, col = viridis(5))


sif_mean <- mean(tropomi_sif, na.rm = TRUE)

vir.col  <- viridis(46)
diff.col <- coolwarm(47)

plot(test3, col = vir.col)
