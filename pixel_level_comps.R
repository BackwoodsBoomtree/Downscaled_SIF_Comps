library(terra)

cf_file    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc"
cs_file    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

# Get the data
nirv_cf     <- rast(cf_file, subds = "NIRv")
nirv_rad_cf <- rast(cf_file, subds = "NIRv_Rad")
sif_cf      <- rast(cf_file, subds = "SIF_743")
ref_665_cf  <- rast(cf_file, subds = "REF_665")
ref_781_cf  <- rast(cf_file, subds = "REF_781")

n_cf            <- rast(cf_file, subds = "n")
nirv_std_cf     <- rast(cf_file, subds = "NIRv_std")
nirv_rad_std_cf <- rast(cf_file, subds = "NIRv_Rad_std")
sif_std_cf      <- rast(cf_file, subds = "SIF_743_std")
ref_665_std_cf  <- rast(cf_file, subds = "REF_665_std")
ref_781_std_cf  <- rast(cf_file, subds = "REF_781_std")

nirv_sem_cf     <- nirv_std_cf / (sqrt(n_cf))
nirv_rad_sem_cf <- nirv_rad_std_cf / (sqrt(n_cf))
sif_sem_cf      <- sif_std_cf / (sqrt(n_cf))
ref_665_sem_cf  <- ref_665_std_cf / (sqrt(n_cf))
ref_781_sem_cf  <- ref_781_std_cf / (sqrt(n_cf))

nirv_cs     <- rast(cs_file, subds = "NIRv")
nirv_rad_cs <- rast(cs_file, subds = "NIRv_Rad")
sif_cs      <- rast(cs_file, subds = "SIF_743")
ref_665_cs  <- rast(cs_file, subds = "REF_665")
ref_781_cs  <- rast(cs_file, subds = "REF_781")

n_cs            <- rast(cs_file, subds = "n")
nirv_std_cs     <- rast(cs_file, subds = "NIRv_std")
nirv_rad_std_cs <- rast(cs_file, subds = "NIRv_Rad_std")
sif_std_cs      <- rast(cs_file, subds = "SIF_743_std")
ref_665_std_cs  <- rast(cs_file, subds = "REF_665_std")
ref_781_std_cs  <- rast(cs_file, subds = "REF_781_std")

nirv_sem_cs     <- nirv_std_cs / (sqrt(n_cs))
nirv_rad_sem_cs <- nirv_rad_std_cs / (sqrt(n_cs))
sif_sem_cs      <- sif_std_cs / (sqrt(n_cs))
ref_665_sem_cs  <- ref_665_std_cs / (sqrt(n_cs))
ref_781_sem_cs  <- ref_781_std_cs / (sqrt(n_cs))

# Coordinate
# Amazon zero diff
# coord <- vect(cbind(-67.5, -2.5), type = "points", crs = "+proj=longlat +datum=WGS84")
# Amazon 3 months early
coord <- vect(cbind(-72.5, -1.5), type = "points", crs = "+proj=longlat +datum=WGS84")

# Check coord is in the correct location
# plot.new()
# plot(nirv[[1]])
# plot(coord, add = TRUE)

# Extract the data
ts_nirv_cf     <- extract(nirv_cf, coord)
ts_nirv_rad_cf <- extract(nirv_rad_cf, coord)
ts_sif_cf      <- extract(sif_cf, coord)
ts_ref_665_cf  <- extract(ref_665_cf, coord)
ts_ref_781_cf  <- extract(ref_781_cf, coord)
ts_n_cf        <- extract(n_cf, coord)

ts_nirv_sem_cf     <- extract(nirv_sem_cf, coord)
ts_nirv_rad_sem_cf <- extract(nirv_rad_sem_cf, coord)
ts_sif_sem_cf      <- extract(sif_sem_cf, coord)
ts_ref_665_sem_cf  <- extract(ref_665_sem_cf, coord)
ts_ref_781_sem_cf  <- extract(ref_781_sem_cf, coord)

ts_nirv_cf     <- as.vector(t(ts_nirv_cf[-c(1)]))
ts_nirv_rad_cf <- as.vector(t(ts_nirv_rad_cf[-c(1)]))
ts_sif_cf      <- as.vector(t(ts_sif_cf[-c(1)]))
ts_ref_665_cf  <- as.vector(t(ts_ref_665_cf[-c(1)]))
ts_ref_781_cf  <- as.vector(t(ts_ref_781_cf[-c(1)]))
ts_n_cf        <- as.vector(t(ts_n_cf[-c(1)]))

ts_nirv_sem_cf     <- as.vector(t(ts_nirv_sem_cf[-c(1)]))
ts_nirv_rad_sem_cf <- as.vector(t(ts_nirv_rad_sem_cf[-c(1)]))
ts_sif_sem_cf      <- as.vector(t(ts_sif_sem_cf[-c(1)]))
ts_ref_665_sem_cf  <- as.vector(t(ts_ref_665_sem_cf[-c(1)]))
ts_ref_781_sem_cf  <- as.vector(t(ts_ref_781_sem_cf[-c(1)]))

ts_nirv_cs     <- extract(nirv_cs, coord)
ts_nirv_rad_cs <- extract(nirv_rad_cs, coord)
ts_sif_cs      <- extract(sif_cs, coord)
ts_ref_665_cs  <- extract(ref_665_cs, coord)
ts_ref_781_cs  <- extract(ref_781_cs, coord)
ts_n_cs        <- extract(n_cs, coord)

ts_nirv_sem_cs     <- extract(nirv_sem_cs, coord)
ts_nirv_rad_sem_cs <- extract(nirv_rad_sem_cs, coord)
ts_sif_sem_cs      <- extract(sif_sem_cs, coord)
ts_ref_665_sem_cs  <- extract(ref_665_sem_cs, coord)
ts_ref_781_sem_cs  <- extract(ref_781_sem_cs, coord)

ts_nirv_cs     <- as.vector(t(ts_nirv_cs[-c(1)]))
ts_nirv_rad_cs <- as.vector(t(ts_nirv_rad_cs[-c(1)]))
ts_sif_cs      <- as.vector(t(ts_sif_cs[-c(1)]))
ts_ref_665_cs  <- as.vector(t(ts_ref_665_cs[-c(1)]))
ts_ref_781_cs  <- as.vector(t(ts_ref_781_cs[-c(1)]))
ts_n_cs        <- as.vector(t(ts_n_cs[-c(1)]))

ts_nirv_sem_cs     <- as.vector(t(ts_nirv_sem_cs[-c(1)]))
ts_nirv_rad_sem_cs <- as.vector(t(ts_nirv_rad_sem_cs[-c(1)]))
ts_sif_sem_cs      <- as.vector(t(ts_sif_sem_cs[-c(1)]))
ts_ref_665_sem_cs  <- as.vector(t(ts_ref_665_sem_cs[-c(1)]))
ts_ref_781_sem_cs  <- as.vector(t(ts_ref_781_sem_cs[-c(1)]))

# Plot
par(mfrow = c(3, 2), oma=c(0.1,0.1,1.25,0.1))

x = 1:12

op <- par(mar = c(0,5,0,0))
plot(x, ts_nirv_cf, col = "black", type = "l", ylim = c(min(ts_nirv_cf), max(ts_nirv_cs)))
lines(x, ts_nirv_cs, col = "black", lty = 2)
arrows(x0 = x, y0 = ts_nirv_cf - ts_nirv_sem_cf, x1 = x, y1 = ts_nirv_cf + ts_nirv_sem_cf, code=3, angle=90, length=0.1)

op <- par(mar = c(0,5,0,0))
plot(x, ts_nirv_rad_cf, col = "orange", type = "l")
arrows(x0 = x, y0 = ts_nirv_rad_cf - ts_nirv_rad_sem_cf, x1 = x, y1 = ts_nirv_rad_cf + ts_nirv_rad_sem_cf, code=3, angle=90, length=0.1)

op <- par(mar = c(0,5,0,0))
plot(x, ts_ref_781_cf, col = "pink", type = "l")
arrows(x0 = x, y0 = ts_ref_781_cf - ts_ref_781_sem_cf, x1 = x, y1 = ts_ref_781_cf + ts_ref_781_sem_cf, code=3, angle=90, length=0.1)

op <- par(mar = c(0,5,0,0))
plot(x, ts_ref_665_cf, col = "red", type = "l")
arrows(x0 = x, y0 = ts_ref_665_cf - ts_ref_665_sem_cf, x1 = x, y1 = ts_ref_665_cf + ts_ref_665_sem_cf, code=3, angle=90, length=0.1)

op <- par(mar = c(0,5,0,0))
plot(x, ts_sif_cf, col = "purple", type = "l")
arrows(x0 = x, y0 = ts_sif_cf - ts_sif_sem_cf, x1 = x, y1 = ts_sif_cf + ts_sif_sem_cf, code=3, angle=90, length=0.1)

op <- par(mar = c(0,5,0,0))
plot(x, ts_n_cf, col = "black", type = "l")

