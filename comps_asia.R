library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)

#### Load Files ####
cf_file    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
cs_file    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Create single mask with cover threshold
m <- mask(mask_ebf, mask_veg)
m[m < 80]  <- NA
# m[m >= 80] <- 1

# Extent and crop
asia_ext <- extent(c(95,155,-11,10))
cover    <- crop(m, asia_ext)

#### Get the data ####
nirv_cf     <- crop(mask(rast(cf_file, subds = "NIRv"), m), asia_ext)
nirv_rad_cf <- crop(mask(rast(cf_file, subds = "NIRv_Rad"), m), asia_ext)
sif_cf      <- crop(mask(rast(cf_file, subds = "SIF_743"), m), asia_ext)
ref_665_cf  <- crop(mask(rast(cf_file, subds = "REF_665"), m), asia_ext)
ref_781_cf  <- crop(mask(rast(cf_file, subds = "REF_781"), m), asia_ext)

n_cf            <- crop(mask(rast(cf_file, subds = "n"), m), asia_ext)
nirv_std_cf     <- crop(mask(rast(cf_file, subds = "NIRv_std"), m), asia_ext)
nirv_rad_std_cf <- crop(mask(rast(cf_file, subds = "NIRv_Rad_std"), m), asia_ext)
sif_std_cf      <- crop(mask(rast(cf_file, subds = "SIF_743_std"), m), asia_ext)
ref_665_std_cf  <- crop(mask(rast(cf_file, subds = "REF_665_std"), m), asia_ext)
ref_781_std_cf  <- crop(mask(rast(cf_file, subds = "REF_781_std"), m), asia_ext)

nirv_cs     <- crop(mask(rast(cs_file, subds = "NIRv"), m), asia_ext)
nirv_rad_cs <- crop(mask(rast(cs_file, subds = "NIRv_Rad"), m), asia_ext)
sif_cs      <- crop(mask(rast(cs_file, subds = "SIF_743"), m), asia_ext)
ref_665_cs  <- crop(mask(rast(cs_file, subds = "REF_665"), m), asia_ext)
ref_781_cs  <- crop(mask(rast(cs_file, subds = "REF_781"), m), asia_ext)

n_cs            <- crop(mask(rast(cs_file, subds = "n"), m), asia_ext)
nirv_std_cs     <- crop(mask(rast(cs_file, subds = "NIRv_std"), m), asia_ext)
nirv_rad_std_cs <- crop(mask(rast(cs_file, subds = "NIRv_Rad_std"), m), asia_ext)
sif_std_cs      <- crop(mask(rast(cs_file, subds = "SIF_743_std"), m), asia_ext)
ref_665_std_cs  <- crop(mask(rast(cs_file, subds = "REF_665_std"), m), asia_ext)
ref_781_std_cs  <- crop(mask(rast(cs_file, subds = "REF_781_std"), m), asia_ext)



#### Extract the data ####
ts_nirv_cf     <- global(nirv_cf, fun = "mean", na.rm = TRUE)
ts_nirv_rad_cf <- global(nirv_rad_cf, fun = "mean", na.rm = TRUE)
ts_sif_cf      <- global(sif_cf, fun = "mean", na.rm = TRUE)
ts_ref_665_cf  <- global(ref_665_cf, fun = "mean", na.rm = TRUE)
ts_ref_781_cf  <- global(ref_781_cf, fun = "mean", na.rm = TRUE)
ts_n_cf        <- global(n_cf, fun = "mean", na.rm = TRUE)

ts_nirv_cf     <- as.vector(t(ts_nirv_cf))
ts_nirv_rad_cf <- as.vector(t(ts_nirv_rad_cf))
ts_sif_cf      <- as.vector(t(ts_sif_cf))
ts_ref_665_cf  <- as.vector(t(ts_ref_665_cf))
ts_ref_781_cf  <- as.vector(t(ts_ref_781_cf))
ts_n_cf        <- as.vector(t(ts_n_cf))

ts_nirv_cs     <- global(nirv_cs, fun = "mean", na.rm = TRUE)
ts_nirv_rad_cs <- global(nirv_rad_cs, fun = "mean", na.rm = TRUE)
ts_sif_cs      <- global(sif_cs, fun = "mean", na.rm = TRUE)
ts_ref_665_cs  <- global(ref_665_cs, fun = "mean", na.rm = TRUE)
ts_ref_781_cs  <- global(ref_781_cs, fun = "mean", na.rm = TRUE)
ts_n_cs        <- global(n_cs, fun = "mean", na.rm = TRUE)

ts_nirv_cs     <- as.vector(t(ts_nirv_cs))
ts_nirv_rad_cs <- as.vector(t(ts_nirv_rad_cs))
ts_sif_cs      <- as.vector(t(ts_sif_cs))
ts_ref_665_cs  <- as.vector(t(ts_ref_665_cs))
ts_ref_781_cs  <- as.vector(t(ts_ref_781_cs))
ts_n_cs        <- as.vector(t(ts_n_cs))


#### Calc SEM at regional scale ####

# Sum the number of soundings from all gridcells
ts_n <- global(n_cf, fun = "sum", na.rm = TRUE)[[1]]

# SIF CF20
var           <- sif_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_sif_sem_cf <- std_time / (sqrt(ts_n))

# SIF CS
var           <- sif_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_sif_sem_cs <- std_time / (sqrt(ts_n))

# NIRv CF20
var           <- nirv_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_sem_cf <- std_time / (sqrt(ts_n))

# NIRv CS
var           <- nirv_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_sem_cs <- std_time / (sqrt(ts_n))

# NIRv Rad CF20
var           <- nirv_rad_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_rad_sem_cf <- std_time / (sqrt(ts_n))

# NIRv Rad CS
var           <- nirv_rad_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_rad_sem_cs <- std_time / (sqrt(ts_n))

# REF 665 CF20
var           <- ref_665_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_665_sem_cf <- std_time / (sqrt(ts_n))

# REF 665 CS
var           <- ref_665_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_665_sem_cs <- std_time / (sqrt(ts_n))

# REF 781 CF20
var           <- ref_781_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_781_sem_cf <- std_time / (sqrt(ts_n))

# REF 781 CS
var           <- ref_781_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_781_sem_cs <- std_time / (sqrt(ts_n))


#### Plot Settings ####
x = 1:12
xlabs = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y_lab_map   <- list(bquote("Forest Cover"), bquote("Percent  (%)"))
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_n     <- list(bquote("Number of "), bquote("Soundings"))
y_lab_nirv  <- "NIRv"
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_665   <- list(bquote("Reflectance"), bquote("665 nm"))
y_lab_781   <- list(bquote("Reflectance"), bquote("781 nm"))

line.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(256))

#### Plot ####
cairo_pdf("G:/SIF_comps/figs/comps_asia_map_black.pdf", width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(raster(cover), col = map.cols, axes=F, legend=F)
plot(raster(cover), col = map.cols, axes=F, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), add = TRUE, ylim = c(-60, 30))
plot(raster(cover), col = map.cols, axes=F, add = TRUE, legend=F)
mtext(3, text = "SE Asia Tropical Forest", col = "white")
box(col = "white")
# Legend
plot(raster(cover), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -3.25, cex.axis=1, tick=F, at=c(80.3, 99.7), labels=c("80","100"), col.axis = "white"),
     smallplot=c(0.175,0.200,0.05,0.95)); par(mar = par("mar"))

# Line Plots
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf, col = line.cols[2], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_sif_cs, ts_sif_cf) - 0.055 * min(ts_sif_cs, ts_sif_cf),
              max(ts_sif_cs, ts_sif_cf) + 0.055 * max(ts_sif_cs, ts_sif_cf)))
lines(x, ts_sif_cs, col = line.cols[2], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_sif_cf - ts_sif_sem_cf, x1 = x, y1 = ts_sif_cf + ts_sif_sem_cf, code=3, angle=90, length=0.05, col = line.cols[2])
arrows(x0 = x, y0 = ts_sif_cs - ts_sif_sem_cs, x1 = x, y1 = ts_sif_cs + ts_sif_sem_cs, code=3, angle=90, length=0.05, col = line.cols[2])
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky", "Cloud Fraction <0.20"), col=c("white", "white"),
       lty=c(2, 1), box.col = "white", text.col = "white", ncol = 2, y.intersp=0.25)
box(col = "white")

# op <- par(mar = c(0,6,0,0.5), bg = "black")
# plot(x, ts_n_cf, col = line.cols[3], type = "l", ylim = c(min(ts_n_cs), max(ts_n_cf)), axes = FALSE, lwd = 1.5)
# lines(x, ts_n_cs, col = line.cols[3], lty = 2, lwd = 1.5)
# axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
# axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
# mtext(2, text = do.call(expression, y_lab_n), col = "white", line = c(4.25, 2.25))
# box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_rad_cf, col = line.cols[3], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_nirv_rad_cs, ts_nirv_rad_cf) - 0.055 * min(ts_nirv_rad_cs, ts_nirv_rad_cf),
              max(ts_nirv_rad_cs, ts_nirv_rad_cf) + 0.055 * max(ts_nirv_rad_cs, ts_nirv_rad_cf)))
lines(x, ts_nirv_rad_cs, col = line.cols[3], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_nirv_rad_cf - ts_nirv_rad_sem_cf, x1 = x, y1 = ts_nirv_rad_cf + ts_nirv_rad_sem_cf, code=3, angle=90, length=0.05, col = line.cols[3])
arrows(x0 = x, y0 = ts_nirv_rad_cs - ts_nirv_rad_sem_cs, x1 = x, y1 = ts_nirv_rad_cs + ts_nirv_rad_sem_cs, code=3, angle=90, length=0.05, col = line.cols[3])
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cf, col = line.cols[4], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_nirv_cs, ts_nirv_cf) - 0.055 * min(ts_nirv_cs, ts_nirv_cf),
              max(ts_nirv_cs, ts_nirv_cf) + 0.055 * max(ts_nirv_cs, ts_nirv_cf)))
lines(x, ts_nirv_cs, col = line.cols[4], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_nirv_cf - ts_nirv_sem_cf, x1 = x, y1 = ts_nirv_cf + ts_nirv_sem_cf, code=3, angle=90, length=0.05, col = line.cols[4])
arrows(x0 = x, y0 = ts_nirv_cs - ts_nirv_sem_cs, x1 = x, y1 = ts_nirv_cs + ts_nirv_sem_cs, code=3, angle=90, length=0.05, col = line.cols[4])
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = y_lab_nirv, col = "white", line = 2.25)
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_665_cf, col = line.cols[5], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_ref_665_cs, ts_ref_665_cf) - 0.055 * min(ts_ref_665_cs, ts_ref_665_cf),
              max(ts_ref_665_cs, ts_ref_665_cf) + 0.055 * max(ts_ref_665_cs, ts_ref_665_cf)))
lines(x, ts_ref_665_cs, col = line.cols[5], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_ref_665_cf - ts_ref_665_sem_cf, x1 = x, y1 = ts_ref_665_cf + ts_ref_665_sem_cf, code=3, angle=90, length=0.05, col = line.cols[5])
arrows(x0 = x, y0 = ts_ref_665_cs - ts_ref_665_sem_cs, x1 = x, y1 = ts_ref_665_cs + ts_ref_665_sem_cs, code=3, angle=90, length=0.05, col = line.cols[5])
axis(1, tck = 0.03, labels = xlabs, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_781_cf, col = line.cols[6], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_ref_781_cs, ts_ref_781_cf) - 0.055 * min(ts_ref_781_cs, ts_ref_781_cf),
              max(ts_ref_781_cs, ts_ref_781_cf) + 0.055 * max(ts_ref_781_cs, ts_ref_781_cf)))
lines(x, ts_ref_781_cs, col = line.cols[6], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_ref_781_cf - ts_ref_781_sem_cf, x1 = x, y1 = ts_ref_781_cf + ts_ref_781_sem_cf, code=3, angle=90, length=0.05, col = line.cols[6])
arrows(x0 = x, y0 = ts_ref_781_cs - ts_ref_781_sem_cs, x1 = x, y1 = ts_ref_781_cs + ts_ref_781_sem_cs, code=3, angle=90, length=0.05, col = line.cols[6])
axis(1, tck = 0.03, labels = xlabs, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()
