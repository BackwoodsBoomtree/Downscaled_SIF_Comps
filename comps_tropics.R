library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
# out_name   <- "G:/SIF_comps/figs/comps_tropics_map_3yr_black.pdf"
# out_name   <- "G:/SIF_comps/figs/comps_tropics_map_3yr_cold_black.pdf"
out_name   <- "G:/SIF_comps/figs/comps_tropics_map_3yr_strict_black.pdf"

#### Load Files ####

### Normal 
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.nc"
# cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
# cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc"
# 
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
# cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
# cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

### Phase Angle > 20
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.cold.nc"
# cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.cold.nc"
# cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.cold.nc"
# 
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.nc"
# cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.nc"
# cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"

### Phase Angle < 20
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.hot.nc"
# cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.hot.nc"
# cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.hot.nc"
# 
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.hot.nc"
# cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.hot.nc"
# cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.hot.nc"

### Strict radiance (cloud) filter of Mean_TOA_Radiance < 150 W m2/sr/um (only applies to clearsky data)
cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.nc"
cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc"

cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.strict.nc"
cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.strict.nc"
cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.strict.nc"


# Masks
mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Create single mask with cover threshold
m <- mask(mask_ebf, mask_veg)
m[m < 80]  <- NA
# m[m >= 80] <- 1

# Extent
tropics_ext  <- extent(c(-120, 155, -20, 13))
cover        <- crop(m, tropics_ext)

#### Get the data ####
n_cf        <- crop(mask(c(rast(cf_2019, subds = "n"), rast(cf_2020, subds = "n"), rast(cf_2021, subds = "n")), m), tropics_ext)
nirv_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv"), rast(cf_2020, subds = "NIRv"), rast(cf_2021, subds = "NIRv")), m), tropics_ext)
nirv_rad_cf <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad"), rast(cf_2020, subds = "NIRv_Rad"), rast(cf_2021, subds = "NIRv_Rad")), m), tropics_ext)
sif_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743"), rast(cf_2020, subds = "SIF_743"), rast(cf_2021, subds = "SIF_743")), m), tropics_ext)
ref_665_cf  <- crop(mask(c(rast(cf_2019, subds = "REF_665"), rast(cf_2020, subds = "REF_665"), rast(cf_2021, subds = "REF_665")), m), tropics_ext)
ref_781_cf  <- crop(mask(c(rast(cf_2019, subds = "REF_781"), rast(cf_2020, subds = "REF_781"), rast(cf_2021, subds = "REF_781")), m), tropics_ext)

nirv_std_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv_std"), rast(cf_2020, subds = "NIRv_std"), rast(cf_2021, subds = "NIRv_std")), m), tropics_ext)
nirv_rad_std_cf <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad_std"), rast(cf_2020, subds = "NIRv_Rad_std"), rast(cf_2021, subds = "NIRv_Rad_std")), m), tropics_ext)
sif_std_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743_std"), rast(cf_2020, subds = "SIF_743_std"), rast(cf_2021, subds = "SIF_743_std")), m), tropics_ext)
ref_665_std_cf  <- crop(mask(c(rast(cf_2019, subds = "REF_665_std"), rast(cf_2020, subds = "REF_665_std"), rast(cf_2021, subds = "REF_665_std")), m), tropics_ext)
ref_781_std_cf  <- crop(mask(c(rast(cf_2019, subds = "REF_781_std"), rast(cf_2020, subds = "REF_781_std"), rast(cf_2021, subds = "REF_781_std")), m), tropics_ext)

n_cs        <- crop(mask(c(rast(cs_2019, subds = "n"), rast(cs_2020, subds = "n"), rast(cs_2021, subds = "n")), m), tropics_ext)
nirv_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv"), rast(cs_2020, subds = "NIRv"), rast(cs_2021, subds = "NIRv")), m), tropics_ext)
nirv_rad_cs <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad"), rast(cs_2020, subds = "NIRv_Rad"), rast(cs_2021, subds = "NIRv_Rad")), m), tropics_ext)
sif_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743"), rast(cs_2020, subds = "SIF_743"), rast(cs_2021, subds = "SIF_743")), m), tropics_ext)
ref_665_cs  <- crop(mask(c(rast(cs_2019, subds = "REF_665"), rast(cs_2020, subds = "REF_665"), rast(cs_2021, subds = "REF_665")), m), tropics_ext)
ref_781_cs  <- crop(mask(c(rast(cs_2019, subds = "REF_781"), rast(cs_2020, subds = "REF_781"), rast(cs_2021, subds = "REF_781")), m), tropics_ext)

nirv_std_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv_std"), rast(cs_2020, subds = "NIRv_std"), rast(cs_2021, subds = "NIRv_std")), m), tropics_ext)
nirv_rad_std_cs <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad_std"), rast(cs_2020, subds = "NIRv_Rad_std"), rast(cs_2021, subds = "NIRv_Rad_std")), m), tropics_ext)
sif_std_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743_std"), rast(cs_2020, subds = "SIF_743_std"), rast(cs_2021, subds = "SIF_743_std")), m), tropics_ext)
ref_665_std_cs  <- crop(mask(c(rast(cs_2019, subds = "REF_665_std"), rast(cs_2020, subds = "REF_665_std"), rast(cs_2021, subds = "REF_665_std")), m), tropics_ext)
ref_781_std_cs  <- crop(mask(c(rast(cs_2019, subds = "REF_781_std"), rast(cs_2020, subds = "REF_781_std"), rast(cs_2021, subds = "REF_781_std")), m), tropics_ext)


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
ts_n_cf <- global(n_cf, fun = "sum", na.rm = TRUE)[[1]]
ts_n_cs <- global(n_cs, fun = "sum", na.rm = TRUE)[[1]]

# SIF CF20
var           <- sif_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_sif_sem_cf <- std_time / (sqrt(ts_n_cf))

# SIF CS
var           <- sif_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_sif_sem_cs <- std_time / (sqrt(ts_n_cs))

# NIRv CF20
var           <- nirv_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_sem_cf <- std_time / (sqrt(ts_n_cf))

# NIRv CS
var           <- nirv_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_sem_cs <- std_time / (sqrt(ts_n_cs))

# NIRv Rad CF20
var           <- nirv_rad_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_rad_sem_cf <- std_time / (sqrt(ts_n_cf))

# NIRv Rad CS
var           <- nirv_rad_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_nirv_rad_sem_cs <- std_time / (sqrt(ts_n_cs))

# REF 665 CF20
var           <- ref_665_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_665_sem_cf <- std_time / (sqrt(ts_n_cf))

# REF 665 CS
var           <- ref_665_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_665_sem_cs <- std_time / (sqrt(ts_n_cs))

# REF 781 CF20
var           <- ref_781_std_cf^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_781_sem_cf <- std_time / (sqrt(ts_n_cf))

# REF 781 CS
var           <- ref_781_std_cs^2
var_tot       <- global(var, fun = "sum", na.rm = TRUE)[[1]]
std_time      <- sqrt(var_tot)
ts_ref_781_sem_cs <- std_time / (sqrt(ts_n_cs))


#### Plot Settings ####
x           <- 1:36
xlabs       <- c("Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct")
y_lab_map   <- list(bquote("Forest Cover"), bquote("Percent  (%)"))
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_n     <- list(bquote("Number of "), bquote("Soundings"))
y_lab_nirv  <- "NIRv"
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_665   <- list(bquote("Reflectance"), bquote("665 nm"))
y_lab_781   <- list(bquote("Reflectance"), bquote("781 nm"))

mag.cols <- magma(7)
vir.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(256))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(raster(cover), col = map.cols, axes=F, legend=F)
plot(raster(cover), col = map.cols, axes=F, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), add = TRUE, ylim = c(-60, 30))
plot(raster(cover), col = map.cols, axes=F, add = TRUE, legend=F)
mtext(3, text = "Tropical Forest", col = "white")
box(col = "white")
# Legend
plot(raster(cover), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -1.75, cex.axis=1, tick=F, at=c(81, 100), labels=c("80","100"), col.axis = "white", hadj = 1),
     smallplot=c(0.175,0.200,0.10,0.90)); par(mar = par("mar"))

# Line Plots
# SIF
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs, ts_sif_cf) - 0.10 * min(ts_sif_cs, ts_sif_cf),
              max(ts_sif_cs, ts_sif_cf) + 0.10 * max(ts_sif_cs, ts_sif_cf)))
rect(13, 0, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_sif_cf, col = mag.cols[4], lwd = 1.5)
lines(x, ts_sif_cs, col = mag.cols[4], lwd = 1.5, lty = 2)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky", "Cloud <0.20"), col=c("white", "white"),
       lty=c(2, 1), box.col = "white", text.col = "white", horiz = TRUE, y.intersp=0.5)
box(col = "white")

# op <- par(mar = c(0,6,0,0.5), bg = "black")
# plot(x, ts_n_cf, col = mag.cols[3], type = "l", ylim = c(min(ts_n_cs), max(ts_n_cf)), axes = FALSE, lwd = 1.5)
# lines(x, ts_n_cs, col = mag.cols[3], lty = 2, lwd = 1.5)
# axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
# axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
# mtext(2, text = do.call(expression, y_lab_n), col = "white", line = c(4.25, 2.25))
# box(col = "white")

# REF 781
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_781_cf, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_ref_781_cs, ts_ref_781_cf) - 0.10 * min(ts_ref_781_cs, ts_ref_781_cf),
              max(ts_ref_781_cs, ts_ref_781_cf) + 0.10 * max(ts_ref_781_cs, ts_ref_781_cf)))
rect(13, 0, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_781_cf, col = vir.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_ref_781_cs, col = vir.cols[5], lty = 2, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRv
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cf, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_cs, ts_nirv_cf) - 0.10 * min(ts_nirv_cs, ts_nirv_cf),
              max(ts_nirv_cs, ts_nirv_cf) + 0.10 * max(ts_nirv_cs, ts_nirv_cf)))
rect(13, 0, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_cf, col = mag.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nirv_cs, col = mag.cols[5], lty = 2, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = y_lab_nirv, col = "white", line = 2.25)
box(col = "white")

# REF 665
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_665_cf, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_ref_665_cs, ts_ref_665_cf) - 0.10 * min(ts_ref_665_cs, ts_ref_665_cf),
              max(ts_ref_665_cs, ts_ref_665_cf) + 0.10 * max(ts_ref_665_cs, ts_ref_665_cf)))
rect(13, 0, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_665_cf, col = vir.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_ref_665_cs, col = vir.cols[6], lty = 2, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

#NIRV Rad
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_rad_cf, col = mag.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_rad_cs, ts_nirv_rad_cf) - 0.10 * min(ts_nirv_rad_cs, ts_nirv_rad_cf),
              max(ts_nirv_rad_cs, ts_nirv_rad_cf) + 0.10 * max(ts_nirv_rad_cs, ts_nirv_rad_cf)))
rect(13, 0, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_rad_cf, col = mag.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_nirv_rad_cs, col = mag.cols[6], lty = 2, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()

