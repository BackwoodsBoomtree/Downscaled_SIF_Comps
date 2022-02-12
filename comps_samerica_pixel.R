library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Load Files ####
cf_file       <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
cs_file       <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
cf20_diff_max <- rast("G:/SIF_comps/min_max_diff/1deg/cf20/NIRv_Rad.cf20-SIF.max.cf20.monthly.1deg.2020.tif")
mask_ebf      <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg      <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines    <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Coordinates Note R is not plotting the point correctly for some reason,
# so make it separately

# Amazon 5 diff
# coord   <- vect(cbind(-70.5, 1.5), type = "points", crs = "+proj=longlat +datum=WGS84")
# coord_p <- vect(cbind(-73.25, 1.5), type = "points", crs = "+proj=longlat +datum=WGS84")

# Amazon -3 diff
# coord   <- vect(cbind(-56.5, 1.5), type = "points", crs = "+proj=longlat +datum=WGS84")
# coord_p <- vect(cbind(-61.25, 1.5), type = "points", crs = "+proj=longlat +datum=WGS84")

# Amazon 0 diff
coord   <- vect(cbind(-71.5, -4.5), type = "points", crs = "+proj=longlat +datum=WGS84")
coord_p <- vect(cbind(-74.25, -4.5), type = "points", crs = "+proj=longlat +datum=WGS84")


# Create single mask with cover threshold
m <- mask(mask_ebf, mask_veg)
m[m < 80]  <- NA
# m[m >= 80] <- 1

# Extent and map
samerica_ext  <- ext(-82,-34,-20,13)
cover         <- crop(m, samerica_ext)
cf20_diff_max <- mask(crop(cf20_diff_max, cover), cover)

#### Get the data ####
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

#### Extract the data ####
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


#### Plot Settings ####
x = 1:12
xlabs = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
y_lab_map   <- list(bquote("Peak NIRv Rad"), bquote("- SIF (Months)"))
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_n     <- list(bquote("Number of "), bquote("Soundings"))
y_lab_nirv  <- "NIRv"
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_665   <- list(bquote("Reflectance"), bquote("665 nm"))
y_lab_781   <- list(bquote("Reflectance"), bquote("781 nm"))

map_max <- minmax(cf20_diff_max)[2]
map_min <- minmax(cf20_diff_max)[1]
mag.cols     <- magma(7)
vir.cols     <- viridis(7)
map.cols     <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols     <- (map.cols(256))
diff_max.col <- rev(viridis(abs(map_min) + map_max + 1))

#### Plot ####
cairo_pdf("G:/SIF_comps/figs/comps_samerica_pixel_0_black.pdf", width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(raster(cover), col = map.cols, axes=F, legend=F)
plot(raster(cover), col = map.cols, axes=F, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), add = TRUE, ylim = c(-60, 30))
plot(raster(cf20_diff_max), col = diff_max.col, axes=F, add = TRUE, legend=F)
plot(coord_p, col = "red", lwd = 2, add = TRUE)
mtext(3, text = "South America Tropical Forest", col = "white")
box(col = "white")
# Legend
map_max <- minmax(cf20_diff_max)[2]
map_min <- minmax(cf20_diff_max)[1]
plot(raster(cf20_diff_max), legend.only=TRUE, col=diff_max.col, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -1.75, cex.axis=1, tick=F, at=c(map_min, 0, map_max), labels=c(map_min, "0", map_max), col.axis = "white", hadj = 1),
     smallplot=c(0.175,0.200,0.10,0.90)); par(mar = par("mar"))

# Line Plots
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_sif_cs, ts_sif_cf) - 0.10 * min(ts_sif_cs, ts_sif_cf),
              max(ts_sif_cs, ts_sif_cf) + 0.10 * max(ts_sif_cs, ts_sif_cf)))
lines(x, ts_sif_cs, col = mag.cols[4], lwd = 1.5, lty = 2)
arrows(x0 = x, y0 = ts_sif_cf - ts_sif_sem_cf, x1 = x, y1 = ts_sif_cf + ts_sif_sem_cf, code=3, angle=90, length=0.025, col = "white")
arrows(x0 = x, y0 = ts_sif_cs - ts_sif_sem_cs, x1 = x, y1 = ts_sif_cs + ts_sif_sem_cs, code=3, angle=90, length=0.025, col = "white")
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = c(1, 4, 7, 12), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky", "Cloud <0.20"), col=c("white", "white"),
       lty=c(2, 1), box.col = "white", text.col = "white", horiz = TRUE, y.intersp=0.5)
box(col = "white")

# op <- par(mar = c(0,6,0,0.5), bg = "black")
# plot(x, ts_n_cf, col = line.cols[3], type = "l", ylim = c(min(ts_n_cs), max(ts_n_cf)), axes = FALSE, lwd = 1.5)
# lines(x, ts_n_cs, col = line.cols[3], lty = 2, lwd = 1.5)
# axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
# axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
# mtext(2, text = do.call(expression, y_lab_n), col = "white", line = c(4.25, 2.25))
# box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_781_cf, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_ref_781_cs, ts_ref_781_cf) - 0.10 * min(ts_ref_781_cs, ts_ref_781_cf),
              max(ts_ref_781_cs, ts_ref_781_cf) + 0.10 * max(ts_ref_781_cs, ts_ref_781_cf)))
lines(x, ts_ref_781_cs, col = vir.cols[5], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_ref_781_cf - ts_ref_781_sem_cf, x1 = x, y1 = ts_ref_781_cf + ts_ref_781_sem_cf, code=3, angle=90, length=0.025, col = "white")
arrows(x0 = x, y0 = ts_ref_781_cs - ts_ref_781_sem_cs, x1 = x, y1 = ts_ref_781_cs + ts_ref_781_sem_cs, code=3, angle=90, length=0.025, col = "white")
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = c(1, 4, 7, 12), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cf, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_nirv_cs, ts_nirv_cf) - 0.10 * min(ts_nirv_cs, ts_nirv_cf),
              max(ts_nirv_cs, ts_nirv_cf) + 0.10 * max(ts_nirv_cs, ts_nirv_cf)))
lines(x, ts_nirv_cs, col = mag.cols[5], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_nirv_cf - ts_nirv_sem_cf, x1 = x, y1 = ts_nirv_cf + ts_nirv_sem_cf, code=3, angle=90, length=0.025, col = "white")
arrows(x0 = x, y0 = ts_nirv_cs - ts_nirv_sem_cs, x1 = x, y1 = ts_nirv_cs + ts_nirv_sem_cs, code=3, angle=90, length=0.025, col = "white")
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = c(1, 4, 7, 12), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = y_lab_nirv, col = "white", line = 2.25)
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_665_cf, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_ref_665_cs, ts_ref_665_cf) - 0.10 * min(ts_ref_665_cs, ts_ref_665_cf),
              max(ts_ref_665_cs, ts_ref_665_cf) + 0.10 * max(ts_ref_665_cs, ts_ref_665_cf)))
lines(x, ts_ref_665_cs, col = vir.cols[6], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_ref_665_cf - ts_ref_665_sem_cf, x1 = x, y1 = ts_ref_665_cf + ts_ref_665_sem_cf, code=3, angle=90, length=0.025, col = "white")
arrows(x0 = x, y0 = ts_ref_665_cs - ts_ref_665_sem_cs, x1 = x, y1 = ts_ref_665_cs + ts_ref_665_sem_cs, code=3, angle=90, length=0.025, col = "white")
axis(1, tck = 0.03, labels = xlabs, at = x, col.axis = "white", col = "white", cex.axis = 0.80, mgp=c(3, 0.1, 0))
axis(1, tck = 0.06, labels = FALSE, at = c(1, 4, 7, 12), col.axis = "white", col = "white")
axis(1, labels = "2020", tck = FALSE, at = 6.5, mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_rad_cf, col = mag.cols[6], type = "l", axes = FALSE, lwd = 1.5,
     ylim = c(min(ts_nirv_rad_cs, ts_nirv_rad_cf) - 0.10 * min(ts_nirv_rad_cs, ts_nirv_rad_cf),
              max(ts_nirv_rad_cs, ts_nirv_rad_cf) + 0.10 * max(ts_nirv_rad_cs, ts_nirv_rad_cf)))
lines(x, ts_nirv_rad_cs, col = mag.cols[6], lty = 2, lwd = 1.5)
arrows(x0 = x, y0 = ts_nirv_rad_cf - ts_nirv_rad_sem_cf, x1 = x, y1 = ts_nirv_rad_cf + ts_nirv_rad_sem_cf, code=3, angle=90, length=0.025, col = "white")
arrows(x0 = x, y0 = ts_nirv_rad_cs - ts_nirv_rad_sem_cs, x1 = x, y1 = ts_nirv_rad_cs + ts_nirv_rad_sem_cs, code=3, angle=90, length=0.025, col = "white")
axis(1, tck = 0.03, labels = xlabs, at = x, col.axis = "white", col = "white", cex.axis = 0.80, mgp=c(3, 0.1, 0))
axis(1, tck = 0.06, labels = FALSE, at = c(1, 4, 7, 12), col.axis = "white", col = "white")
axis(1, labels = "2020", tck = FALSE, at = 6.5, mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()
