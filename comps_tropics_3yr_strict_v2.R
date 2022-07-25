library(terra)
library(raster)
library(viridis)
library(RColorBrewer)

#### Output PDF name ####
out_name    <- "G:/SIF_comps/figs/v2/comps_tropics_map_3yr_cold_all_strict_black_v2.pdf"
data_prefix <- "G:/SIF_comps/csv/tropics/strict/Tropics_2019-2021_strict_"

#### Grab the data ####

ts_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_sif_cs_cold <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_sif_cf_cold <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean

ts_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_nirv_cs_cold <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_nirv_cf_cold <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean

ts_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_nirvr_cs_cold <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_nirvr_cf_cold <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean

ts_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_red_cs_cold <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_red_cf_cold <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean

ts_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_nir_cs_cold <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_nir_cf_cold <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean

# convert red to absorbance
ts_red_cs_all  <- 1-ts_red_cs_all
ts_red_cs_cold <- 1-ts_red_cs_cold
ts_red_cf_cold <- 1-ts_red_cf_cold

#### Get shapes and forest cover ####

coastlines     <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
tropics_ext    <- ext(c(-180, 180, -23.5, 23.5))

mcd12_majority <- rast("G:/MCD12C1/2020/reprocessed/percent/MCD12C1.A2020001.006.Percent_LC_03.tif")
mcd12_majority <- crop(mcd12_majority, tropics_ext)
mcd12_majority[mcd12_majority < 90]  <- NA

#### Plot Settings ####
x           <- 1:36
xlabs       <- c("Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct")
y_lab_map   <- list(bquote("Forest Cover"), bquote("Percent  (%)"))
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_n     <- list(bquote("Number of "), bquote("Soundings"))
y_lab_nirv  <- list(bquote("NIRv"), bquote("(Reflectance)"))
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_665   <- list(bquote("Red"), bquote("Absorbance"))
y_lab_781   <- list(bquote("NIR"), bquote("Reflectance"))

mag.cols <- magma(7)
vir.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(11))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xlim = c(-85, 160), ylim = c(-23.5, 23.5), mar = c(0,6,0,0.5))
plot(mcd12_majority, col = map.cols, add = TRUE, legend = FALSE)
mtext(3, text = "Tropical Forest", col = "white")
box(col = "white")
# Legend
plot(raster(mcd12_majority), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -1.75, cex.axis=1, tick=F, at=c(90, 100), labels=c("90","100"), col.axis = "white", hadj = 1),
     smallplot=c(0.175,0.200,0.10,0.90)); par(mar = par("mar"))

# Line Plots
# SIF
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cs_cold, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_all, ts_sif_cf_cold) - 0.10 * min(ts_sif_cs_all, ts_sif_cf_cold),
              max(ts_sif_cs_all, ts_sif_cf_cold) + 0.10 * max(ts_sif_cs_all, ts_sif_cf_cold)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_sif_cs_cold, col = mag.cols[4], lwd = 1.5)
lines(x, ts_sif_cf_cold, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_sif_cs_all, col = mag.cols[4], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky (w/ Hotspot)", "Clear Sky (No Hotspot)", "Cloud <0.20 (No Hotspot)"), col=c("white", "white", "white"),
       lty=c(3, 1, 2), box.col = "white", text.col = "white", horiz = FALSE, y.intersp = 1, cex = 0.75)
box(col = "white")

# RED
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_red_cs_cold, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_red_cs_cold, ts_red_cf_cold, ts_red_cs_all) - 0.01,
              max(ts_red_cs_cold, ts_red_cf_cold, ts_red_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_red_cs_cold, col = vir.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_red_cf_cold, col = vir.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_red_cs_all, col = vir.cols[6], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRv
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cs_cold, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_cs_cold, ts_nirv_cf_cold) - 0.10 * min(ts_nirv_cs_cold, ts_nirv_cf_cold),
              max(ts_nirv_cs_cold, ts_nirv_cf_cold) + 0.10 * max(ts_nirv_cs_cold, ts_nirv_cf_cold)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_cs_cold, col = mag.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nirv_cf_cold, col = mag.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_nirv_cs_all, col = mag.cols[5], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirv), col = "white", line = c(4.25, 2.25))
box(col = "white")

# REF 781
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nir_cs_cold, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nir_cs_cold, ts_nir_cf_cold, ts_nir_cs_all) - 0.01,
              max(ts_nir_cs_cold, ts_nir_cf_cold, ts_nir_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nir_cs_cold, col = vir.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nir_cf_cold, col = vir.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_nir_cs_all, col = vir.cols[5], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRV Rad
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirvr_cs_cold, col = mag.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirvr_cs_cold, ts_nirvr_cf_cold, ts_nirvr_cs_all) - 2,
              max(ts_nirvr_cs_cold, ts_nirvr_cf_cold, ts_nirvr_cs_all) + 2))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirvr_cs_cold, col = mag.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_nirvr_cf_cold, col = mag.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_nirvr_cs_all, col = mag.cols[6], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()

