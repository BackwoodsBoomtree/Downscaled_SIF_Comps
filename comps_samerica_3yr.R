library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
# out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_black.pdf"
# out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_cold_black.pdf"
# out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_hot_black.pdf"
# out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_strict_black.pdf"
# out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_cold_strict_black.pdf"
out_name   <- "G:/SIF_comps/figs/comps_samerica_map_3yr_cold_all_black.pdf"

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
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.nc"
# cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
# cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc"
# 
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.strict.nc"
# cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.strict.nc"
# cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.strict.nc"

# ### Strict and cold
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.cold.nc"
# cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.cold.nc"
# cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.cold.nc"
# 
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.strict.nc"
# cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.strict.nc"
# cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.strict.nc"

### All PAs, cold, cold CF
cs_all_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
cs_all_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
cs_all_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.nc"
cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.nc"
cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"

cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.cold.nc"
cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.cold.nc"
cf_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.cold.nc"

# Masks
mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Create single mask with cover threshold
mask_total <- mask(mask_ebf, mask_veg)
mask_total[mask_total < 80]  <- NA
# m[m >= 80] <- 1

# Extent
samerica_ext   <- extent(c(-82,-34,-20,13))
samerica_cover <- crop(mask_total, samerica_ext)

# Returns standard error of mean and weighted global mean of variable (v) using number of
# observations (n) and standard deviation (s) for entire time series raster from file list (f)
# using defined extent (e) and mask (m)
get_ts <- function(v, n, s, f, e, m){
  
  for (i in 1:length(f)) {
    n_obs      <- crop(mask(rast(f[i], subds = n), m), e)
    group_avg  <- crop(mask(rast(f[i], subds = v), m), e)
    group_var  <- crop(mask(rast(f[i], subds = s), m), e)^2
    
    for (j in 1:length(depth(n_obs))) {
      pop_avg <- global(group_avg[[j]], fun = "mean", na.rm = TRUE, weights = n_obs[[j]])
      pop_avg <- as.vector(unlist(pop_avg))
      
      pop_n   <- global(n_obs[[j]], fun = "sum", na.rm = TRUE)
      pop_n   <- as.vector(unlist(pop_n))
      
      # Between group variance
      bgv <- n_obs[[j]] * (group_avg[[j]] - pop_avg)^2
      bgv <- global(bgv, fun = "sum", na.rm = TRUE)
      
      # Within group variance
      wgv <- n_obs[[j]] * group_var[[j]]
      wgv <- global(wgv, fun = "sum", na.rm = TRUE)
      
      # Population variance
      pop_var <- (bgv + wgv) / pop_n
      
      # Population SEM
      pop_sem <- sqrt(pop_var) / sqrt(pop_n)
      pop_sem <- as.vector(unlist(pop_sem))
      
      if (i == 1 && j == 1){
        ts_avg <- pop_avg
        ts_sem <- pop_sem
      } else {
        ts_avg <- c(ts_avg, pop_avg)
        ts_sem <- c(ts_sem, pop_sem)
      }
    }
  }
  return(list(ts_avg, ts_sem))
}

ts_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]

ts_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]

ts_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]

# convert red to absorbance
ts_ref_665_cs_all <- 1-ts_ref_665_cs_all
ts_ref_665_cs     <- 1-ts_ref_665_cs
ts_ref_665_cf     <- 1-ts_ref_665_cf

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
map.cols  <- (map.cols(256))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(raster(samerica_cover), col = map.cols, axes=F, legend=F)
plot(raster(samerica_cover), col = map.cols, axes=F, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), add = TRUE, ylim = c(-60, 30))
plot(raster(samerica_cover), col = map.cols, axes=F, add = TRUE, legend=F)
mtext(3, text = "South America Tropical Forest", col = "white")
box(col = "white")
# Legend
plot(raster(samerica_cover), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -1.75, cex.axis=1, tick=F, at=c(81, 100), labels=c("80","100"), col.axis = "white", hadj = 1),
     smallplot=c(0.175,0.200,0.10,0.90)); par(mar = par("mar"))

# Line Plots
# SIF
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cs, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs, ts_sif_cf) - 0.10 * min(ts_sif_cs, ts_sif_cf),
              max(ts_sif_cs, ts_sif_cf) + 0.10 * max(ts_sif_cs, ts_sif_cf)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_sif_cs, col = mag.cols[4], lwd = 1.5)
lines(x, ts_sif_cf, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_sif_cs_all, col = mag.cols[4], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky (w/ Hotspot)", "Clear Sky (No Hotspot)", "Cloud <0.20 (No Hotspot)"), col=c("white", "white", "white"),
       lty=c(3, 1, 2), box.col = "white", text.col = "white", horiz = FALSE, y.intersp = 1, cex = 0.75)
box(col = "white")

# REF 665
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_665_cs, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_ref_665_cs, ts_ref_665_cf, ts_ref_665_cs_all) - 0.01,
              max(ts_ref_665_cs, ts_ref_665_cf, ts_ref_665_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_665_cs, col = vir.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_ref_665_cf, col = vir.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_ref_665_cs_all, col = vir.cols[6], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRv
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cs, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_cs, ts_nirv_cf) - 0.10 * min(ts_nirv_cs, ts_nirv_cf),
              max(ts_nirv_cs, ts_nirv_cf) + 0.10 * max(ts_nirv_cs, ts_nirv_cf)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_cs, col = mag.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nirv_cf, col = mag.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_nirv_cs_all, col = mag.cols[5], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirv), col = "white", line = c(4.25, 2.25))
box(col = "white")

# REF 781
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_781_cs, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_ref_781_cs, ts_ref_781_cf, ts_ref_781_cs_all) - 0.01,
              max(ts_ref_781_cs, ts_ref_781_cf, ts_ref_781_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_781_cs, col = vir.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_ref_781_cf, col = vir.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_ref_781_cs_all, col = vir.cols[5], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

#NIRV Rad
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_rad_cs, col = mag.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_rad_cs, ts_nirv_rad_cf, ts_nirv_rad_cs_all) - 2,
              max(ts_nirv_rad_cs, ts_nirv_rad_cf, ts_nirv_rad_cs_all) + 2))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_rad_cs, col = mag.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_nirv_rad_cf, col = mag.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_nirv_rad_cs_all, col = mag.cols[6], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()

