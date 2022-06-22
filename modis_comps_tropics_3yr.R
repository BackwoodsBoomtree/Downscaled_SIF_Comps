library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/modis_comps_tropics_3yr_black.pdf"

#### Load Files ####

### MODIS
mod_red_2019 <- "G:/MCD43C4/nc_annual/monthly/1deg/RED/MCD43C4.A2019.RED.Monthly.1deg.nc"
mod_red_2020 <- "G:/MCD43C4/nc_annual/monthly/1deg/RED/MCD43C4.A2020.RED.Monthly.1deg.nc"
mod_red_2021 <- "G:/MCD43C4/nc_annual/monthly/1deg/RED/MCD43C4.A2021.RED.Monthly.1deg.nc"

mod_nir_2019 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIR/MCD43C4.A2019.NIR.Monthly.1deg.nc"
mod_nir_2020 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIR/MCD43C4.A2020.NIR.Monthly.1deg.nc"
mod_nir_2021 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIR/MCD43C4.A2021.NIR.Monthly.1deg.nc"

mod_nirv_2019 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIRv/MCD43C4.A2019.NIRv.Monthly.1deg.nc"
mod_nirv_2020 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIRv/MCD43C4.A2020.NIRv.Monthly.1deg.nc"
mod_nirv_2021 <- "G:/MCD43C4/nc_annual/monthly/1deg/NIRv/MCD43C4.A2021.NIRv.Monthly.1deg.nc"


### All PAs, cold
cs_all_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
cs_all_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
cs_all_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.nc"
cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.nc"
cs_2021    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"

# Masks
mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Create single mask with cover threshold
mask_total <- mask(mask_ebf, mask_veg)
mask_total[mask_total < 80]  <- NA
# m[m >= 80] <- 1

# Extent
tropics_ext  <- extent(c(-120, 155, -20, 13))
cover        <- crop(mask_total, tropics_ext)

ts_mod_red_2019 <- crop(mask(rast(mod_red_2019, subds = "RED"), mask_total), tropics_ext)
ts_mod_red_2019 <- global(ts_mod_red_2019, fun = "mean", na.rm = TRUE)
ts_mod_red_2019 <- as.vector(unlist(ts_mod_red_2019))

# For tropomi data
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

# For MOD data
get_ts_mod <- function(v, f, e, m){
  
  for (i in 1:length(f)) {
    group_avg  <- crop(mask(rast(f[i], subds = v), m), e)
    group_avg  <- global(group_avg, fun = "mean", na.rm = TRUE)
    group_avg  <- as.vector(unlist(group_avg))
    
    if (i == 1){
      ts_avg <- group_avg
    } else {
      ts_avg <- c(ts_avg, group_avg)
    }
  }
  return(ts_avg)
}

# Normalize data
min_max_norm <- function(x) {
  for (i in 1:length(x)) {
    v <- (x[i] - min(x)) / (max(x) - min(x))
    if (i == 1) {
      out <- v
    } else {
      out <- c(out, v)
    }
  }
  return(out)
}

# Get timeseries Tropomi data
ts_sif_cs_all      <- min_max_norm(get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]])
ts_nirv_cs_all     <- min_max_norm(get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]])
ts_nirv_rad_cs_all <- min_max_norm(get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]])
ts_ref_665_cs_all  <- min_max_norm(get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]])
ts_ref_781_cs_all  <- min_max_norm(get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]])

ts_sif_cs      <- min_max_norm(get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]])
ts_nirv_cs     <- min_max_norm(get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]])
ts_nirv_rad_cs <- min_max_norm(get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]])
ts_ref_665_cs  <- min_max_norm(get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]])
ts_ref_781_cs  <- min_max_norm(get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]])

# Get timeseries MOD data
ts_mod_red  <- min_max_norm(get_ts_mod("RED", c(mod_red_2019, mod_red_2020, mod_red_2021), tropics_ext, mask_total)/1000)
ts_mod_nir  <- min_max_norm(get_ts_mod("NIR", c(mod_nir_2019, mod_nir_2020, mod_nir_2021), tropics_ext, mask_total)/10000)
ts_mod_nirv <- min_max_norm(get_ts_mod("NIRv", c(mod_nirv_2019, mod_nirv_2020, mod_nirv_2021), tropics_ext, mask_total)/10000)


# convert red to absorbance
ts_ref_665_cs_all <- 1-ts_ref_665_cs_all
ts_ref_665_cs     <- 1-ts_ref_665_cs
# MOD
ts_mod_red        <- 1-ts_mod_red

#### Plot Settings ####
x           <- 1:36
xlabs       <- c("Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct")
y_lab_sif   <- list(bquote("SIF"), bquote("(Normalized)"))
y_lab_nirv  <- list(bquote("NIRv Reflectance"), bquote("(Normalized)"))
y_lab_665   <- list(bquote("Red Absorbance"), bquote("(Normalized)"))
y_lab_781   <- list(bquote("NIR Reflectance"), bquote("(Normalized)"))

mag.cols <- magma(7)
vir.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(256))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 3.25)

par(mfrow = c(2, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Line Plots
# SIF
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cs, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs) - 0.10 * min(ts_sif_cs),
              max(ts_sif_cs) + 0.10 * max(ts_sif_cs)))
rect(13, -10, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_sif_cs, col = mag.cols[4], lwd = 1.5)
lines(x, ts_sif_cs_all, col = mag.cols[4], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("S5P Clear Sky (w/ Hotspot)", "S5P Clear Sky (No Hotspot)", "MCD43C4"), col=c("white", "white", "gray50"),
       lty=c(1, 3, 1), box.col = "white", text.col = "white", horiz = FALSE, y.intersp = 1, cex = 0.75)
box(col = "white")

# REF 665
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_665_cs, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i", lty = 3,
     ylim = c(min(ts_mod_red, ts_ref_665_cs, ts_ref_665_cs_all) - 0.01,
              max(ts_mod_red, ts_ref_665_cs, ts_ref_665_cs_all) + 0.01))
rect(13, -10, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_665_cs, col = vir.cols[6], lty = 3, lwd = 1.5)
lines(x, ts_ref_665_cs_all, col = vir.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_mod_red, col = "gray50", lty = 1, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRv
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cs, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i", lty = 3,
     ylim = c(min(ts_mod_nirv, ts_nirv_cs) - 0.10 * min(ts_nirv_cs),
              max(ts_mod_nirv, ts_nirv_cs) + 0.10 * max(ts_nirv_cs)))
rect(13, -10, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_cs, col = mag.cols[5], lty = 3, lwd = 1.5)
lines(x, ts_nirv_cs_all, col = mag.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_mod_nirv, col = "gray50", lty = 1, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirv), col = "white", line = c(4.25, 2.25))
box(col = "white")

# REF 781
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_ref_781_cs, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i", lty = 3,
     ylim = c(min(ts_mod_nir, ts_ref_781_cs, ts_ref_781_cs_all) - 0.01,
              max(ts_mod_nir, ts_ref_781_cs, ts_ref_781_cs_all) + 0.01))
rect(13, -10, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_ref_781_cs, col = vir.cols[5], lty = 3, lwd = 1.5)
lines(x, ts_ref_781_cs_all, col = vir.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_mod_nir, col = "gray50", lty = 1, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

mtext(3, text = "Tropical Forest", col = "white", outer = TRUE)

dev.off()

