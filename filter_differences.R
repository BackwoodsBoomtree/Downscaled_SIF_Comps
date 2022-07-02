library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/filter_differences.pdf"

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
tropics_ext  <- extent(c(-120, 155, -20, 13))
samerica_ext <- extent(c(-82,-34,-20,13))
africa_ext   <- extent(c(7,45,-5,5))
seasia_ext   <- extent(c(95,155,-11,10))

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

# Get tropics data
ts_tropics_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), tropics_ext, mask_total)[[1]]

ts_tropics_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), tropics_ext, mask_total)[[1]]

ts_tropics_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), tropics_ext, mask_total)[[1]]

# convert red to absorbance
ts_tropics_ref_665_cs_all <- 1-ts_tropics_ref_665_cs_all
ts_tropics_ref_665_cs     <- 1-ts_tropics_ref_665_cs
ts_tropics_ref_665_cf     <- 1-ts_tropics_ref_665_cf

# Differences
# Tropics
dif_tropics_sif_hotspot      <- (ts_tropics_sif_cs_all - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100
dif_tropics_sif_cloud        <- (ts_tropics_sif_cf - ts_tropics_sif_cs_all) / abs(ts_tropics_sif_cs_all) * 100
combo_tropics_sif            <- dif_tropics_sif_hotspot + dif_tropics_sif_cloud

dif_tropics_ref_665_hotspot  <- (ts_tropics_ref_665_cs_all - ts_tropics_ref_665_cs) / abs(ts_tropics_ref_665_cs) * 100
dif_tropics_ref_665_cloud    <- (ts_tropics_ref_665_cf - ts_tropics_ref_665_cs_all) / abs(ts_tropics_ref_665_cs_all) * 100
combo_tropics_ref_665        <- dif_tropics_ref_665_hotspot + dif_tropics_ref_665_cloud

dif_tropics_ref_781_hotspot  <- (ts_tropics_ref_781_cs_all - ts_tropics_ref_781_cs) / abs(ts_tropics_ref_781_cs) * 100
dif_tropics_ref_781_cloud    <- (ts_tropics_ref_781_cf - ts_tropics_ref_781_cs_all) / abs(ts_tropics_ref_781_cs_all) * 100
combo_tropics_ref_781        <- dif_tropics_ref_781_hotspot + dif_tropics_ref_781_cloud

dif_tropics_nirv_hotspot     <- (ts_tropics_nirv_cs_all - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100
dif_tropics_nirv_cloud       <- (ts_tropics_nirv_cf - ts_tropics_nirv_cs_all) / abs(ts_tropics_nirv_cs_all) * 100
combo_tropics_nirv           <- dif_tropics_nirv_hotspot + dif_tropics_nirv_cloud

dif_tropics_nirv_rad_hotspot <- (ts_tropics_nirv_rad_cs_all - ts_tropics_nirv_rad_cs) / abs(ts_tropics_nirv_rad_cs) * 100
dif_tropics_nirv_rad_cloud   <- (ts_tropics_nirv_rad_cf - ts_tropics_nirv_rad_cs_all) / abs(ts_tropics_nirv_rad_cs_all) * 100
combo_tropics_nirv_rad       <- dif_tropics_nirv_rad_hotspot + dif_tropics_nirv_rad_cloud

# Get samerica data
ts_samerica_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), samerica_ext, mask_total)[[1]]

ts_samerica_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), samerica_ext, mask_total)[[1]]

ts_samerica_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), samerica_ext, mask_total)[[1]]

# convert red to absorbance
ts_samerica_ref_665_cs_all <- 1-ts_samerica_ref_665_cs_all
ts_samerica_ref_665_cs     <- 1-ts_samerica_ref_665_cs
ts_samerica_ref_665_cf     <- 1-ts_samerica_ref_665_cf

# Differences
# samerica
dif_samerica_sif_hotspot      <- (ts_samerica_sif_cs_all - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100
dif_samerica_sif_cloud        <- (ts_samerica_sif_cf - ts_samerica_sif_cs_all) / abs(ts_samerica_sif_cs_all) * 100
combo_samerica_sif            <- dif_samerica_sif_hotspot + dif_samerica_sif_cloud

dif_samerica_ref_665_hotspot  <- (ts_samerica_ref_665_cs_all - ts_samerica_ref_665_cs) / abs(ts_samerica_ref_665_cs) * 100
dif_samerica_ref_665_cloud    <- (ts_samerica_ref_665_cf - ts_samerica_ref_665_cs_all) / abs(ts_samerica_ref_665_cs_all) * 100
combo_samerica_ref_665        <- dif_samerica_ref_665_hotspot + dif_samerica_ref_665_cloud

dif_samerica_ref_781_hotspot  <- (ts_samerica_ref_781_cs_all - ts_samerica_ref_781_cs) / abs(ts_samerica_ref_781_cs) * 100
dif_samerica_ref_781_cloud    <- (ts_samerica_ref_781_cf - ts_samerica_ref_781_cs_all) / abs(ts_samerica_ref_781_cs_all) * 100
combo_samerica_ref_781        <- dif_samerica_ref_781_hotspot + dif_samerica_ref_781_cloud

dif_samerica_nirv_hotspot     <- (ts_samerica_nirv_cs_all - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100
dif_samerica_nirv_cloud       <- (ts_samerica_nirv_cf - ts_samerica_nirv_cs_all) / abs(ts_samerica_nirv_cs_all) * 100
combo_samerica_nirv           <- dif_samerica_nirv_hotspot + dif_samerica_nirv_cloud

dif_samerica_nirv_rad_hotspot <- (ts_samerica_nirv_rad_cs_all - ts_samerica_nirv_rad_cs) / abs(ts_samerica_nirv_rad_cs) * 100
dif_samerica_nirv_rad_cloud   <- (ts_samerica_nirv_rad_cf - ts_samerica_nirv_rad_cs_all) / abs(ts_samerica_nirv_rad_cs_all) * 100
combo_samerica_nirv_rad       <- dif_samerica_nirv_rad_hotspot + dif_samerica_nirv_rad_cloud


# Labels
x_lab      <- c(1, 13, 25, 36)
lab_tropic <- "Tropical Forest (TF)"
lab_sa     <- "S American TF"
lab_africa <- "African TF"
lab_seasia <- "SE Asian TF"
lab_sif    <- bquote("SIF (%)")
lab_nirv   <- bquote("NIRv Reflectance (%)")
lab_nirvr  <- bquote("NIRv Radiance (%)")
lab_665    <- bquote("Red Absorbance (%)")
lab_781    <- bquote("NIR Reflectance (%)")
lab_hot    <- bquote("Hotspot effect")
lab_cloud  <- bquote("Cloud effect")
lab_combo  <- bquote("Combined effect")

#### PLOT ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(5, 4), oma=c(2.5,2,0,1))

# SIF
# tropics
op <- par(mar = c(0,3,4,0.5))
plot(dif_tropics_sif_hotspot, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 2,
     ylim = c(min(dif_tropics_sif_hotspot, dif_tropics_sif_cloud) - 0.10 * min(dif_tropics_sif_hotspot, dif_tropics_sif_cloud),
              max(dif_tropics_sif_hotspot, dif_tropics_sif_cloud) + 0.10 * max(dif_tropics_sif_hotspot, dif_tropics_sif_cloud)))
lines(dif_tropics_sif_cloud, lty = 3)
lines(combo_tropics_sif, lty = 1)
abline(h = 0)
# axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(lab_hot), as.expression(lab_cloud), as.expression(lab_combo)),
       lty = c(2, 3, 1), horiz = TRUE)
mtext(2, text = lab_sif, line = 0.5)
mtext(3, text = lab_tropic, line = 0.5)
box()

# samerica
op <- par(mar = c(0,3,4,0.5))
plot(dif_samerica_sif_hotspot, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 2,
     ylim = c(min(dif_samerica_sif_hotspot, dif_samerica_sif_cloud) - 0.10 * min(dif_samerica_sif_hotspot, dif_samerica_sif_cloud),
              max(dif_samerica_sif_hotspot, dif_samerica_sif_cloud) + 0.10 * max(dif_samerica_sif_hotspot, dif_samerica_sif_cloud)))
lines(dif_samerica_sif_cloud, lty = 3)
lines(combo_samerica_sif, lty = 1)
abline(h = 0)
# axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(lab_hot), as.expression(lab_cloud), as.expression(lab_combo)),
       lty = c(2, 3, 1), horiz = TRUE)
mtext(3, text = lab_sa, line = 0.5)
box()



dev.off()


plot(ts_tropics_sif_cs_all, type = "l")
lines(ts_tropics_sif_cs, lty = 2)

plot(dif_tropics_sif_hotspot, type = "l")
plot(dif_tropics_sif_cloud, type = "l")
plot(combo_tropics, type = "l")
