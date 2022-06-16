library(terra)
library(raster)
library(rgdal)
library(rgeos)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/error_africa.pdf"


### All PAs clear sky (w/hotspot), clear sky cold (no hotspot), cold CF20
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

# Create single mask with cover threshold
mask_total <- mask(mask_ebf, mask_veg)
mask_total[mask_total < 80]  <- NA

# Extent
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

africa_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[2]]
africa_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[2]]
africa_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[2]]
africa_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[2]]
africa_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[2]]

africa_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[2]]
africa_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[2]]
africa_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[2]]
africa_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[2]]
africa_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[2]]

africa_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[2]]
africa_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[2]]
africa_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[2]]
africa_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[2]]
africa_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[2]]

## PLOT
cairo_pdf(out_name, width = 6.5, height = 8)

par(mfrow = c(5, 3), oma=c(3,1,0,1))

lab_sif   <- bquote("SIF (mW/m"^"2"*"/sr/nm)")
lab_nirv  <- bquote("NIRv (Reflectance)")
lab_nirvr <- bquote("NIRv Radiance (mW/m"^"2"*"/sr/nm)")
lab_665   <- bquote("Red Reflectance")
lab_781   <- bquote("NIR Reflectance")

# SIF
op <- par(mar = c(0,2,4,0))
hist(africa_sif_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)
mtext(3, text = "Clear Sky", line = 0.5)

hist(africa_sif_cs, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(3, text = "Clear Sky No Hotspot", line = 0.5)
mtext(1, text = lab_sif, line = 2)

hist(africa_sif_cf, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(3, text = "CF <0.20 No Hotspot", line = 0.5)

# nirv
hist(africa_nirv_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(africa_nirv_cs, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_nirv, line = 2)
box()

hist(africa_nirv_cf, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# nirv rad
hist(africa_nirv_rad_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(africa_nirv_rad_cs, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_nirvr, line = 2)
box()

hist(africa_nirv_rad_cf, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# red ref
hist(africa_ref_665_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(africa_ref_665_cs, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_665, line = 2)
box()

hist(africa_ref_665_cf, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# red ref
hist(africa_ref_781_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(africa_ref_781_cs, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_781, line = 2)
box()

hist(africa_ref_781_cf, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

mtext(3, text = "Monthly Standard Error of the Mean for African Tropical Forest", outer = TRUE, line = -1.5)

dev.off()