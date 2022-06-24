library(terra)
library(raster)
library(rgdal)
library(rgeos)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/regressions_seasia.pdf"

#### Load Files ####

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
seasia_ext   <- extent(c(95,155,-11,10))
cover        <- crop(mask_total, seasia_ext)


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

ts_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]

ts_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]

ts_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]

# convert red to absorbance
ts_ref_665_cs_all <- 1-ts_ref_665_cs_all
ts_ref_665_cs     <- 1-ts_ref_665_cs
ts_ref_665_cf     <- 1-ts_ref_665_cf

### Can be used for pvalues
round2 = function(x, n, p) {
  posneg = sign(x)
  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z <- z*posneg
  
  if (p == TRUE) {
    if (z < 0.05 && z > 0.01) {
      z <- "p < 0.05"
    } else if (z < 0.01) {
      z <- "p < 0.01"
    } else {
      z <- paste0("p = ", z)
    }
  }
  return(z)
}

# Regressions
reg_nirv_cs_all     <- lm(ts_sif_cs_all ~ ts_nirv_cs_all)
reg_nirv_cs_all_sum <- summary(reg_nirv_cs_all)
reg_nirv_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_nirv_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_nirv_cs_all_p   <- round2(reg_nirv_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_nirv_rad_cs_all     <- lm(ts_sif_cs_all ~ ts_nirv_rad_cs_all)
reg_nirv_rad_cs_all_sum <- summary(reg_nirv_rad_cs_all)
reg_nirv_rad_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_nirv_rad_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_nirv_rad_cs_all_p   <- round2(reg_nirv_rad_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_ref_665_cs_all     <- lm(ts_sif_cs_all ~ ts_ref_665_cs_all)
reg_ref_665_cs_all_sum <- summary(reg_ref_665_cs_all)
reg_ref_665_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_ref_665_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_ref_665_cs_all_p   <- round2(reg_ref_665_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_ref_781_cs_all     <- lm(ts_sif_cs_all ~ ts_ref_781_cs_all)
reg_ref_781_cs_all_sum <- summary(reg_ref_781_cs_all)
reg_ref_781_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_ref_781_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_ref_781_cs_all_p   <- round2(reg_ref_781_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_nirv_cs     <- lm(ts_sif_cs ~ ts_nirv_cs)
reg_nirv_cs_sum <- summary(reg_nirv_cs)
reg_nirv_cs_r   <- bquote(R^2~" = "~.(round2(reg_nirv_cs_sum$adj.r.squared, 2, FALSE)))
reg_nirv_cs_p   <- round2(reg_nirv_cs_sum$coefficients[2,4], 2, TRUE)

reg_nirv_rad_cs     <- lm(ts_sif_cs ~ ts_nirv_rad_cs)
reg_nirv_rad_cs_sum <- summary(reg_nirv_rad_cs)
reg_nirv_rad_cs_r   <- bquote(R^2~" = "~.(round2(reg_nirv_rad_cs_sum$adj.r.squared, 2, FALSE)))
reg_nirv_rad_cs_p   <- round2(reg_nirv_rad_cs_sum$coefficients[2,4], 2, TRUE)

reg_ref_665_cs     <- lm(ts_sif_cs ~ ts_ref_665_cs)
reg_ref_665_cs_sum <- summary(reg_ref_665_cs)
reg_ref_665_cs_r   <- bquote(R^2~" = "~.(round2(reg_ref_665_cs_sum$adj.r.squared, 2, FALSE)))
reg_ref_665_cs_p   <- round2(reg_ref_665_cs_sum$coefficients[2,4], 2, TRUE)

reg_ref_781_cs     <- lm(ts_sif_cs ~ ts_ref_781_cs)
reg_ref_781_cs_sum <- summary(reg_ref_781_cs)
reg_ref_781_cs_r   <- bquote(R^2~" = "~.(round2(reg_ref_781_cs_sum$adj.r.squared, 2, FALSE)))
reg_ref_781_cs_p   <- round2(reg_ref_781_cs_sum$coefficients[2,4], 2, TRUE)

reg_nirv_cf     <- lm(ts_sif_cf ~ ts_nirv_cf)
reg_nirv_cf_sum <- summary(reg_nirv_cf)
reg_nirv_cf_r   <- bquote(R^2~" = "~.(round2(reg_nirv_cf_sum$adj.r.squared, 2, FALSE)))
reg_nirv_cf_p   <- round2(reg_nirv_cf_sum$coefficients[2,4], 2, TRUE)

reg_nirv_rad_cf     <- lm(ts_sif_cf ~ ts_nirv_rad_cf)
reg_nirv_rad_cf_sum <- summary(reg_nirv_rad_cf)
reg_nirv_rad_cf_r   <- bquote(R^2~" = "~.(round2(reg_nirv_rad_cf_sum$adj.r.squared, 2, FALSE)))
reg_nirv_rad_cf_p   <- round2(reg_nirv_rad_cf_sum$coefficients[2,4], 2, TRUE)

reg_ref_665_cf     <- lm(ts_sif_cf ~ ts_ref_665_cf)
reg_ref_665_cf_sum <- summary(reg_ref_665_cf)
reg_ref_665_cf_r   <- bquote(R^2~" = "~.(round2(reg_ref_665_cf_sum$adj.r.squared, 2, FALSE)))
reg_ref_665_cf_p   <- round2(reg_ref_665_cf_sum$coefficients[2,4], 2, TRUE)

reg_ref_781_cf     <- lm(ts_sif_cf ~ ts_ref_781_cf)
reg_ref_781_cf_sum <- summary(reg_ref_781_cf)
reg_ref_781_cf_r   <- bquote(R^2~" = "~.(round2(reg_ref_781_cf_sum$adj.r.squared, 2, FALSE)))
reg_ref_781_cf_p   <- round2(reg_ref_781_cf_sum$coefficients[2,4], 2, TRUE)

# Labels
lab_title  <- "Southeast Asian Tropical Forest"
lab_sif    <- bquote("SIF (mW/m"^"2"*"/sr/nm)")
lab_nirv   <- bquote("NIRv Reflectance")
lab_nirvr  <- bquote("NIRv Radiance (mW/m"^"2"*"/sr/nm)")
lab_665    <- bquote("Red Absorbance")
lab_781    <- bquote("NIR Reflectance")
lab_cs     <- bquote("Clear Sky (No Hotspot)")
lab_cs_all <- bquote("Clear Sky (w/ Hotspot)")
lab_cf     <- bquote("Cloud Frac < 0.20 (w/ Hotspot)")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(4, 3), oma=c(2.5,2,0,1))

# RED
op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_665_cs, ts_sif_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_665_cs)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_665_cs_r), as.expression(reg_ref_665_cs_p)))
mtext(3, text = lab_cs, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_665_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_665_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_665_cs_all_r), as.expression(reg_ref_665_cs_all_p)))
mtext(3, text = lab_cs_all, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_665_cf, ts_sif_cf, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_665_cf)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_665_cf_r), as.expression(reg_ref_665_cf_p)))
mtext(3, text = lab_cf, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

# NIR
op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_781_cs, ts_sif_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_781_cs)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_781_cs_r), as.expression(reg_ref_781_cs_p)))
mtext(1, text = lab_781, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_781_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_781_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_781_cs_all_r), as.expression(reg_ref_781_cs_all_p)))
mtext(1, text = lab_781, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_ref_781_cf, ts_sif_cf, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_ref_781_cf)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_ref_781_cf_r), as.expression(reg_ref_781_cf_p)))
mtext(1, text = lab_781, line = 1.75)
box()

# NIRv
op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cs, ts_sif_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_cs)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cs_r), as.expression(reg_nirv_cs_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cs_all_r), as.expression(reg_nirv_cs_all_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cf, ts_sif_cf, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_cf)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cf_r), as.expression(reg_nirv_cf_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

# NIRv Rad
op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_rad_cs, ts_sif_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_rad_cs)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_rad_cs_r), as.expression(reg_nirv_rad_cs_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_rad_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_rad_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_rad_cs_all_r), as.expression(reg_nirv_rad_cs_all_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_rad_cf, ts_sif_cf, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA)
abline(reg_nirv_rad_cf)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_rad_cf_r), as.expression(reg_nirv_rad_cf_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

mtext(lab_sif, side = 2, outer = TRUE, line = -0.65, cex = 1.5)
mtext(lab_title, side = 3, outer = TRUE, line = -1.65, cex = 1.5)

dev.off()

