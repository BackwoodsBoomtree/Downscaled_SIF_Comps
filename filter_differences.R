library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/filter_differences.pdf"

### All PAs, cold, cold CF
cs_all_2019 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
cs_all_2020 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
cs_all_2021 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

cs_2019     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.nc"
cs_2020     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.nc"
cs_2021     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"

cf_2019     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.cold.nc"
cf_2020     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.cold.nc"
cf_2021     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.cold.nc"

cf_all_2019 <-"G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.nc"
cf_all_2020 <-"G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
cf_all_2021 <-"G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.nc"

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

ts_tropics_sif_cf_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_all_2019, cf_all_2020, cf_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_cf_all     <- get_ts("NIRv", "n", "NIRv_std", c(cf_all_2019, cf_all_2020, cf_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_nirv_rad_cf_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_all_2019, cf_all_2020, cf_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_665_cf_all  <- get_ts("REF_665", "n", "REF_665_std", c(cf_all_2019, cf_all_2020, cf_all_2021), tropics_ext, mask_total)[[1]]
ts_tropics_ref_781_cf_all  <- get_ts("REF_781", "n", "REF_781_std", c(cf_all_2019, cf_all_2020, cf_all_2021), tropics_ext, mask_total)[[1]]

# # convert red to absorbance
# ts_tropics_ref_665_cs_all <- 1-ts_tropics_ref_665_cs_all
# ts_tropics_ref_665_cs     <- 1-ts_tropics_ref_665_cs
# ts_tropics_ref_665_cf     <- 1-ts_tropics_ref_665_cf
# ts_tropics_ref_665_cf_all <- 1-ts_tropics_ref_665_cf_all

## Here we calculate:
## 1. The hotspot effect on clear sky 
## 2. and CF <0.20 data.
## 3. The cloud effect (<0.20) on data with 
## 4. and without hotspot.
## 5. Combined effect of inclusion of cloud and hotspot
## (with hotspot = _all)

# Differences
# Tropics
dif_tropics_sif_hotspot_cs   <- (ts_tropics_sif_cs_all - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100
dif_tropics_sif_hotspot_cf   <- (ts_tropics_sif_cf_all - ts_tropics_sif_cf) / abs(ts_tropics_sif_cf) * 100
dif_tropics_sif_cloud        <- (ts_tropics_sif_cf - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100
dif_tropics_sif_cloud_all    <- (ts_tropics_sif_cf_all - ts_tropics_sif_cs_all) / abs(ts_tropics_sif_cs_all) * 100
dif_tropics_sif              <- (ts_tropics_sif_cf_all - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100

dif_tropics_ref_665_hotspot_cs   <- ts_tropics_ref_665_cs_all - ts_tropics_ref_665_cs
dif_tropics_ref_665_hotspot_cf   <- ts_tropics_ref_665_cf_all - ts_tropics_ref_665_cf
dif_tropics_ref_665_cloud        <- ts_tropics_ref_665_cf - ts_tropics_ref_665_cs
dif_tropics_ref_665_cloud_all    <- ts_tropics_ref_665_cf_all - ts_tropics_ref_665_cs_all
dif_tropics_ref_665              <- ts_tropics_ref_665_cf_all - ts_tropics_ref_665_cs

dif_tropics_ref_781_hotspot_cs   <- ts_tropics_ref_781_cs_all - ts_tropics_ref_781_cs
dif_tropics_ref_781_hotspot_cf   <- ts_tropics_ref_781_cf_all - ts_tropics_ref_781_cf
dif_tropics_ref_781_cloud        <- ts_tropics_ref_781_cf - ts_tropics_ref_781_cs
dif_tropics_ref_781_cloud_all    <- ts_tropics_ref_781_cf_all - ts_tropics_ref_781_cs_all
dif_tropics_ref_781              <- ts_tropics_ref_781_cf_all - ts_tropics_ref_781_cs

dif_tropics_nirv_hotspot_cs   <- (ts_tropics_nirv_cs_all - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100
dif_tropics_nirv_hotspot_cf   <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cf) / abs(ts_tropics_nirv_cf) * 100
dif_tropics_nirv_cloud        <- (ts_tropics_nirv_cf - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100
dif_tropics_nirv_cloud_all    <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cs_all) / abs(ts_tropics_nirv_cs_all) * 100
dif_tropics_nirv              <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100

dif_tropics_nirv_rad_hotspot_cs   <- (ts_tropics_nirv_rad_cs_all - ts_tropics_nirv_rad_cs) / abs(ts_tropics_nirv_rad_cs) * 100
dif_tropics_nirv_rad_hotspot_cf   <- (ts_tropics_nirv_rad_cf_all - ts_tropics_nirv_rad_cf) / abs(ts_tropics_nirv_rad_cf) * 100
dif_tropics_nirv_rad_cloud        <- (ts_tropics_nirv_rad_cf - ts_tropics_nirv_rad_cs) / abs(ts_tropics_nirv_rad_cs) * 100
dif_tropics_nirv_rad_cloud_all    <- (ts_tropics_nirv_rad_cf_all - ts_tropics_nirv_rad_cs_all) / abs(ts_tropics_nirv_rad_cs_all) * 100
dif_tropics_nirv_rad              <- (ts_tropics_nirv_rad_cf_all - ts_tropics_nirv_rad_cs) / abs(ts_tropics_nirv_rad_cs) * 100

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

ts_samerica_sif_cf_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_all_2019, cf_all_2020, cf_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_cf_all     <- get_ts("NIRv", "n", "NIRv_std", c(cf_all_2019, cf_all_2020, cf_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_nirv_rad_cf_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_all_2019, cf_all_2020, cf_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_665_cf_all  <- get_ts("REF_665", "n", "REF_665_std", c(cf_all_2019, cf_all_2020, cf_all_2021), samerica_ext, mask_total)[[1]]
ts_samerica_ref_781_cf_all  <- get_ts("REF_781", "n", "REF_781_std", c(cf_all_2019, cf_all_2020, cf_all_2021), samerica_ext, mask_total)[[1]]

# # convert red to absorbance
# ts_samerica_ref_665_cs_all <- 1-ts_samerica_ref_665_cs_all
# ts_samerica_ref_665_cs     <- 1-ts_samerica_ref_665_cs
# ts_samerica_ref_665_cf     <- 1-ts_samerica_ref_665_cf
# ts_samerica_ref_665_cf_all <- 1-ts_samerica_ref_665_cf_all

# Differences
# samerica
dif_samerica_sif_hotspot_cs   <- (ts_samerica_sif_cs_all - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100
dif_samerica_sif_hotspot_cf   <- (ts_samerica_sif_cf_all - ts_samerica_sif_cf) / abs(ts_samerica_sif_cf) * 100
dif_samerica_sif_cloud        <- (ts_samerica_sif_cf - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100
dif_samerica_sif_cloud_all    <- (ts_samerica_sif_cf_all - ts_samerica_sif_cs_all) / abs(ts_samerica_sif_cs_all) * 100
dif_samerica_sif              <- (ts_samerica_sif_cf_all - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100

dif_samerica_ref_665_hotspot_cs   <- ts_samerica_ref_665_cs_all - ts_samerica_ref_665_cs
dif_samerica_ref_665_hotspot_cf   <- ts_samerica_ref_665_cf_all - ts_samerica_ref_665_cf
dif_samerica_ref_665_cloud        <- ts_samerica_ref_665_cf - ts_samerica_ref_665_cs
dif_samerica_ref_665_cloud_all    <- ts_samerica_ref_665_cf_all - ts_samerica_ref_665_cs_all
dif_samerica_ref_665              <- ts_samerica_ref_665_cf_all - ts_samerica_ref_665_cs

dif_samerica_ref_781_hotspot_cs   <- ts_samerica_ref_781_cs_all - ts_samerica_ref_781_cs
dif_samerica_ref_781_hotspot_cf   <- ts_samerica_ref_781_cf_all - ts_samerica_ref_781_cf
dif_samerica_ref_781_cloud        <- ts_samerica_ref_781_cf - ts_samerica_ref_781_cs
dif_samerica_ref_781_cloud_all    <- ts_samerica_ref_781_cf_all - ts_samerica_ref_781_cs_all
dif_samerica_ref_781              <- ts_samerica_ref_781_cf_all - ts_samerica_ref_781_cs

dif_samerica_nirv_hotspot_cs   <- (ts_samerica_nirv_cs_all - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100
dif_samerica_nirv_hotspot_cf   <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cf) / abs(ts_samerica_nirv_cf) * 100
dif_samerica_nirv_cloud        <- (ts_samerica_nirv_cf - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100
dif_samerica_nirv_cloud_all    <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cs_all) / abs(ts_samerica_nirv_cs_all) * 100
dif_samerica_nirv              <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100

dif_samerica_nirv_rad_hotspot_cs   <- (ts_samerica_nirv_rad_cs_all - ts_samerica_nirv_rad_cs) / abs(ts_samerica_nirv_rad_cs) * 100
dif_samerica_nirv_rad_hotspot_cf   <- (ts_samerica_nirv_rad_cf_all - ts_samerica_nirv_rad_cf) / abs(ts_samerica_nirv_rad_cf) * 100
dif_samerica_nirv_rad_cloud        <- (ts_samerica_nirv_rad_cf - ts_samerica_nirv_rad_cs) / abs(ts_samerica_nirv_rad_cs) * 100
dif_samerica_nirv_rad_cloud_all    <- (ts_samerica_nirv_rad_cf_all - ts_samerica_nirv_rad_cs_all) / abs(ts_samerica_nirv_rad_cs_all) * 100
dif_samerica_nirv_rad              <- (ts_samerica_nirv_rad_cf_all - ts_samerica_nirv_rad_cs) / abs(ts_samerica_nirv_rad_cs) * 100


# Get africa data
ts_africa_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), africa_ext, mask_total)[[1]]

ts_africa_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), africa_ext, mask_total)[[1]]

ts_africa_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), africa_ext, mask_total)[[1]]

ts_africa_sif_cf_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_all_2019, cf_all_2020, cf_all_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_cf_all     <- get_ts("NIRv", "n", "NIRv_std", c(cf_all_2019, cf_all_2020, cf_all_2021), africa_ext, mask_total)[[1]]
ts_africa_nirv_rad_cf_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_all_2019, cf_all_2020, cf_all_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_665_cf_all  <- get_ts("REF_665", "n", "REF_665_std", c(cf_all_2019, cf_all_2020, cf_all_2021), africa_ext, mask_total)[[1]]
ts_africa_ref_781_cf_all  <- get_ts("REF_781", "n", "REF_781_std", c(cf_all_2019, cf_all_2020, cf_all_2021), africa_ext, mask_total)[[1]]

# # convert red to absorbance
# ts_africa_ref_665_cs_all <- 1-ts_africa_ref_665_cs_all
# ts_africa_ref_665_cs     <- 1-ts_africa_ref_665_cs
# ts_africa_ref_665_cf     <- 1-ts_africa_ref_665_cf
# ts_africa_ref_665_cf_all <- 1-ts_africa_ref_665_cf_all

# Differences
# africa
dif_africa_sif_hotspot_cs   <- (ts_africa_sif_cs_all - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100
dif_africa_sif_hotspot_cf   <- (ts_africa_sif_cf_all - ts_africa_sif_cf) / abs(ts_africa_sif_cf) * 100
dif_africa_sif_cloud        <- (ts_africa_sif_cf - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100
dif_africa_sif_cloud_all    <- (ts_africa_sif_cf_all - ts_africa_sif_cs_all) / abs(ts_africa_sif_cs_all) * 100
dif_africa_sif              <- (ts_africa_sif_cf_all - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100

dif_africa_ref_665_hotspot_cs   <- ts_africa_ref_665_cs_all - ts_africa_ref_665_cs
dif_africa_ref_665_hotspot_cf   <- ts_africa_ref_665_cf_all - ts_africa_ref_665_cf
dif_africa_ref_665_cloud        <- ts_africa_ref_665_cf - ts_africa_ref_665_cs
dif_africa_ref_665_cloud_all    <- ts_africa_ref_665_cf_all - ts_africa_ref_665_cs_all
dif_africa_ref_665              <- ts_africa_ref_665_cf_all - ts_africa_ref_665_cs

dif_africa_ref_781_hotspot_cs   <- ts_africa_ref_781_cs_all - ts_africa_ref_781_cs
dif_africa_ref_781_hotspot_cf   <- ts_africa_ref_781_cf_all - ts_africa_ref_781_cf
dif_africa_ref_781_cloud        <- ts_africa_ref_781_cf - ts_africa_ref_781_cs
dif_africa_ref_781_cloud_all    <- ts_africa_ref_781_cf_all - ts_africa_ref_781_cs_all
dif_africa_ref_781              <- ts_africa_ref_781_cf_all - ts_africa_ref_781_cs

dif_africa_nirv_hotspot_cs   <- (ts_africa_nirv_cs_all - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100
dif_africa_nirv_hotspot_cf   <- (ts_africa_nirv_cf_all - ts_africa_nirv_cf) / abs(ts_africa_nirv_cf) * 100
dif_africa_nirv_cloud        <- (ts_africa_nirv_cf - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100
dif_africa_nirv_cloud_all    <- (ts_africa_nirv_cf_all - ts_africa_nirv_cs_all) / abs(ts_africa_nirv_cs_all) * 100
dif_africa_nirv              <- (ts_africa_nirv_cf_all - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100

dif_africa_nirv_rad_hotspot_cs   <- (ts_africa_nirv_rad_cs_all - ts_africa_nirv_rad_cs) / abs(ts_africa_nirv_rad_cs) * 100
dif_africa_nirv_rad_hotspot_cf   <- (ts_africa_nirv_rad_cf_all - ts_africa_nirv_rad_cf) / abs(ts_africa_nirv_rad_cf) * 100
dif_africa_nirv_rad_cloud        <- (ts_africa_nirv_rad_cf - ts_africa_nirv_rad_cs) / abs(ts_africa_nirv_rad_cs) * 100
dif_africa_nirv_rad_cloud_all    <- (ts_africa_nirv_rad_cf_all - ts_africa_nirv_rad_cs_all) / abs(ts_africa_nirv_rad_cs_all) * 100
dif_africa_nirv_rad              <- (ts_africa_nirv_rad_cf_all - ts_africa_nirv_rad_cs) / abs(ts_africa_nirv_rad_cs) * 100


# Get seasia data
ts_seasia_sif_cs_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_cs_all     <- get_ts("NIRv", "n", "NIRv_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_rad_cs_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_665_cs_all  <- get_ts("REF_665", "n", "REF_665_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_781_cs_all  <- get_ts("REF_781", "n", "REF_781_std", c(cs_all_2019, cs_all_2020, cs_all_2021), seasia_ext, mask_total)[[1]]

ts_seasia_sif_cs      <- get_ts("SIF_743", "n", "SIF_743_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_cs     <- get_ts("NIRv", "n", "NIRv_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_rad_cs <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_665_cs  <- get_ts("REF_665", "n", "REF_665_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_781_cs  <- get_ts("REF_781", "n", "REF_781_std", c(cs_2019, cs_2020, cs_2021), seasia_ext, mask_total)[[1]]

ts_seasia_sif_cf      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_cf     <- get_ts("NIRv", "n", "NIRv_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_rad_cf <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_665_cf  <- get_ts("REF_665", "n", "REF_665_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_781_cf  <- get_ts("REF_781", "n", "REF_781_std", c(cf_2019, cf_2020, cf_2021), seasia_ext, mask_total)[[1]]

ts_seasia_sif_cf_all      <- get_ts("SIF_743", "n", "SIF_743_std", c(cf_all_2019, cf_all_2020, cf_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_cf_all     <- get_ts("NIRv", "n", "NIRv_std", c(cf_all_2019, cf_all_2020, cf_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_nirv_rad_cf_all <- get_ts("NIRv_Rad", "n", "NIRv_Rad_std", c(cf_all_2019, cf_all_2020, cf_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_665_cf_all  <- get_ts("REF_665", "n", "REF_665_std", c(cf_all_2019, cf_all_2020, cf_all_2021), seasia_ext, mask_total)[[1]]
ts_seasia_ref_781_cf_all  <- get_ts("REF_781", "n", "REF_781_std", c(cf_all_2019, cf_all_2020, cf_all_2021), seasia_ext, mask_total)[[1]]

# # convert red to absorbance
# ts_seasia_ref_665_cs_all <- 1-ts_seasia_ref_665_cs_all
# ts_seasia_ref_665_cs     <- 1-ts_seasia_ref_665_cs
# ts_seasia_ref_665_cf     <- 1-ts_seasia_ref_665_cf
# ts_seasia_ref_665_cf_all <- 1-ts_seasia_ref_665_cf_all

# Differences
# seasia
dif_seasia_sif_hotspot_cs   <- (ts_seasia_sif_cs_all - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100
dif_seasia_sif_hotspot_cf   <- (ts_seasia_sif_cf_all - ts_seasia_sif_cf) / abs(ts_seasia_sif_cf) * 100
dif_seasia_sif_cloud        <- (ts_seasia_sif_cf - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100
dif_seasia_sif_cloud_all    <- (ts_seasia_sif_cf_all - ts_seasia_sif_cs_all) / abs(ts_seasia_sif_cs_all) * 100
dif_seasia_sif              <- (ts_seasia_sif_cf_all - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100

dif_seasia_ref_665_hotspot_cs   <- ts_seasia_ref_665_cs_all - ts_seasia_ref_665_cs
dif_seasia_ref_665_hotspot_cf   <- ts_seasia_ref_665_cf_all - ts_seasia_ref_665_cf
dif_seasia_ref_665_cloud        <- ts_seasia_ref_665_cf - ts_seasia_ref_665_cs
dif_seasia_ref_665_cloud_all    <- ts_seasia_ref_665_cf_all - ts_seasia_ref_665_cs_all
dif_seasia_ref_665              <- ts_seasia_ref_665_cf_all - ts_seasia_ref_665_cs

dif_seasia_ref_781_hotspot_cs   <- ts_seasia_ref_781_cs_all - ts_seasia_ref_781_cs
dif_seasia_ref_781_hotspot_cf   <- ts_seasia_ref_781_cf_all - ts_seasia_ref_781_cf
dif_seasia_ref_781_cloud        <- ts_seasia_ref_781_cf - ts_seasia_ref_781_cs
dif_seasia_ref_781_cloud_all    <- ts_seasia_ref_781_cf_all - ts_seasia_ref_781_cs_all
dif_seasia_ref_781              <- ts_seasia_ref_781_cf_all - ts_seasia_ref_781_cs

dif_seasia_nirv_hotspot_cs   <- (ts_seasia_nirv_cs_all - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100
dif_seasia_nirv_hotspot_cf   <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cf) / abs(ts_seasia_nirv_cf) * 100
dif_seasia_nirv_cloud        <- (ts_seasia_nirv_cf - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100
dif_seasia_nirv_cloud_all    <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cs_all) / abs(ts_seasia_nirv_cs_all) * 100
dif_seasia_nirv              <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100

dif_seasia_nirv_rad_hotspot_cs   <- (ts_seasia_nirv_rad_cs_all - ts_seasia_nirv_rad_cs) / abs(ts_seasia_nirv_rad_cs) * 100
dif_seasia_nirv_rad_hotspot_cf   <- (ts_seasia_nirv_rad_cf_all - ts_seasia_nirv_rad_cf) / abs(ts_seasia_nirv_rad_cf) * 100
dif_seasia_nirv_rad_cloud        <- (ts_seasia_nirv_rad_cf - ts_seasia_nirv_rad_cs) / abs(ts_seasia_nirv_rad_cs) * 100
dif_seasia_nirv_rad_cloud_all    <- (ts_seasia_nirv_rad_cf_all - ts_seasia_nirv_rad_cs_all) / abs(ts_seasia_nirv_rad_cs_all) * 100
dif_seasia_nirv_rad              <- (ts_seasia_nirv_rad_cf_all - ts_seasia_nirv_rad_cs) / abs(ts_seasia_nirv_rad_cs) * 100


# Labels
x_lab          <- c(1, 13, 25, 36)
lab_tropic     <- "Tropical Forest (TF)"
lab_sa         <- "S American TF"
lab_africa     <- "African TF"
lab_seasia     <- "SE Asian TF"
lab_sif        <- bquote("SIF (%)")
lab_nirv       <- bquote("NIRv Ref. (%)")
lab_nirvr      <- bquote("NIRv Rad. (%)")
lab_665        <- bquote("Red Reflectance")
lab_781        <- bquote("NIR Reflectance")
lab_hot_cs     <- bquote("Hotspot effect clearsky")
lab_hot_cf     <- bquote("Hotspot effect CF <0.20")
lab_cloud_hot  <- bquote("Cloud effect w/ hotspot")
lab_cloud_cold <- bquote("Cloud effect no hotspot")
lab_combo      <- bquote("Combined effect")

#### LEGEND PLOT ####
cairo_pdf("G:/SIF_comps/figs/filter_differences_legend.pdf", width = 10, height = 3)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

legend("top", bty = "n", legend = c(lab_hot_cf, lab_hot_cs, lab_cloud_hot, lab_cloud_cold, lab_combo),
       lty = c(2, 1, 1, 2, 1),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5),
       col = c("red", "red", "blue", "blue", "black"),
       ncol = 3)

dev.off()

#### PLOT ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(5, 4), oma=c(2.5,3,6,0.5))

# SIF
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_tropics_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_sif_cloud, col = "blue", lty = 2)
lines(dif_tropics_sif_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_sif, line = 2)
mtext(3, text = lab_tropic, line = 0.5)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_samerica_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_sif_cloud, col = "blue", lty = 2)
lines(dif_samerica_sif_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_sa, line = 0.5)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_africa_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_sif_cloud, col = "blue", lty = 2)
lines(dif_africa_sif_cloud_all, col = "blue", lty = 1)
lines(dif_africa_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_africa, line = 0.5, lty = 1)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_seasia_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_sif_cloud, col = "blue", lty = 2)
lines(dif_seasia_sif_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_seasia, line = 0.5)
box()


# ref_665
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_ref_665_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_tropics_ref_665_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_ref_665_cloud, col = "blue", lty = 2)
lines(dif_tropics_ref_665_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_ref_665)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_665, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_ref_665_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_samerica_ref_665_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_ref_665_cloud, col = "blue", lty = 2)
lines(dif_samerica_ref_665_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_ref_665)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_ref_665_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_africa_ref_665_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_ref_665_cloud, col = "blue", lty = 2)
lines(dif_africa_ref_665_cloud_all, col = "blue", lty = 1)
lines(dif_africa_ref_665)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_ref_665_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_seasia_ref_665_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_ref_665_cloud, col = "blue", lty = 2)
lines(dif_seasia_ref_665_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_ref_665)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()


# ref_781
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_ref_781_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_tropics_ref_781_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_ref_781_cloud, col = "blue", lty = 2)
lines(dif_tropics_ref_781_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_ref_781)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_781, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_ref_781_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_samerica_ref_781_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_ref_781_cloud, col = "blue", lty = 2)
lines(dif_samerica_ref_781_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_ref_781)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_ref_781_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_africa_ref_781_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_ref_781_cloud, col = "blue", lty = 2)
lines(dif_africa_ref_781_cloud_all, col = "blue", lty = 1)
lines(dif_africa_ref_781)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_ref_781_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_seasia_ref_781_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_ref_781_cloud, col = "blue", lty = 2)
lines(dif_seasia_ref_781_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_ref_781)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
box()


# nirv
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_tropics_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_nirv_cloud, col = "blue", lty = 2)
lines(dif_tropics_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_nirv, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_samerica_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_nirv_cloud, col = "blue", lty = 2)
lines(dif_samerica_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_africa_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_nirv_cloud, col = "blue", lty = 2)
lines(dif_africa_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_africa_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_seasia_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_nirv_cloud, col = "blue", lty = 2)
lines(dif_seasia_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()


# nirv_rad
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_nirv_rad_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_tropics_nirv_rad_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_nirv_rad_cloud, col = "blue", lty = 2)
lines(dif_tropics_nirv_rad_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_nirv_rad)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_nirvr, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_nirv_rad_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_samerica_nirv_rad_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_nirv_rad_cloud, col = "blue", lty = 2)
lines(dif_samerica_nirv_rad_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_nirv_rad)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_nirv_rad_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_africa_nirv_rad_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_nirv_rad_cloud, col = "blue", lty = 2)
lines(dif_africa_nirv_rad_cloud_all, col = "blue", lty = 1)
lines(dif_africa_nirv_rad)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_nirv_rad_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_seasia_nirv_rad_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_nirv_rad_cloud, col = "blue", lty = 2)
lines(dif_seasia_nirv_rad_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_nirv_rad)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

mtext(1, text = "Month (2019-2021)", line = 1.5, outer = TRUE)


dev.off()

