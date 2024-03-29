library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/relative_sif_comp_yield_precip_black.pdf"

#### Load Files ####

### All PAs, cold, cold CF
# SIF data
cs_all_2019 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
cs_all_2020 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"
cs_all_2021 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.nc"

cs_2019     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.cold.nc"
cs_2020     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.cold.nc"
cs_2021     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.clearsky.cold.nc"

cf_2019     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.cold.nc"
cf_2020     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.cold.nc"
cf_2021     <- "G:/TROPOMI/esa/gridded/1deg/monthly/2021/TROPOMI.ESA.SIF.2021.global.monthly.1deg.CF20.cold.nc"

# Clima yield
yield <- rast("G:/ChloFluo/input/yield/1deg/yield.2019.8-day.1deg.nc", subds = "sif_yield")

# Precip
era   <- "G:/ERA5/era5.precip.temp.par.2019-2021.monthly.nc"

#### Min-Max normalization function
min_max_norm <- function(x) {
        (x - min(x)) / (max(x) - min(x))
}

#### Convert 8-day data to monthly
toMonth <- function(raster, year) {
        
        # number of days in that year (leap year or not?)
        ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)
        
        # how many layers?
        n <- ceiling(ndays/8) 
        # day of year for each layer
        nn <- rep(1:n, each=8)[1:ndays] 
        
        # day of year for each month
        m <- as.integer(format(as.Date(1:ndays, origin=paste0(year-1, "-12-31")), "%m"))
        
        x <- cbind(layer=nn, month=m)
        weights <- table(x[,1], x[,2])
        
        s <- list()
        for (i in 1:12) {
                w <- weights[,i]
                x <- raster[[which(w > 0)]]
                ww <- w[w > 0] / 8
                s[[i]] <- weighted.mean(x, ww)
        }
        
        s <- rast(s)
        names(s) <- month.abb
        
        return(s)
        
}

# Conver yield to monthly data
yield <- toMonth(yield, 2019)

# Veg masks (max NDVI > 0.40)
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
mask_ebf                 <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_ebf[mask_ebf < 80]  <- NA
mask_ebf[mask_ebf >= 80] <- 1

# Extents
tropical_ext <- ext(c(-120, 155, -20, 13))
samerica_ext <- ext(c(-82,-34,-20,13))
africa_ext   <- ext(c(7,45,-5,5))
seasia_ext   <- ext(c(95,155,-11,10))

# Crop masks
tropical_ebf <- crop(mask_ebf, tropical_ext)
samerica_ebf <- crop(mask_ebf, samerica_ext)
africa_ebf   <- crop(mask_ebf, africa_ext)
seasia_ebf   <- crop(mask_ebf, seasia_ext)

tropical_ebf <- as.polygons(tropical_ebf, dissolve = TRUE)
samerica_ebf <- as.polygons(samerica_ebf, dissolve = TRUE)
africa_ebf   <- as.polygons(africa_ebf, dissolve = TRUE)
seasia_ebf   <- as.polygons(seasia_ebf, dissolve = TRUE)


#### Get the data and crop ####
tropical_nirv_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv"), rast(cf_2020, subds = "NIRv"), rast(cf_2021, subds = "NIRv")), tropical_ebf), tropical_ext)
tropical_nirvr_cf    <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad"), rast(cf_2020, subds = "NIRv_Rad"), rast(cf_2021, subds = "NIRv_Rad")), tropical_ebf), tropical_ext)
tropical_sif_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743"), rast(cf_2020, subds = "SIF_743"), rast(cf_2021, subds = "SIF_743")), tropical_ebf), tropical_ext)
tropical_TOA_cf      <- crop(mask(c(rast(cf_2019, subds = "Mean_TOA_RAD_743"), rast(cf_2020, subds = "Mean_TOA_RAD_743"), rast(cf_2021, subds = "Mean_TOA_RAD_743")), tropical_ebf), tropical_ext)

tropical_nirv_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv"), rast(cs_2020, subds = "NIRv"), rast(cs_2021, subds = "NIRv")), tropical_ebf), tropical_ext)
tropical_nirvr_cs    <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad"), rast(cs_2020, subds = "NIRv_Rad"), rast(cs_2021, subds = "NIRv_Rad")), tropical_ebf), tropical_ext)
tropical_sif_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743"), rast(cs_2020, subds = "SIF_743"), rast(cs_2021, subds = "SIF_743")), tropical_ebf), tropical_ext)
tropical_TOA_cs      <- crop(mask(c(rast(cs_2019, subds = "Mean_TOA_RAD_743"), rast(cs_2020, subds = "Mean_TOA_RAD_743"), rast(cs_2021, subds = "Mean_TOA_RAD_743")), tropical_ebf), tropical_ext)

tropical_nirv_cs_all     <- crop(mask(c(rast(cs_all_2019, subds = "NIRv"), rast(cs_all_2020, subds = "NIRv"), rast(cs_all_2021, subds = "NIRv")), tropical_ebf), tropical_ext)
tropical_nirvr_cs_all    <- crop(mask(c(rast(cs_all_2019, subds = "NIRv_Rad"), rast(cs_all_2020, subds = "NIRv_Rad"), rast(cs_all_2021, subds = "NIRv_Rad")), tropical_ebf), tropical_ext)
tropical_sif_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "SIF_743"), rast(cs_all_2020, subds = "SIF_743"), rast(cs_all_2021, subds = "SIF_743")), tropical_ebf), tropical_ext)
tropical_TOA_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "Mean_TOA_RAD_743"), rast(cs_all_2020, subds = "Mean_TOA_RAD_743"), rast(cs_all_2021, subds = "Mean_TOA_RAD_743")), tropical_ebf), tropical_ext)

samerica_nirv_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv"), rast(cf_2020, subds = "NIRv"), rast(cf_2021, subds = "NIRv")), samerica_ebf), samerica_ext)
samerica_nirvr_cf    <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad"), rast(cf_2020, subds = "NIRv_Rad"), rast(cf_2021, subds = "NIRv_Rad")), samerica_ebf), samerica_ext)
samerica_sif_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743"), rast(cf_2020, subds = "SIF_743"), rast(cf_2021, subds = "SIF_743")), samerica_ebf), samerica_ext)
samerica_TOA_cf      <- crop(mask(c(rast(cf_2019, subds = "Mean_TOA_RAD_743"), rast(cf_2020, subds = "Mean_TOA_RAD_743"), rast(cf_2021, subds = "Mean_TOA_RAD_743")), samerica_ebf), samerica_ext)

samerica_nirv_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv"), rast(cs_2020, subds = "NIRv"), rast(cs_2021, subds = "NIRv")), samerica_ebf), samerica_ext)
samerica_nirvr_cs    <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad"), rast(cs_2020, subds = "NIRv_Rad"), rast(cs_2021, subds = "NIRv_Rad")), samerica_ebf), samerica_ext)
samerica_sif_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743"), rast(cs_2020, subds = "SIF_743"), rast(cs_2021, subds = "SIF_743")), samerica_ebf), samerica_ext)
samerica_TOA_cs      <- crop(mask(c(rast(cs_2019, subds = "Mean_TOA_RAD_743"), rast(cs_2020, subds = "Mean_TOA_RAD_743"), rast(cs_2021, subds = "Mean_TOA_RAD_743")), samerica_ebf), samerica_ext)

samerica_nirv_cs_all     <- crop(mask(c(rast(cs_all_2019, subds = "NIRv"), rast(cs_all_2020, subds = "NIRv"), rast(cs_all_2021, subds = "NIRv")), samerica_ebf), samerica_ext)
samerica_nirvr_cs_all    <- crop(mask(c(rast(cs_all_2019, subds = "NIRv_Rad"), rast(cs_all_2020, subds = "NIRv_Rad"), rast(cs_all_2021, subds = "NIRv_Rad")), samerica_ebf), samerica_ext)
samerica_sif_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "SIF_743"), rast(cs_all_2020, subds = "SIF_743"), rast(cs_all_2021, subds = "SIF_743")), samerica_ebf), samerica_ext)
samerica_TOA_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "Mean_TOA_RAD_743"), rast(cs_all_2020, subds = "Mean_TOA_RAD_743"), rast(cs_all_2021, subds = "Mean_TOA_RAD_743")), samerica_ebf), samerica_ext)

africa_nirv_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv"), rast(cf_2020, subds = "NIRv"), rast(cf_2021, subds = "NIRv")), africa_ebf), africa_ext)
africa_nirvr_cf    <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad"), rast(cf_2020, subds = "NIRv_Rad"), rast(cf_2021, subds = "NIRv_Rad")), africa_ebf), africa_ext)
africa_sif_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743"), rast(cf_2020, subds = "SIF_743"), rast(cf_2021, subds = "SIF_743")), africa_ebf), africa_ext)
africa_TOA_cf      <- crop(mask(c(rast(cf_2019, subds = "Mean_TOA_RAD_743"), rast(cf_2020, subds = "Mean_TOA_RAD_743"), rast(cf_2021, subds = "Mean_TOA_RAD_743")), africa_ebf), africa_ext)

africa_nirv_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv"), rast(cs_2020, subds = "NIRv"), rast(cs_2021, subds = "NIRv")), africa_ebf), africa_ext)
africa_nirvr_cs    <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad"), rast(cs_2020, subds = "NIRv_Rad"), rast(cs_2021, subds = "NIRv_Rad")), africa_ebf), africa_ext)
africa_sif_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743"), rast(cs_2020, subds = "SIF_743"), rast(cs_2021, subds = "SIF_743")), africa_ebf), africa_ext)
africa_TOA_cs      <- crop(mask(c(rast(cs_2019, subds = "Mean_TOA_RAD_743"), rast(cs_2020, subds = "Mean_TOA_RAD_743"), rast(cs_2021, subds = "Mean_TOA_RAD_743")), africa_ebf), africa_ext)

africa_nirv_cs_all     <- crop(mask(c(rast(cs_all_2019, subds = "NIRv"), rast(cs_all_2020, subds = "NIRv"), rast(cs_all_2021, subds = "NIRv")), africa_ebf), africa_ext)
africa_nirvr_cs_all    <- crop(mask(c(rast(cs_all_2019, subds = "NIRv_Rad"), rast(cs_all_2020, subds = "NIRv_Rad"), rast(cs_all_2021, subds = "NIRv_Rad")), africa_ebf), africa_ext)
africa_sif_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "SIF_743"), rast(cs_all_2020, subds = "SIF_743"), rast(cs_all_2021, subds = "SIF_743")), africa_ebf), africa_ext)
africa_TOA_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "Mean_TOA_RAD_743"), rast(cs_all_2020, subds = "Mean_TOA_RAD_743"), rast(cs_all_2021, subds = "Mean_TOA_RAD_743")), africa_ebf), africa_ext)

seasia_nirv_cf     <- crop(mask(c(rast(cf_2019, subds = "NIRv"), rast(cf_2020, subds = "NIRv"), rast(cf_2021, subds = "NIRv")), seasia_ebf), seasia_ext)
seasia_nirvr_cf    <- crop(mask(c(rast(cf_2019, subds = "NIRv_Rad"), rast(cf_2020, subds = "NIRv_Rad"), rast(cf_2021, subds = "NIRv_Rad")), seasia_ebf), seasia_ext)
seasia_sif_cf      <- crop(mask(c(rast(cf_2019, subds = "SIF_743"), rast(cf_2020, subds = "SIF_743"), rast(cf_2021, subds = "SIF_743")), seasia_ebf), seasia_ext)
seasia_TOA_cf      <- crop(mask(c(rast(cf_2019, subds = "Mean_TOA_RAD_743"), rast(cf_2020, subds = "Mean_TOA_RAD_743"), rast(cf_2021, subds = "Mean_TOA_RAD_743")), seasia_ebf), seasia_ext)

seasia_nirv_cs     <- crop(mask(c(rast(cs_2019, subds = "NIRv"), rast(cs_2020, subds = "NIRv"), rast(cs_2021, subds = "NIRv")), seasia_ebf), seasia_ext)
seasia_nirvr_cs    <- crop(mask(c(rast(cs_2019, subds = "NIRv_Rad"), rast(cs_2020, subds = "NIRv_Rad"), rast(cs_2021, subds = "NIRv_Rad")), seasia_ebf), seasia_ext)
seasia_sif_cs      <- crop(mask(c(rast(cs_2019, subds = "SIF_743"), rast(cs_2020, subds = "SIF_743"), rast(cs_2021, subds = "SIF_743")), seasia_ebf), seasia_ext)
seasia_TOA_cs      <- crop(mask(c(rast(cs_2019, subds = "Mean_TOA_RAD_743"), rast(cs_2020, subds = "Mean_TOA_RAD_743"), rast(cs_2021, subds = "Mean_TOA_RAD_743")), seasia_ebf), seasia_ext)

seasia_nirv_cs_all     <- crop(mask(c(rast(cs_all_2019, subds = "NIRv"), rast(cs_all_2020, subds = "NIRv"), rast(cs_all_2021, subds = "NIRv")), seasia_ebf), seasia_ext)
seasia_nirvr_cs_all    <- crop(mask(c(rast(cs_all_2019, subds = "NIRv_Rad"), rast(cs_all_2020, subds = "NIRv_Rad"), rast(cs_all_2021, subds = "NIRv_Rad")), seasia_ebf), seasia_ext)
seasia_sif_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "SIF_743"), rast(cs_all_2020, subds = "SIF_743"), rast(cs_all_2021, subds = "SIF_743")), seasia_ebf), seasia_ext)
seasia_TOA_cs_all      <- crop(mask(c(rast(cs_all_2019, subds = "Mean_TOA_RAD_743"), rast(cs_all_2020, subds = "Mean_TOA_RAD_743"), rast(cs_all_2021, subds = "Mean_TOA_RAD_743")), seasia_ebf), seasia_ext)

# yield
tropical_yield <- crop(mask(yield, tropical_ebf), tropical_ext)
samerica_yield <- crop(mask(yield, samerica_ebf), samerica_ext)
africa_yield   <- crop(mask(yield, africa_ebf), africa_ext)
seasia_yield   <- crop(mask(yield, seasia_ebf), seasia_ext)

# precip
tropical_precip <- crop(mask(rast(era, subds = "tp"), tropical_ebf), tropical_ext)
samerica_precip <- crop(mask(rast(era, subds = "tp"), samerica_ebf), samerica_ext)
africa_precip   <- crop(mask(rast(era, subds = "tp"), africa_ebf), africa_ext)
seasia_precip   <- crop(mask(rast(era, subds = "tp"), seasia_ebf), seasia_ext)


#### Extract the data ####
tropical_ts_nirv_cf     <- global(tropical_nirv_cf, fun = "mean", na.rm = TRUE)
tropical_ts_nirvr_cf    <- global(tropical_nirvr_cf, fun = "mean", na.rm = TRUE)
tropical_ts_sif_cf      <- global(tropical_sif_cf, fun = "mean", na.rm = TRUE)
tropical_ts_TOA_cf      <- global(tropical_TOA_cf, fun = "mean", na.rm = TRUE)

tropical_ts_nirv_cf     <- as.vector(t(tropical_ts_nirv_cf))
tropical_ts_nirvr_cf    <- as.vector(t(tropical_ts_nirvr_cf))
tropical_ts_sif_cf      <- as.vector(t(tropical_ts_sif_cf))
tropical_ts_TOA_cf      <- as.vector(t(tropical_ts_TOA_cf))

tropical_ts_nirv_cs     <- global(tropical_nirv_cs, fun = "mean", na.rm = TRUE)
tropical_ts_nirvr_cs    <- global(tropical_nirvr_cs, fun = "mean", na.rm = TRUE)
tropical_ts_sif_cs      <- global(tropical_sif_cs, fun = "mean", na.rm = TRUE)
tropical_ts_TOA_cs      <- global(tropical_TOA_cs, fun = "mean", na.rm = TRUE)

tropical_ts_nirv_cs     <- as.vector(t(tropical_ts_nirv_cs))
tropical_ts_nirvr_cs    <- as.vector(t(tropical_ts_nirvr_cs))
tropical_ts_sif_cs      <- as.vector(t(tropical_ts_sif_cs))
tropical_ts_TOA_cs      <- as.vector(t(tropical_ts_TOA_cs))

tropical_ts_nirv_cs_all     <- global(tropical_nirv_cs_all, fun = "mean", na.rm = TRUE)
tropical_ts_nirvr_cs_all    <- global(tropical_nirvr_cs_all, fun = "mean", na.rm = TRUE)
tropical_ts_sif_cs_all      <- global(tropical_sif_cs_all, fun = "mean", na.rm = TRUE)
tropical_ts_TOA_cs_all      <- global(tropical_TOA_cs_all, fun = "mean", na.rm = TRUE)

tropical_ts_nirv_cs_all     <- as.vector(t(tropical_ts_nirv_cs_all))
tropical_ts_nirvr_cs_all    <- as.vector(t(tropical_ts_nirvr_cs_all))
tropical_ts_sif_cs_all      <- as.vector(t(tropical_ts_sif_cs_all))
tropical_ts_TOA_cs_all      <- as.vector(t(tropical_ts_TOA_cs_all))

tropical_ts_sif_rel_cf     <- tropical_ts_sif_cf     / tropical_ts_TOA_cf
tropical_ts_sif_rel_cs     <- tropical_ts_sif_cs     / tropical_ts_TOA_cs
tropical_ts_sif_rel_cs_all <- tropical_ts_sif_cs_all / tropical_ts_TOA_cs_all

tropical_ts_yelu_cf     <- tropical_ts_sif_cf     / tropical_ts_nirvr_cf
tropical_ts_yelu_cs     <- tropical_ts_sif_cs     / tropical_ts_nirvr_cs
tropical_ts_yelu_cs_all <- tropical_ts_sif_cs_all / tropical_ts_nirvr_cs_all

## 
samerica_ts_nirv_cf     <- global(samerica_nirv_cf, fun = "mean", na.rm = TRUE)
samerica_ts_nirvr_cf    <- global(samerica_nirvr_cf, fun = "mean", na.rm = TRUE)
samerica_ts_sif_cf      <- global(samerica_sif_cf, fun = "mean", na.rm = TRUE)
samerica_ts_TOA_cf      <- global(samerica_TOA_cf, fun = "mean", na.rm = TRUE)

samerica_ts_nirv_cf     <- as.vector(t(samerica_ts_nirv_cf))
samerica_ts_nirvr_cf    <- as.vector(t(samerica_ts_nirvr_cf))
samerica_ts_sif_cf      <- as.vector(t(samerica_ts_sif_cf))
samerica_ts_TOA_cf      <- as.vector(t(samerica_ts_TOA_cf))

samerica_ts_nirv_cs     <- global(samerica_nirv_cs, fun = "mean", na.rm = TRUE)
samerica_ts_nirvr_cs    <- global(samerica_nirvr_cs, fun = "mean", na.rm = TRUE)
samerica_ts_sif_cs      <- global(samerica_sif_cs, fun = "mean", na.rm = TRUE)
samerica_ts_TOA_cs      <- global(samerica_TOA_cs, fun = "mean", na.rm = TRUE)

samerica_ts_nirv_cs     <- as.vector(t(samerica_ts_nirv_cs))
samerica_ts_nirvr_cs    <- as.vector(t(samerica_ts_nirvr_cs))
samerica_ts_sif_cs      <- as.vector(t(samerica_ts_sif_cs))
samerica_ts_TOA_cs      <- as.vector(t(samerica_ts_TOA_cs))

samerica_ts_nirv_cs_all     <- global(samerica_nirv_cs_all, fun = "mean", na.rm = TRUE)
samerica_ts_nirvr_cs_all    <- global(samerica_nirvr_cs_all, fun = "mean", na.rm = TRUE)
samerica_ts_sif_cs_all      <- global(samerica_sif_cs_all, fun = "mean", na.rm = TRUE)
samerica_ts_TOA_cs_all      <- global(samerica_TOA_cs_all, fun = "mean", na.rm = TRUE)

samerica_ts_nirv_cs_all     <- as.vector(t(samerica_ts_nirv_cs_all))
samerica_ts_nirvr_cs_all    <- as.vector(t(samerica_ts_nirvr_cs_all))
samerica_ts_sif_cs_all      <- as.vector(t(samerica_ts_sif_cs_all))
samerica_ts_TOA_cs_all      <- as.vector(t(samerica_ts_TOA_cs_all))

# relative
samerica_ts_sif_rel_cf     <- samerica_ts_sif_cf     / samerica_ts_TOA_cf
samerica_ts_sif_rel_cs     <- samerica_ts_sif_cs     / samerica_ts_TOA_cs
samerica_ts_sif_rel_cs_all <- samerica_ts_sif_cs_all / samerica_ts_TOA_cs_all

samerica_ts_nirv_rel_cf     <- samerica_ts_nirv_cf     / samerica_ts_TOA_cf
samerica_ts_nirv_rel_cs     <- samerica_ts_nirv_cs     / samerica_ts_TOA_cs
samerica_ts_nirv_rel_cs_all <- samerica_ts_nirv_cs_all / samerica_ts_TOA_cs_all

samerica_ts_yelu_cf     <- samerica_ts_sif_cf     / samerica_ts_nirvr_cf
samerica_ts_yelu_cs     <- samerica_ts_sif_cs     / samerica_ts_nirvr_cs
samerica_ts_yelu_cs_all <- samerica_ts_sif_cs_all / samerica_ts_nirvr_cs_all

##
africa_ts_nirv_cf     <- global(africa_nirv_cf, fun = "mean", na.rm = TRUE)
africa_ts_nirvr_cf    <- global(africa_nirvr_cf, fun = "mean", na.rm = TRUE)
africa_ts_sif_cf      <- global(africa_sif_cf, fun = "mean", na.rm = TRUE)
africa_ts_TOA_cf      <- global(africa_TOA_cf, fun = "mean", na.rm = TRUE)

africa_ts_nirv_cf     <- as.vector(t(africa_ts_nirv_cf))
africa_ts_nirvr_cf    <- as.vector(t(africa_ts_nirvr_cf))
africa_ts_sif_cf      <- as.vector(t(africa_ts_sif_cf))
africa_ts_TOA_cf      <- as.vector(t(africa_ts_TOA_cf))

africa_ts_nirv_cs     <- global(africa_nirv_cs, fun = "mean", na.rm = TRUE)
africa_ts_nirvr_cs    <- global(africa_nirvr_cs, fun = "mean", na.rm = TRUE)
africa_ts_sif_cs      <- global(africa_sif_cs, fun = "mean", na.rm = TRUE)
africa_ts_TOA_cs      <- global(africa_TOA_cs, fun = "mean", na.rm = TRUE)

africa_ts_nirv_cs     <- as.vector(t(africa_ts_nirv_cs))
africa_ts_nirvr_cs    <- as.vector(t(africa_ts_nirvr_cs))
africa_ts_sif_cs      <- as.vector(t(africa_ts_sif_cs))
africa_ts_TOA_cs      <- as.vector(t(africa_ts_TOA_cs))

africa_ts_nirv_cs_all     <- global(africa_nirv_cs_all, fun = "mean", na.rm = TRUE)
africa_ts_nirvr_cs_all    <- global(africa_nirvr_cs_all, fun = "mean", na.rm = TRUE)
africa_ts_sif_cs_all      <- global(africa_sif_cs_all, fun = "mean", na.rm = TRUE)
africa_ts_TOA_cs_all      <- global(africa_TOA_cs_all, fun = "mean", na.rm = TRUE)

africa_ts_nirv_cs_all     <- as.vector(t(africa_ts_nirv_cs_all))
africa_ts_nirvr_cs_all    <- as.vector(t(africa_ts_nirvr_cs_all))
africa_ts_sif_cs_all      <- as.vector(t(africa_ts_sif_cs_all))
africa_ts_TOA_cs_all      <- as.vector(t(africa_ts_TOA_cs_all))

# relative
africa_ts_sif_rel_cf     <- africa_ts_sif_cf     / africa_ts_TOA_cf
africa_ts_sif_rel_cs     <- africa_ts_sif_cs     / africa_ts_TOA_cs
africa_ts_sif_rel_cs_all <- africa_ts_sif_cs_all / africa_ts_TOA_cs_all

africa_ts_nirv_rel_cf     <- africa_ts_nirv_cf     / africa_ts_TOA_cf
africa_ts_nirv_rel_cs     <- africa_ts_nirv_cs     / africa_ts_TOA_cs
africa_ts_nirv_rel_cs_all <- africa_ts_nirv_cs_all / africa_ts_TOA_cs_all

africa_ts_yelu_cf     <- africa_ts_sif_cf     / africa_ts_nirvr_cf
africa_ts_yelu_cs     <- africa_ts_sif_cs     / africa_ts_nirvr_cs
africa_ts_yelu_cs_all <- africa_ts_sif_cs_all / africa_ts_nirvr_cs_all

##
seasia_ts_nirv_cf     <- global(seasia_nirv_cf, fun = "mean", na.rm = TRUE)
seasia_ts_nirvr_cf    <- global(seasia_nirvr_cf, fun = "mean", na.rm = TRUE)
seasia_ts_sif_cf      <- global(seasia_sif_cf, fun = "mean", na.rm = TRUE)
seasia_ts_TOA_cf      <- global(seasia_TOA_cf, fun = "mean", na.rm = TRUE)

seasia_ts_nirv_cf     <- as.vector(t(seasia_ts_nirv_cf))
seasia_ts_nirvr_cf    <- as.vector(t(seasia_ts_nirvr_cf))
seasia_ts_sif_cf      <- as.vector(t(seasia_ts_sif_cf))
seasia_ts_TOA_cf      <- as.vector(t(seasia_ts_TOA_cf))

seasia_ts_nirv_cs     <- global(seasia_nirv_cs, fun = "mean", na.rm = TRUE)
seasia_ts_nirvr_cs    <- global(seasia_nirvr_cs, fun = "mean", na.rm = TRUE)
seasia_ts_sif_cs      <- global(seasia_sif_cs, fun = "mean", na.rm = TRUE)
seasia_ts_TOA_cs      <- global(seasia_TOA_cs, fun = "mean", na.rm = TRUE)

seasia_ts_nirv_cs     <- as.vector(t(seasia_ts_nirv_cs))
seasia_ts_nirvr_cs    <- as.vector(t(seasia_ts_nirvr_cs))
seasia_ts_sif_cs      <- as.vector(t(seasia_ts_sif_cs))
seasia_ts_TOA_cs      <- as.vector(t(seasia_ts_TOA_cs))

seasia_ts_nirv_cs_all     <- global(seasia_nirv_cs_all, fun = "mean", na.rm = TRUE)
seasia_ts_nirvr_cs_all    <- global(seasia_nirvr_cs_all, fun = "mean", na.rm = TRUE)
seasia_ts_sif_cs_all      <- global(seasia_sif_cs_all, fun = "mean", na.rm = TRUE)
seasia_ts_TOA_cs_all      <- global(seasia_TOA_cs_all, fun = "mean", na.rm = TRUE)

seasia_ts_nirv_cs_all     <- as.vector(t(seasia_ts_nirv_cs_all))
seasia_ts_nirvr_cs_all    <- as.vector(t(seasia_ts_nirvr_cs_all))
seasia_ts_sif_cs_all      <- as.vector(t(seasia_ts_sif_cs_all))
seasia_ts_TOA_cs_all      <- as.vector(t(seasia_ts_TOA_cs_all))

# relative
seasia_ts_sif_rel_cf     <- seasia_ts_sif_cf     / seasia_ts_TOA_cf
seasia_ts_sif_rel_cs     <- seasia_ts_sif_cs     / seasia_ts_TOA_cs
seasia_ts_sif_rel_cs_all <- seasia_ts_sif_cs_all / seasia_ts_TOA_cs_all

seasia_ts_nirv_rel_cf     <- seasia_ts_nirv_cf     / seasia_ts_TOA_cf
seasia_ts_nirv_rel_cs     <- seasia_ts_nirv_cs     / seasia_ts_TOA_cs
seasia_ts_nirv_rel_cs_all <- seasia_ts_nirv_cs_all / seasia_ts_TOA_cs_all

seasia_ts_yelu_cf     <- seasia_ts_sif_cf     / seasia_ts_nirvr_cf
seasia_ts_yelu_cs     <- seasia_ts_sif_cs     / seasia_ts_nirvr_cs
seasia_ts_yelu_cs_all <- seasia_ts_sif_cs_all / seasia_ts_nirvr_cs_all

# yield
tropical_ts_yield <- global(tropical_yield , fun = "mean", na.rm = TRUE)
samerica_ts_yield <- global(samerica_yield , fun = "mean", na.rm = TRUE)
africa_ts_yield   <- global(africa_yield , fun = "mean", na.rm = TRUE)
seasia_ts_yield   <- global(seasia_yield , fun = "mean", na.rm = TRUE)

tropical_ts_yield <- as.vector(t(tropical_ts_yield))
samerica_ts_yield <- as.vector(t(samerica_ts_yield))
africa_ts_yield   <- as.vector(t(africa_ts_yield))
seasia_ts_yield   <- as.vector(t(seasia_ts_yield))

# precip
tropical_ts_precip <- global(tropical_precip , fun = "mean", na.rm = TRUE)
samerica_ts_precip <- global(samerica_precip , fun = "mean", na.rm = TRUE)
africa_ts_precip   <- global(africa_precip , fun = "mean", na.rm = TRUE)
seasia_ts_precip   <- global(seasia_precip , fun = "mean", na.rm = TRUE)

tropical_ts_precip <- as.vector(t(tropical_ts_precip))
samerica_ts_precip <- as.vector(t(samerica_ts_precip))
africa_ts_precip   <- as.vector(t(africa_ts_precip))
seasia_ts_precip   <- as.vector(t(seasia_ts_precip))

### Normalize values
samerica_ts_sif_rel_cs_norm <- min_max_norm(samerica_ts_sif_rel_cs)
africa_ts_sif_rel_cs_norm   <- min_max_norm(africa_ts_sif_rel_cs)
seasia_ts_sif_rel_cs_norm   <- min_max_norm(seasia_ts_sif_rel_cs)

samerica_ts_nirv_rel_cs_norm <- min_max_norm(samerica_ts_nirv_rel_cs)
africa_ts_nirv_rel_cs_norm   <- min_max_norm(africa_ts_nirv_rel_cs)
seasia_ts_nirv_rel_cs_norm   <- min_max_norm(seasia_ts_nirv_rel_cs)

samerica_ts_yelu_cs_norm <- min_max_norm(samerica_ts_yelu_cs)
africa_ts_yelu_cs_norm   <- min_max_norm(africa_ts_yelu_cs)
seasia_ts_yelu_cs_norm   <- min_max_norm(seasia_ts_yelu_cs)

# yield
tropical_ts_yield_norm <- min_max_norm(tropical_ts_yield)
samerica_ts_yield_norm <- min_max_norm(samerica_ts_yield)
africa_ts_yield_norm   <- min_max_norm(africa_ts_yield)
seasia_ts_yield_norm   <- min_max_norm(seasia_ts_yield)

# precip
tropical_ts_precip_norm <- min_max_norm(tropical_ts_precip)
samerica_ts_precip_norm <- min_max_norm(samerica_ts_precip)
africa_ts_precip_norm   <- min_max_norm(africa_ts_precip)
seasia_ts_precip_norm   <- min_max_norm(seasia_ts_precip)

#### Plot Settings ####
x           <- 1:36
xlabs       <- c("Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct")
y_lab_sif   <- list(bquote("SIF Relative"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_nirv  <- list(bquote("NIRv Relative"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_yelu  <- list(bquote("SIF/NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))

mag.cols <- magma(7)
vir.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(256))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.75)

par(mfrow = c(3, 1), oma=c(3.0,2.75,0,0.1), bg = "black")

# samerica
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, samerica_ts_sif_rel_cs_norm, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, samerica_ts_sif_rel_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, samerica_ts_nirv_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
lines(seq(13,24), samerica_ts_yield_norm, col = mag.cols[6], lwd = 1.5, lty = 3)
lines(x, samerica_ts_precip_norm, col = "blue", lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
legend("topleft", legend=c("SIF Relative", "NIRv Relative", "SIF / NIRv Radiance"), col=c(mag.cols[4], mag.cols[5], mag.cols[6]),
       lty=c(1, 2, 3), box.col = "white", text.col = "white", horiz = FALSE, y.intersp = 1, cex = 0.75)
mtext(3, text = "S America Tropical Forest", col = "white", line = 0.5)
box(col = "white")

# Africa
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, africa_ts_sif_rel_cs_norm, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, africa_ts_sif_rel_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, africa_ts_nirv_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
lines(x, africa_ts_yelu_cs_norm, col = mag.cols[6], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(3, text = "Africa Tropical Forest", col = "white", line = 0.5)
box(col = "white")

# SE Asia
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, seasia_ts_sif_rel_cs_norm, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 24, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, seasia_ts_sif_rel_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, seasia_ts_nirv_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
lines(x, seasia_ts_yelu_cs_norm, col = mag.cols[6], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = xlabs, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(3, text = "SE Asia Tropical Forest", col = "white", line = 0.5)
box(col = "white")

mtext(1, text = "2019 - 2021", col = "white", outer = TRUE, line = 1.5)
mtext(2, text = "Normalized Value", col = "white", outer = TRUE, line = 1.5)

dev.off()
