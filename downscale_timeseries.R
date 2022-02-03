library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Load Files ####
cf_2018    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2018/TROPOMI.ESA.SIF.2018.global.monthly.1deg.CF20.nc"
# cf_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.CF20.nc"
cf_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.CF20.nc"
cs_2018    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2018/TROPOMI.ESA.SIF.2018.global.monthly.1deg.clearsky.nc"
# cs_2019    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2019/TROPOMI.ESA.SIF.2019.global.monthly.1deg.clearsky.nc"
cs_2020    <- "G:/TROPOMI/esa/gridded/1deg/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.1deg.clearsky.nc"

# CSIF
csif_list <- list.files("G:/CSIF/8-day/daily/1deg/2020", full.names = TRUE, pattern = "*.nc")
for (i in 1:length(csif_list)) {
  if (i == 1) {
    csif <- rast(csif_list[i], subds = "csif_clear")
  } else {
      csif <- c(csif, rast(csif_list[i], subds = "csif_clear"))
  }
}

# GOSIF
gosif <- rast("G:/GOSIF/2020/1deg/GOSIF.2020.1deg.nc", subds = "gosif")

# SIF-LUE
siflue <- rast("G:/SIF-LUE/1deg/GOME_PK_dcSIF_005deg_8day_2018_1deg.nc", subds = "sif-lue")

mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

#### Monthly Function ####
to_month <- function(vals, year) {
  
  # Number of days for the year
  ndays <- ifelse(((year %% 100 != 0) & (year %%4 ==0)) | (year %% 400==0), 366 , 365)
  
  # DOY each value
  nn <- rep(1:length(vals), each = 8)[1:ndays]
  
  # day of year for each month
  m <- as.integer(format(as.Date(1:ndays, origin=paste0(year-1, "-12-31")), "%m"))
  
  # x describes for each day of the year, which layer to use, and which month it is.
  x <- cbind(layer=nn, month=m)
  
  # Now we can, for each month, determine how much of each layer is in that month
  weights <- table(x[,1], x[,2])
  
  s <- c()
  for (i in 1:12) {
    w <- weights[,i]
    x <- vals[which(w > 0)]
    ww <- w[w > 0] / 8
    s[i] <- weighted.mean(x, ww)
  }
  
  return(s)
  
}

#### Mask, Crop, and Ext ####
# Create single mask with cover threshold
m <- mask(mask_ebf, mask_veg)
m[m < 80]  <- NA
# m[m >= 80] <- 1

# Extent
sa_ext     <- extent(c(-82,-34,-15,13))
africa_ext <- extent(c(7,45,-5,5))
asia_ext <- extent(c(95,155,-11,10))

# South America
sif_cf_sa    <- crop(mask(rast(cf_2020, subds = "SIF_Corr_743"), m), sa_ext)
sif_cs_sa    <- crop(mask(rast(cs_2020, subds = "SIF_Corr_743"), m), sa_ext)
sif_cf_sa_18 <- crop(mask(rast(cf_2018, subds = "SIF_Corr_743"), m), sa_ext)
sif_cs_sa_18 <- crop(mask(rast(cs_2018, subds = "SIF_Corr_743"), m), sa_ext)
csif_sa      <- crop(mask(csif, m), sa_ext)
gosif_sa     <- crop(mask(gosif, m), sa_ext)
siflue_sa    <- crop(mask(siflue, m), sa_ext)

# Africa
sif_cf_africa    <- crop(mask(rast(cf_2020, subds = "SIF_Corr_743"), m), africa_ext)
sif_cs_africa    <- crop(mask(rast(cs_2020, subds = "SIF_Corr_743"), m), africa_ext)
sif_cf_africa_18 <- crop(mask(rast(cf_2018, subds = "SIF_Corr_743"), m), africa_ext)
sif_cs_africa_18 <- crop(mask(rast(cs_2018, subds = "SIF_Corr_743"), m), africa_ext)
csif_africa      <- crop(mask(csif, m), africa_ext)
gosif_africa     <- crop(mask(gosif, m), africa_ext)
siflue_africa    <- crop(mask(siflue, m), africa_ext)

# SE Asia
sif_cf_asia    <- crop(mask(rast(cf_2020, subds = "SIF_Corr_743"), m), asia_ext)
sif_cs_asia    <- crop(mask(rast(cs_2020, subds = "SIF_Corr_743"), m), asia_ext)
sif_cf_asia_18 <- crop(mask(rast(cf_2018, subds = "SIF_Corr_743"), m), asia_ext)
sif_cs_asia_18 <- crop(mask(rast(cs_2018, subds = "SIF_Corr_743"), m), asia_ext)
csif_asia      <- crop(mask(csif, m), asia_ext)
gosif_asia     <- crop(mask(gosif, m), asia_ext)
siflue_asia    <- crop(mask(siflue, m), asia_ext)


#### Extract the data ####
# South America
ts_sif_cf_sa    <- global(sif_cf_sa, fun = "mean", na.rm = TRUE)
ts_sif_cs_sa    <- global(sif_cs_sa, fun = "mean", na.rm = TRUE)
ts_sif_cf_sa_18 <- global(sif_cf_sa_18, fun = "mean", na.rm = TRUE)
ts_sif_cs_sa_18 <- global(sif_cs_sa_18, fun = "mean", na.rm = TRUE)
ts_csif_sa      <- global(csif_sa, fun = "mean", na.rm = TRUE)
ts_gosif_sa     <- global(gosif_sa, fun = "mean", na.rm = TRUE)
ts_siflue_sa    <- global(siflue_sa, fun = "mean", na.rm = TRUE)

ts_sif_cf_sa    <- as.vector(t(ts_sif_cf_sa))
ts_sif_cs_sa    <- as.vector(t(ts_sif_cs_sa))
ts_sif_cf_sa_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cf_sa_18)))
ts_sif_cs_sa_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cs_sa_18)))
ts_csif_sa      <- to_month(as.vector(t(ts_csif_sa)), 2020)
ts_gosif_sa     <- to_month(as.vector(t(ts_gosif_sa)), 2020)
ts_siflue_sa    <- to_month(as.vector(t(ts_siflue_sa)), 2018)

# Africa
ts_sif_cf_africa    <- global(sif_cf_africa, fun = "mean", na.rm = TRUE)
ts_sif_cs_africa    <- global(sif_cs_africa, fun = "mean", na.rm = TRUE)
ts_sif_cf_africa_18 <- global(sif_cf_africa_18, fun = "mean", na.rm = TRUE)
ts_sif_cs_africa_18 <- global(sif_cs_africa_18, fun = "mean", na.rm = TRUE)
ts_csif_africa      <- global(csif_africa, fun = "mean", na.rm = TRUE)
ts_gosif_africa     <- global(gosif_africa, fun = "mean", na.rm = TRUE)
ts_siflue_africa    <- global(siflue_africa, fun = "mean", na.rm = TRUE)

ts_sif_cf_africa    <- as.vector(t(ts_sif_cf_africa))
ts_sif_cs_africa    <- as.vector(t(ts_sif_cs_africa))
ts_sif_cf_africa_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cf_africa_18)))
ts_sif_cs_africa_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cs_africa_18)))
ts_csif_africa      <- to_month(as.vector(t(ts_csif_africa)), 2020)
ts_gosif_africa     <- to_month(as.vector(t(ts_gosif_africa)), 2020)
ts_siflue_africa    <- to_month(as.vector(t(ts_siflue_africa)), 2018)

# SE Asia
ts_sif_cf_asia    <- global(sif_cf_asia, fun = "mean", na.rm = TRUE)
ts_sif_cs_asia    <- global(sif_cs_asia, fun = "mean", na.rm = TRUE)
ts_sif_cf_asia_18 <- global(sif_cf_asia_18, fun = "mean", na.rm = TRUE)
ts_sif_cs_asia_18 <- global(sif_cs_asia_18, fun = "mean", na.rm = TRUE)
ts_csif_asia      <- global(csif_asia, fun = "mean", na.rm = TRUE)
ts_gosif_asia     <- global(gosif_asia, fun = "mean", na.rm = TRUE)
ts_siflue_asia    <- global(siflue_asia, fun = "mean", na.rm = TRUE)

ts_sif_cf_asia    <- as.vector(t(ts_sif_cf_asia))
ts_sif_cs_asia    <- as.vector(t(ts_sif_cs_asia))
ts_sif_cf_asia_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cf_asia_18)))
ts_sif_cs_asia_18 <- c(rep(NA, 4), as.vector(t(ts_sif_cs_asia_18)))
ts_csif_asia      <- to_month(as.vector(t(ts_csif_asia)), 2020)
ts_gosif_asia     <- to_month(as.vector(t(ts_gosif_asia)), 2020)
ts_siflue_asia    <- to_month(as.vector(t(ts_siflue_asia)), 2018)

#### Plot Settings ####
x           <- 1:12
xlabs       <- c("Jan", "Apr", "Jul", "Oct")
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_nirv  <- "NIRv"
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))

mag.cols <- magma(7)
vir.cols <- viridis(7)

#### Plot ####
cairo_pdf("G:/SIF_comps/figs/downscale_timeseries_black.pdf", width = 7.5, height = 5.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,5.25,0.1), bg = "black")

# South America
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_sa, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_sa, ts_sif_cf_sa, ts_csif_sa, ts_gosif_sa) - 0.10 * min(ts_sif_cs_sa, ts_sif_cf_sa, ts_csif_sa, ts_gosif_sa),
              max(ts_sif_cs_sa, ts_sif_cf_sa, ts_csif_sa, ts_gosif_sa) + 0.10 * max(ts_sif_cs_sa, ts_sif_cf_sa, ts_csif_sa, ts_gosif_sa)))
lines(x, ts_sif_cs_sa, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_csif_sa, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, ts_gosif_sa, col = vir.cols[4], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "South America Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# South America SIF-LUE
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_sa_18, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_sa_18, ts_sif_cf_sa_18, ts_siflue_sa, na.rm = TRUE) - 0.10 * min(ts_sif_cs_sa_18, ts_sif_cf_sa_18, ts_siflue_sa, na.rm = TRUE),
              max(ts_sif_cs_sa_18, ts_sif_cf_sa_18, ts_siflue_sa, na.rm = TRUE) + 0.10 * max(ts_sif_cs_sa_18, ts_sif_cf_sa_18, ts_siflue_sa, na.rm = TRUE)))
lines(x, ts_sif_cs_sa_18, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_siflue_sa, col = vir.cols[5], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "South America Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Africa
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_africa, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_africa, ts_sif_cf_africa, ts_csif_africa, ts_gosif_africa) - 0.10 * min(ts_sif_cs_africa, ts_sif_cf_africa, ts_csif_africa, ts_gosif_africa),
              max(ts_sif_cs_africa, ts_sif_cf_africa, ts_csif_africa, ts_gosif_africa) + 0.10 * max(ts_sif_cs_africa, ts_sif_cf_africa, ts_csif_africa, ts_gosif_africa)))
lines(x, ts_sif_cs_africa, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_csif_africa, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, ts_gosif_africa, col = vir.cols[4], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "Africa Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Africa SIF-LUE
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_africa_18, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_africa_18, ts_sif_cf_africa_18, ts_siflue_africa, na.rm = TRUE) - 0.10 * min(ts_sif_cs_africa_18, ts_sif_cf_africa_18, ts_siflue_africa, na.rm = TRUE),
              max(ts_sif_cs_africa_18, ts_sif_cf_africa_18, ts_siflue_africa, na.rm = TRUE) + 0.10 * max(ts_sif_cs_africa_18, ts_sif_cf_africa_18, ts_siflue_africa, na.rm = TRUE)))
lines(x, ts_sif_cs_africa_18, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_siflue_africa, col = vir.cols[5], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "Africa Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Asia
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_asia, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_asia, ts_sif_cf_asia, ts_csif_asia, ts_gosif_asia) - 0.10 * min(ts_sif_cs_asia, ts_sif_cf_asia, ts_csif_asia, ts_gosif_asia),
              max(ts_sif_cs_asia, ts_sif_cf_asia, ts_csif_asia, ts_gosif_asia) + 0.10 * max(ts_sif_cs_asia, ts_sif_cf_asia, ts_csif_asia, ts_gosif_asia)))
lines(x, ts_sif_cs_asia, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_csif_asia, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, ts_gosif_asia, col = vir.cols[4], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = xlabs, at = seq(1, 12, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = "2020", tck = FALSE, at = 6.5, mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "Asia Tropical Forest", line = -1.35, col = "white")
box(col = "white")


# Asia SIF-LUE
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cf_asia_18, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_asia_18, ts_sif_cf_asia_18, ts_siflue_asia, na.rm = TRUE) - 0.10 * min(ts_sif_cs_asia_18, ts_sif_cf_asia_18, ts_siflue_asia, na.rm = TRUE),
              max(ts_sif_cs_asia_18, ts_sif_cf_asia_18, ts_siflue_asia, na.rm = TRUE) + 0.10 * max(ts_sif_cs_asia_18, ts_sif_cf_asia_18, ts_siflue_asia, na.rm = TRUE)))
lines(x, ts_sif_cs_asia_18, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_siflue_asia, col = vir.cols[5], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, tck = 0.05, labels = xlabs, at = seq(1, 12, by = 3), mgp=c(3, 0.1, 0), cex.axis = 0.85, col.axis = "white", col = "white")
axis(1, labels = "2018", tck = FALSE, at = 6.5, mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.1, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
mtext(3, text = "Asia Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Legends
par(new=TRUE, mfrow = c(1,1), oma=c(0, 0, 0, 0))
op <- par(mar = c(0, 0, 0, 0)) # Set margins

plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
ltext       <- c("TROPOMI Clear Sky", "TROPOMI Cloud Fraction <0.20", "CSIF", "GOSIF", "SIF-LUE")
legend(0.225, 1.05, legend=ltext, ncol = 2,
       col = c(mag.cols[4], mag.cols[4], vir.cols[3], vir.cols[4], vir.cols[5]),
       lty=c(2, 1, 1, 1, 1), box.col = NA, text.col = "white", bg = "NA")

dev.off()
