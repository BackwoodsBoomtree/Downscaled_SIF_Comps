library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name <- "G:/SIF_comps/figs/clima_sifyield_black.pdf"

#### SIF DATA ####
yield    <- rast("G:/ChloFluo/input/yield/1deg/yield.2019.8-day.1deg.nc", subds = "sif_yield")

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

yield <- toMonth(yield, 2019)

# Masks
mask_ebf   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_veg   <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif")
coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

# Create single mask with cover threshold
m <- mask(mask_ebf, mask_veg)
m[m < 80]  <- NA
# m[m >= 80] <- 1

# Extent
tropics_ext  <- extent(c(-120, 155, -20, 13))
samerica_ext <- extent(c(-82,-34,-20,13))
africa_ext   <- extent(c(7,45,-5,5))
seasia_ext   <- extent(c(95,155,-11,10))

tropics_cover  <- crop(m, tropics_ext)
samerica_cover <- crop(m, samerica_ext)
africa_cover   <- crop(m, africa_ext)
seaia_cover    <- crop(m, seasia_ext)

### Crop Yield
tropics_yield  <- crop(mask(yield, m), tropics_ext)
samerica_yield <- crop(mask(yield, m), samerica_ext)
africa_yield   <- crop(mask(yield, m), africa_ext)
seasia_yield   <- crop(mask(yield, m), seasia_ext)

#### Calculate time series means
ts_tropics_yield  <- global(tropics_yield , fun = "mean", na.rm = TRUE)
ts_samerica_yield <- global(samerica_yield , fun = "mean", na.rm = TRUE)
ts_africa_yield   <- global(africa_yield , fun = "mean", na.rm = TRUE)
ts_seasia_yield   <- global(seasia_yield , fun = "mean", na.rm = TRUE)

### Reorder time series means
ts_tropics_yield  <- as.vector(t(ts_tropics_yield))
ts_samerica_yield <- as.vector(t(ts_samerica_yield))
ts_africa_yield   <- as.vector(t(ts_africa_yield))
ts_seasia_yield   <- as.vector(t(ts_seasia_yield))


#### Plot Settings ####
x        <- 1:12
xlabs    <- month.abb
y_lab    <- bquote("SIF Yield (mJ/nm"^"-1"*"/sr"^"-1"*"/umol"^"-1"*")")
# y_lab    <- list(bquote("SIF Yield"), bquote("(mJ/nm"^"-1"*"/sr"^"-1"*"/umol"^"-1"*")")) # For two lines
mag.cols <- magma(7)


#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.25)


par(oma=c(2.0,0.1,1.25,0.1), bg = "black")

op <- par(mar = c(0,5,0,1), bg = "black")
plot(x, ts_tropics_yield, col = mag.cols[7], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0.004, 0.0052))
lines(x, ts_samerica_yield, col = mag.cols[6], lwd = 1.5, lty = 2)
lines(x, ts_africa_yield, col = mag.cols[5], lwd = 1.5, lty = 2)
lines(x, ts_seasia_yield, col = mag.cols[4], lwd = 1.5, lty = 2)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), labels = xlabs, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = y_lab, col = "white", line = 3.0)
# mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25)) # For two lines
legend("topleft", legend=c("All Tropical Forest", "South America", "Africa", "Southeast Asia"), col=c(mag.cols[7], mag.cols[6], mag.cols[5], mag.cols[4]),
       lty=c(1, 2, 2, 2), box.col = "white", text.col = "white", horiz = FALSE, ncol = 2)
box(col = "white")

dev.off()