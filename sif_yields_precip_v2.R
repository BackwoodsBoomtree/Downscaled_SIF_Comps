library(terra)
library(viridis)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/v2/sif_yields_precip_samerica_black_v2.pdf"
# out_name   <- "G:/SIF_comps/figs/sif_yields_precip_black.pdf"

data_prefix <- "G:/SIF_comps/csv/amazon/Amazon_2019-2021_"

#### Grab the data ####

ts_sif_rel_cs_all  <- read.csv(paste0(data_prefix, "sif_rel_cs_all.csv"))$Mean
ts_sif_rel_cs_cold <- read.csv(paste0(data_prefix, "sif_rel_cs_cold.csv"))$Mean

ts_sif_nirv_cs_all  <- read.csv(paste0(data_prefix, "sif_nirv_cs_all.csv"))$Mean
ts_sif_nirv_cs_cold <- read.csv(paste0(data_prefix, "sif_nirv_cs_cold.csv"))$Mean

# Clima yield
yield <- rast("G:/ChloFluo/input/yield/1deg/yield.2019.8-day.1deg.nc", subds = "sif_yield")

# Precip
era   <- "G:/ERA5/era5.precip.temp.par.2019-2021.monthly.nc"

#### Min-Max normalization function
min_max_norm <- function(x) {
        (x - min(x)) / (max(x) - min(x))
}

### Convert 8-day data to monthly
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

# Convert yield to monthly data
yield <- toMonth(yield, 2019)

# Veg masks
mask_ebf                 <- rast("G:/MCD12C1/2020/reprocessed/MCD12C1.A2020001.006.EBF.1deg.tif")
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

# PAR
tropical_par <- crop(mask(rast(era, subds = "ssrd"), tropical_ebf), tropical_ext)
samerica_par <- crop(mask(rast(era, subds = "ssrd"), samerica_ebf), samerica_ext)
africa_par   <- crop(mask(rast(era, subds = "ssrd"), africa_ebf), africa_ext)
seasia_par   <- crop(mask(rast(era, subds = "ssrd"), seasia_ebf), seasia_ext)


## 
samerica_ts_par         <- global(samerica_par, fun = "mean", na.rm = TRUE)
samerica_ts_par         <- as.vector(t(samerica_ts_par))

# relative
samerica_ts_nirvp_cs    <- samerica_ts_sif_cs  / (samerica_ts_nirv_cs * samerica_ts_par)


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
tropical_ts_precip_norm <- min_max_norm(tropical_ts_precip)
samerica_ts_precip_norm <- min_max_norm(samerica_ts_precip)
africa_ts_precip_norm   <- min_max_norm(africa_ts_precip)
seasia_ts_precip_norm   <- min_max_norm(seasia_ts_precip)

# yield
ts_sif_rel_cs_all <- min_max_norm(ts_sif_rel_cs_all)
ts_sif_nirv_cs_all <- min_max_norm(ts_sif_nirv_cs_all)

# Regressions
# Means
# samerica_precip_reg      <- lm(samerica_ts_sif_cs ~ samerica_ts_precip)
# summary(samerica_precip_reg)
# 
# samerica_precip_red_abs_reg <- lm(samerica_ts_sif_cs ~ samerica_ts_red_abs_cs + samerica_ts_precip)
# summary(samerica_precip_red_abs_reg)
# 
# samerica_red_abs_reg     <- lm(samerica_ts_sif_cs ~ samerica_ts_red_abs_cs)
# summary(samerica_red_abs_reg)
# 
# samerica_red_rad_reg     <- lm(samerica_ts_sif_cs ~ samerica_ts_red_rad_cs)
# summary(samerica_red_rad_reg)
# 
# samerica_sif_rel_precip_reg  <- lm(samerica_ts_sif_rel_cs ~ samerica_ts_precip)
# summary(samerica_sif_rel_precip_reg)
# 
# samerica_sif_rel_red_abs_reg  <- lm(samerica_ts_sif_rel_cs ~ samerica_ts_red_abs_cs)
# summary(samerica_sif_rel_red_abs_reg)
# 
# samerica_sif_reg <- lm(samerica_ts_sif_cs ~ samerica_ts_sif_rel_cs + samerica_ts_red_rad_cs)
# summary(samerica_sif_reg)
# 
# samerica_sif_rel_reg <- lm(samerica_ts_sif_rel_cs ~ samerica_ts_red_abs_cs + samerica_ts_precip)
# summary(samerica_sif_rel_reg)
# 
# samerica_sif_rel_reg <- lm(samerica_ts_sif_cs ~ samerica_ts_sif_rel_cs)
# summary(samerica_sif_rel_reg)
# 
# samerica_sif_rel_red_rad_reg <- lm(samerica_ts_sif_rel_cs ~ samerica_ts_red_rad_cs)
# summary(samerica_sif_rel_red_rad_reg)
# 
# samerica_yelu_red_rad_reg  <- lm(samerica_ts_sif_cs ~ samerica_ts_yelu_cs + samerica_ts_red_rad_cs)
# summary(samerica_yelu_red_rad_reg)
#  
# samerica_my_reg  <- lm(samerica_ts_my_cs ~ samerica_ts_red_abs_cs + samerica_ts_precip)
# summary(samerica_my_reg)


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
cairo_pdf(out_name, width = 7.5, height = 6.75)

par(mfrow = c(3, 1), oma=c(3.0,2.75,0,0.1), bg = "black")

# samerica
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, rep(NA, times = length(x)), col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, samerica_ts_sif_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, samerica_ts_sif_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
# lines(x, samerica_ts_my_cs_norm, col = mag.cols[7], lwd = 1.5, lty = 2)
lines(x, samerica_ts_yelu_cs_norm, col = vir.cols[4], lwd = 1.5, lty = 2)
lines(x, samerica_ts_nirvp_cs_norm, col = vir.cols[6], lwd = 1.5, lty = 2)

lines(x, samerica_ts_precip_norm, col = "blue", lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
#axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = xlabs, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
legend("topleft", legend=c("SIF", "Precipitation", "SIF Relative", "SIF / NIRv Radiance", "SIF / NIRv * PAR"),
       col=c(mag.cols[4], "blue", mag.cols[5], vir.cols[4], vir.cols[6]),
       lty=c(1, 1, 2, 2, 2), box.col = "white", text.col = "white", ncol = 5, x.intersp = 1, cex = 0.75)
mtext(3, text = "South American Tropical Forest", col = "white", line = 0.25)
# mtext(1, text = "2019", col = "white", line = 1.5, at = 7)
# mtext(1, text = "2020", col = "white", line = 1.5, at = 19)
# mtext(1, text = "2021", col = "white", line = 1.5, at = 31)
# mtext(2, text = "Normalized Value", col = "white", line = 1.75)
box(col = "white")

# Africa
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, rep(NA, times = length(x)), col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, africa_ts_sif_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, africa_ts_sif_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
# lines(x, africa_ts_my_cs_norm, col = mag.cols[7], lwd = 1.5, lty = 2)
lines(x, africa_ts_yelu_cs_norm, col = vir.cols[4], lwd = 1.5, lty = 2)
lines(x, africa_ts_nirvp_cs_norm, col = vir.cols[6], lwd = 1.5, lty = 2)

lines(x, africa_ts_precip_norm, col = "blue", lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
# legend("topleft", legend=c("SIF", "Precipitation", "SIF Relative", "SIF / Red * NIR Radiance", "SIF / NIRv Radiance", "SIF / NIRv * PAR"),
#        col=c(mag.cols[4], "blue", mag.cols[5], mag.cols[7], vir.cols[4], vir.cols[6]),
#        lty=c(1, 1, 2, 2, 2, 2), box.col = "white", text.col = "white", ncol = 3, y.intersp = 1, cex = 0.75)
mtext(3, text = "Africa Tropical Forest", col = "white", line = 0.25)
box(col = "white")

# SE Asia
op <- par(mar = c(0,0.5,2,0.5), bg = "black")
plot(x, rep(NA, times = length(x)), col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(0,1))
rect(13, -100, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, seasia_ts_sif_cs_norm, col = mag.cols[4], lwd = 1.5)
lines(x, seasia_ts_sif_rel_cs_norm, col = mag.cols[5], lwd = 1.5, lty = 2)
# lines(x, seasia_ts_my_cs_norm, col = mag.cols[7], lwd = 1.5, lty = 2)
lines(x, seasia_ts_yelu_cs_norm, col = vir.cols[4], lwd = 1.5, lty = 2)
lines(x, seasia_ts_nirvp_cs_norm, col = vir.cols[6], lwd = 1.5, lty = 2)

lines(x, seasia_ts_precip_norm, col = "blue", lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = xlabs, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
# legend("topleft", legend=c("SIF", "Precipitation", "SIF Relative", "SIF / Red * NIR Radiance", "SIF / NIRv Radiance", "SIF / NIRv * PAR"),
#        col=c(mag.cols[4], "blue", mag.cols[5], mag.cols[7], vir.cols[4], vir.cols[6]),
#        lty=c(1, 1, 2, 2, 2, 2), box.col = "white", text.col = "white", ncol = 3, y.intersp = 1, cex = 0.75)
mtext(3, text = "SE Asia Tropical Forest", col = "white", line = 0.25)
mtext(1, text = "2019", col = "white", line = 1.5, at = 7)
mtext(1, text = "2020", col = "white", line = 1.5, at = 19)
mtext(1, text = "2021", col = "white", line = 1.5, at = 31)

box(col = "white")

# mtext(1, text = "2019 - 2021", col = "white", outer = TRUE, line = 1.5)
mtext(2, text = "Normalized Value", col = "white", outer = TRUE, line = 1.5)

dev.off()
