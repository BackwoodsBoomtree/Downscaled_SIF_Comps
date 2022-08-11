library(terra)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

out_name <- "G:/SIF_comps/figs/v2/downscale_timeseries_black_v2.pdf"

#### Setup raster for EBF clipping and ROIs ####
mcd12_majority <- rast("G:/MCD12C1/2020/reprocessed/percent/MCD12C1.A2020001.006.Percent_LC_03.tif")
mcd12_majority[mcd12_majority < 90]  <- NA

tropics_ext <- ext(c(-180, 180, -23.5, 23.5))
roi_amazon  <- vect("F:/BACKUPS/Russell/Projects/Amazon/Amazon_poly.shp") # Amazon
africa_ext  <- ext(c(-18, 52, -11, 14))
seasia_ext  <- ext(c(72, 180, -23.5, 23.5))

#### Load TROPOMI Data ####
tropics_t_all    <- read.csv("G:/SIF_comps/csv/tropics/16day/Tropics_2019-2021_16day_sif_d_all.csv")$Mean
amazon_t_all     <- read.csv("G:/SIF_comps/csv/amazon/16day/Amazon_2019-2021_16day_sif_d_all.csv")$Mean
africa_t_all     <- read.csv("G:/SIF_comps/csv/africa/16day/Africa_2019-2021_16day_sif_d_all.csv")$Mean
seasia_t_all     <- read.csv("G:/SIF_comps/csv/seasia/16day/SEAsia_2019-2021_16day_sif_d_all.csv")$Mean

tropics_t_cs_all <- read.csv("G:/SIF_comps/csv/tropics/16day/Tropics_2019-2021_16day_sif_d_cs_all.csv")$Mean
amazon_t_cs_all  <- read.csv("G:/SIF_comps/csv/amazon/16day/Amazon_2019-2021_16day_sif_d_cs_all.csv")$Mean
africa_t_cs_all  <- read.csv("G:/SIF_comps/csv/africa/16day/Africa_2019-2021_16day_sif_d_cs_all.csv")$Mean
seasia_t_cs_all  <- read.csv("G:/SIF_comps/csv/seasia/16day/SEAsia_2019-2021_16day_sif_d_cs_all.csv")$Mean

# Get 2-year monthly means
tropics_t_all    <- cbind(tropics_t_all[1:23], tropics_t_all[24:46])
amazon_t_all     <- cbind(amazon_t_all[1:23], amazon_t_all[24:46])
africa_t_all     <- cbind(africa_t_all[1:23], africa_t_all[24:46])
seasia_t_all     <- cbind(seasia_t_all[1:23], seasia_t_all[24:46])
tropics_t_cs_all <- cbind(tropics_t_cs_all[1:23], tropics_t_cs_all[24:46])
amazon_t_cs_all  <- cbind(amazon_t_cs_all[1:23], amazon_t_cs_all[24:46])
africa_t_cs_all  <- cbind(africa_t_cs_all[1:23], africa_t_cs_all[24:46])
seasia_t_cs_all  <- cbind(seasia_t_cs_all[1:23], seasia_t_cs_all[24:46])

tropics_t_all    <- rowMeans(tropics_t_all)
amazon_t_all     <- rowMeans(amazon_t_all)
africa_t_all     <- rowMeans(africa_t_all)
seasia_t_all     <- rowMeans(seasia_t_all)
tropics_t_cs_all <- rowMeans(tropics_t_cs_all)
amazon_t_cs_all  <- rowMeans(amazon_t_cs_all)
africa_t_cs_all  <- rowMeans(africa_t_cs_all)
seasia_t_cs_all  <- rowMeans(seasia_t_cs_all)


#### Grab recon SIF data ####

# CSIF
csif <- rast("G:/CSIF/16-day/CSIF.daily.2019-2020.16day.nc", subds = "clear_daily_SIF")
csif <- mask(csif, mcd12_majority)
csif_tropics <- crop(csif, tropics_ext)
csif_amazon  <- crop(csif, roi_amazon, mask = TRUE)
csif_africa  <- crop(csif, africa_ext)
csif_seasia  <- crop(csif, seasia_ext)

# GOSIF
gosif <- rast("G:/GOSIF/16-day/GOSIF.2019-2020.16-day.nc", subds = "gosif")
gosif <- mask(gosif, mcd12_majority)
gosif_tropics <- crop(gosif, tropics_ext)
gosif_amazon  <- crop(gosif, roi_amazon, mask = TRUE)
gosif_africa  <- crop(gosif, africa_ext)
gosif_seasia  <- crop(gosif, seasia_ext)

# SUN SIF
sunsif_list <- list.files("G:/SIF_OCO2_005/original", pattern = ".nc", full.names = TRUE)
for (i in 1:length(sunsif_list)) {
  sunsif_t <- rast(sunsif_list[i], subds = "sif_ann")
  if (i == 1) {
    sunsif <- sunsif_t
  } else {
    sunsif <- c(sunsif, sunsif_t)
  }
}
sunsif         <- mask(sunsif, mcd12_majority)
sunsif_tropics <- crop(sunsif, tropics_ext)
sunsif_amazon  <- crop(sunsif, roi_amazon, mask = TRUE)
sunsif_africa  <- crop(sunsif, africa_ext)
sunsif_seasia  <- crop(sunsif, seasia_ext)

# compute regional means
for (i in 1:46) {
  csif_tropics_mean_t <- global(csif_tropics[[i]], fun = "mean", na.rm = TRUE)[[1]]
  csif_amazon_mean_t  <- global(csif_amazon[[i]], fun = "mean", na.rm = TRUE)[[1]]
  csif_africa_mean_t  <- global(csif_africa[[i]], fun = "mean", na.rm = TRUE)[[1]]
  csif_seasia_mean_t  <- global(csif_seasia[[i]], fun = "mean", na.rm = TRUE)[[1]]
  
  gosif_tropics_mean_t <- global(gosif_tropics[[i]], fun = "mean", na.rm = TRUE)[[1]]
  gosif_amazon_mean_t  <- global(gosif_amazon[[i]], fun = "mean", na.rm = TRUE)[[1]]
  gosif_africa_mean_t  <- global(gosif_africa[[i]], fun = "mean", na.rm = TRUE)[[1]]
  gosif_seasia_mean_t  <- global(gosif_seasia[[i]], fun = "mean", na.rm = TRUE)[[1]]
  
  if (i == 1) {
    csif_tropics_mean <- csif_tropics_mean_t
    csif_amazon_mean  <- csif_amazon_mean_t
    csif_africa_mean  <- csif_africa_mean_t
    csif_seasia_mean  <- csif_seasia_mean_t
    
    gosif_tropics_mean <- gosif_tropics_mean_t
    gosif_amazon_mean  <- gosif_amazon_mean_t
    gosif_africa_mean  <- gosif_africa_mean_t
    gosif_seasia_mean  <- gosif_seasia_mean_t
  } else {
    csif_tropics_mean <- c(csif_tropics_mean, csif_tropics_mean_t)
    csif_amazon_mean  <- c(csif_amazon_mean, csif_amazon_mean_t)
    csif_africa_mean  <- c(csif_africa_mean, csif_africa_mean_t)
    csif_seasia_mean  <- c(csif_seasia_mean, csif_seasia_mean_t)
    
    gosif_tropics_mean <- c(gosif_tropics_mean, gosif_tropics_mean_t)
    gosif_amazon_mean  <- c(gosif_amazon_mean, gosif_amazon_mean_t)
    gosif_africa_mean  <- c(gosif_africa_mean, gosif_africa_mean_t)
    gosif_seasia_mean  <- c(gosif_seasia_mean, gosif_seasia_mean_t)
  }
}

for (i in 1:48) {
  sunsif_tropics_mean_t <- global(sunsif_tropics[[i]], fun = "mean", na.rm = TRUE)[[1]]
  sunsif_amazon_mean_t  <- global(sunsif_amazon[[i]], fun = "mean", na.rm = TRUE)[[1]]
  sunsif_africa_mean_t  <- global(sunsif_africa[[i]], fun = "mean", na.rm = TRUE)[[1]]
  sunsif_seasia_mean_t  <- global(sunsif_seasia[[i]], fun = "mean", na.rm = TRUE)[[1]]
  if (i == 1) {
    sunsif_tropics_mean <- sunsif_tropics_mean_t
    sunsif_amazon_mean  <- sunsif_amazon_mean_t
    sunsif_africa_mean  <- sunsif_africa_mean_t
    sunsif_seasia_mean  <- sunsif_seasia_mean_t
  } else {
    sunsif_tropics_mean <- c(sunsif_tropics_mean, sunsif_tropics_mean_t)
    sunsif_amazon_mean  <- c(sunsif_amazon_mean, sunsif_amazon_mean_t)
    sunsif_africa_mean  <- c(sunsif_africa_mean, sunsif_africa_mean_t)
    sunsif_seasia_mean  <- c(sunsif_seasia_mean, sunsif_seasia_mean_t)
  }
}


# Get 2-year monthly means
csif_tropics_mean <- cbind(csif_tropics_mean[1:23], csif_tropics_mean[24:46])
csif_amazon_mean  <- cbind(csif_amazon_mean[1:23], csif_amazon_mean[24:46])
csif_africa_mean  <- cbind(csif_africa_mean[1:23], csif_africa_mean[24:46])
csif_seasia_mean  <- cbind(csif_seasia_mean[1:23], csif_seasia_mean[24:46])

csif_tropics_mean <- rowMeans(csif_tropics_mean)
csif_amazon_mean  <- rowMeans(csif_amazon_mean)
csif_africa_mean  <- rowMeans(csif_africa_mean)
csif_seasia_mean  <- rowMeans(csif_seasia_mean)

gosif_tropics_mean <- cbind(gosif_tropics_mean[1:23], gosif_tropics_mean[24:46])
gosif_amazon_mean  <- cbind(gosif_amazon_mean[1:23], gosif_amazon_mean[24:46])
gosif_africa_mean  <- cbind(gosif_africa_mean[1:23], gosif_africa_mean[24:46])
gosif_seasia_mean  <- cbind(gosif_seasia_mean[1:23], gosif_seasia_mean[24:46])

gosif_tropics_mean <- rowMeans(gosif_tropics_mean)
gosif_amazon_mean  <- rowMeans(gosif_amazon_mean)
gosif_africa_mean  <- rowMeans(gosif_africa_mean)
gosif_seasia_mean  <- rowMeans(gosif_seasia_mean)

sunsif_tropics_mean <- cbind(sunsif_tropics_mean[1:24], sunsif_tropics_mean[25:48])
sunsif_amazon_mean  <- cbind(sunsif_amazon_mean[1:24], sunsif_amazon_mean[25:48])
sunsif_africa_mean  <- cbind(sunsif_africa_mean[1:24], sunsif_africa_mean[25:48])
sunsif_seasia_mean  <- cbind(sunsif_seasia_mean[1:24], sunsif_seasia_mean[25:48])

sunsif_tropics_mean <- rowMeans(sunsif_tropics_mean)
sunsif_amazon_mean  <- rowMeans(sunsif_amazon_mean)
sunsif_africa_mean  <- rowMeans(sunsif_africa_mean)
sunsif_seasia_mean  <- rowMeans(sunsif_seasia_mean)

# interpolate sunsif
# sunsif_amazon_mean_l <- approx(sunsif_amazon_mean, method = "linear", n = 23)$y
# sunsif_africa_mean_l <- rowMeans(sunsif_africa_mean)
# sunsif_seasia_mean_l <- rowMeans(sunsif_seasia_mean)
# # sunsif_amazon_mean_s <- spline(sunsif_amazon_mean, n = 365)$y
# # sunsif_amazon_mean_s <- approx(sunsif_amazon_mean, n = 365)$y

#### Plot Settings ####
x           <- 1:23
x_sun       <- 1:24
xlabs       <- seq(1, 365, 32)
sif_lim     <- c(0.25, 0.70)
y_lab_sif  <- bquote("Daily Average SIF (mW/m"^"2"*"/sr/nm)")

mag.cols <- magma(7)
vir.cols <- viridis(7)

#### Plot ####
cairo_pdf(out_name, width = 8.5, height = 4.25)

par(mfrow = c(2, 2), oma=c(2.5,2.5,3.25,0.25), bg = "black")

# Tropical forest
op <- par(mar = c(0,0.5,0,0.5), bg = "black")
plot(x, tropics_t_all, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = sif_lim)
lines(x, tropics_t_cs_all, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, csif_tropics_mean, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, gosif_tropics_mean, col = vir.cols[5], lwd = 1.5, lty = 1)
lines(x_sun, sunsif_tropics_mean, col = vir.cols[7], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 23, by = 2), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(3, text = "Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# South America
op <- par(mar = c(0,0.5,0,0.5), bg = "black")
plot(x, amazon_t_all, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = sif_lim)
lines(x, amazon_t_cs_all, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, csif_amazon_mean, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, gosif_amazon_mean, col = vir.cols[5], lwd = 1.5, lty = 1)
lines(x_sun, sunsif_amazon_mean, col = vir.cols[7], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 23, by = 2), col.axis = "white", col = "white")
axis(2, tck = 0.03, labels = FALSE, col.axis = "white", col = "white", las = 2)
mtext(3, text = "Amazon Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Africa
op <- par(mar = c(0,0.5,0,0.5), bg = "black")
plot(x, africa_t_all, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = sif_lim)
lines(x, africa_t_cs_all, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, csif_africa_mean, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, gosif_africa_mean, col = vir.cols[5], lwd = 1.5, lty = 1)
lines(x_sun, sunsif_africa_mean, col = vir.cols[7], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = xlabs, mgp=c(3, 0.2, 0), at = seq(1, 23, by = 2), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(3, text = "African Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# SE Asia
op <- par(mar = c(0,0.5,0,0.5), bg = "black")
plot(x, seasia_t_all, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = sif_lim)
lines(x, seasia_t_cs_all, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, csif_seasia_mean, col = vir.cols[3], lwd = 1.5, lty = 1)
lines(x, gosif_seasia_mean, col = vir.cols[5], lwd = 1.5, lty = 1)
lines(x_sun, sunsif_seasia_mean, col = vir.cols[7], lwd = 1.5, lty = 1)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = xlabs, mgp=c(3, 0.2, 0), at = seq(1, 23, by = 2), col.axis = "white", col = "white")
axis(2, tck = 0.03, labels = FALSE, col.axis = "white", col = "white", las = 2)
mtext(3, text = "Asia-Pacific Tropical Forest", line = -1.35, col = "white")
box(col = "white")

# Legends
par(new=TRUE, mfrow = c(1,1), oma=c(0, 0, 0, 0))
op <- par(mar = c(0, 0, 0, 0)) # Set margins

plot(NULL, xaxt='n', yaxt='n', bty='n', ylab='', xlab='', xlim=0:1, ylim=0:1)
ltext       <- c("TROPOMI Clear Sky", "TROPOMI CF <0.80", "CSIF", "GOSIF", "SIF_OCO2_005")
legend(0.15, 1.05, legend=ltext, ncol = 3, text.width=c(0,0.085,0.235,0.35, 0.4),
       col = c(mag.cols[4], mag.cols[4], vir.cols[3], vir.cols[5], vir.cols[7]), lwd = c(1.5, 1.5, 1.5, 1.5, 1.5),
       lty=c(2, 1, 1, 1, 1), box.col = NA, text.col = "white", bg = "NA")

mtext(1, text = "Day of Year", col = "white", line = -1, outer = TRUE)
mtext(2, text = y_lab_sif, col = "white", line = -1.35, outer = TRUE)

dev.off()
