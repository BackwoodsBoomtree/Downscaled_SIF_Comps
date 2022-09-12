library(terra)
library(ncdf4)
library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/sif_comps_white_v2.pdf"

# SIF data
tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_sifd_mean_qc_nadir.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_mean_qc_nadir.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2.csv", header = TRUE)

tropo_pa_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_pa_median.csv", header = TRUE)
oco2_pa_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_pa_median_qc_nadir.csv", header = TRUE)
oco3_pa_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_pa_median_qc_nadir.csv", header = TRUE)
gome2_pa_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_pa_median_qc2.csv", header = TRUE)

tropo_sza_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sza_median.csv", header = TRUE)
oco2_sza_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_sza_median_qc_nadir.csv", header = TRUE)
oco3_sza_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sza_median_qc_nadir.csv", header = TRUE)
gome2_sza_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sza_median_qc2.csv", header = TRUE)

tropo_vza_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_vza_median.csv", header = TRUE)
oco2_vza_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_vza_median_qc_nadir.csv", header = TRUE)
oco3_vza_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_vza_median_qc_nadir.csv", header = TRUE)
gome2_vza_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_vza_median_qc2.csv", header = TRUE)


# MCD43C4 data
get_annual_means <- function(ts_data) {
  df <- as.data.frame(split(ts_data, ceiling(seq_along(ts_data)/12)))
  means <- rowMeans(df)
  return(means)
}

mcd_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000

tropo_sif_mean <- tropo_sif_data$Mean
oco2_sif_mean  <- oco2_sif_data$Mean
oco3_sif_mean  <- oco3_sif_data$Mean
gome2_sif_mean  <- gome2_sif_data$Mean

tropo_pa_median <- tropo_pa_data$Median
oco2_pa_median  <- oco2_pa_data$Median
oco3_pa_median  <- oco3_pa_data$Median
gome2_pa_median  <- gome2_pa_data$Median

tropo_sza_median <- tropo_sza_data$Median
oco2_sza_median  <- oco2_sza_data$Median
oco3_sza_median  <- oco3_sza_data$Median
gome2_sza_median  <- gome2_sza_data$Median

tropo_vza_median <- tropo_vza_data$Median
oco2_vza_median  <- oco2_vza_data$Median
oco3_vza_median  <- oco3_vza_data$Median
gome2_vza_median  <- gome2_vza_data$Median


#### Plot ####
inf.cols   <- inferno(11)
vir.cols   <- viridis(11)
sif.col    <- vir.cols[3]
pa.col     <- inf.cols[3]
sza.col    <- inf.cols[7]
vza.col    <- inf.cols[5]
y_sif      <- c(0.2, 0.7)
y_pa       <- c(0, 70)
y_sza      <- c(0, 70)
y_vza      <- c(0, 70)
y_lab_sif  <- bquote("SIF"[Daily]*" (mW/m"^"2"*"/sr/nm)")
y_lab_pa   <- bquote("Phase Angle")
y_lab_sza  <- bquote("Solar Zenith Angle")
y_lab_vza  <- bquote("Viewing Zenith Angle")
x_lab      <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

cairo_pdf(out_name, width = 11, height = 3.75)

par(mfrow = c(2, 4), oma=c(2.5,3.75,0,7.5))

#### FIRST ROW ####
### TROPOMI ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(tropo_sif_mean, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
mtext(3, text = "TROPOMI")

box()

# Add data
par(new = TRUE)
plot(tropo_sif_mean, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sif.col, col = sif.col, las = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))

legend("topleft", legend = c("SIF"), horiz = TRUE,
       col = c(sif.col), lty = c(1), pch = c(16), lwd = c(1),
       box.col = "transparent", bg = "transparent")

mtext(2, text = y_lab_sif, col = sif.col, line = 1.8)


### OCO2 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(oco2_sif_mean, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
mtext(3, text = "OCO-2")

box()

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))


### OCO3 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(oco3_sif_mean, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
mtext(3, text = "OCO-3")

box()

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))


### GOME2 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(gome2_sif_mean, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
mtext(3, text = "GOME-2")

box()

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))



#### SECOND ROW ####

### TROPOMI ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_pa, axes = FALSE, xaxs="i")

box()

# Add data
lines(tropo_pa_median, col = pa.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = pa.col, col = pa.col, las = 2)

par(new = TRUE)
plot(tropo_sza_median, ylim = y_sza, col = sza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sza.col, col = sza.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(tropo_vza_median, ylim = y_vza, col = vza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)

axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = vza.col, col = vza.col, las = 2, line = 1)
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))

legend("topleft", legend = c("PA", "SZA", "VZA"), horiz = TRUE,
       col = c(pa.col, sza.col, vza.col), lty = c(2,1,1), pch = c(4, 15, 17), lwd = c(1,1,1),
       box.col = "transparent", bg = "transparent")

mtext(2, text = y_lab_pa, col = pa.col, line = 2, outer = FALSE)
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### OCO2 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_pa, axes = FALSE, xaxs="i")

box()

# Add data
lines(oco2_pa_median, col = pa.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
axis(2, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pa.col, col = pa.col, las = 2)

par(new = TRUE)
plot(oco2_sza_median, ylim = y_sza, col = sza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sza.col, col = sza.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(oco2_vza_median, ylim = y_vza, col = vza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)

axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = vza.col, col = vza.col, las = 2, line = 1)
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### OCO3 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_pa, axes = FALSE, xaxs="i")

box()

# Add data
lines(oco3_pa_median, col = pa.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
axis(2, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pa.col, col = pa.col, las = 2)

par(new = TRUE)
plot(oco3_sza_median, ylim = y_sza, col = sza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sza.col, col = sza.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(oco3_vza_median, ylim = y_vza, col = vza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)

axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = vza.col, col = vza.col, las = 2, line = 1)
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### GOME2 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_pa, axes = FALSE, xaxs="i")

box()

# Add data
lines(gome2_pa_median, col = pa.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = pa.col, col = pa.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(gome2_sza_median, ylim = y_sza, col = sza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sza.col, col = sza.col, las = 2)

par(new = TRUE)
plot(gome2_vza_median, ylim = y_vza, col = vza.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = vza.col, col = vza.col, las = 2, line = 4.5)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))

# Margins
mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(4, text = y_lab_sza, col = sza.col, line = 2.75, outer = FALSE)
mtext(4, text = y_lab_vza, col = vza.col, line = 7.55, outer = FALSE)

dev.off()

### Regression analysis

cax_gep_na <- c(NA, cax_gep[1], NA, cax_gep[2:3], NA, cax_gep[4:9])
cax_pc_na  <- c(NA, cax_pc[1], NA, cax_pc[2:3], NA, cax_pc[4:9])
rja_gep_na <- c(NA, rja_gep, NA)
rja_pc_na  <- c(NA, rja_pc, NA)

sif_cat  <- c(k34_sif_cor, cax_sif_cor, k67_sif_cor, rja_sif_cor)
pc_cat   <- c(k34_pc, cax_pc_na, k67_pc, rja_pc_na)
gep_cat  <- c(k34_gep, cax_gep_na, k67_gep, rja_gep_na)
nirv_cat <- c(k34_nirv, cax_nirv, k67_nirv, rja_nirv)

# All
sif_pc_reg   <- lm(sif_cat~pc_cat)
sif_gep_reg  <- lm(sif_cat~gep_cat)
sif_nirv_reg <- lm(sif_cat~nirv_cat)

nirv_pc_reg   <- lm(nirv_cat~pc_cat)
nirv_gep_reg  <- lm(nirv_cat~gep_cat)


summary(sif_pc_reg)
summary(sif_gep_reg)
summary(sif_nirv_reg)

summary(nirv_gep_reg)
summary(nirv_pc_reg)

# K34
k34_sif_pc_reg   <- lm(k34_sif_cor~k34_pc)
k34_sif_gep_reg  <- lm(k34_sif_cor~k34_gep)
k34_sif_nirv_reg <- lm(k34_sif_cor~k34_nirv)
k34_nirv_pc_reg  <- lm(k34_nirv~k34_pc)
k34_nirv_gep_reg <- lm(k34_nirv~k34_gep)


summary(k34_sif_pc_reg)
summary(k34_sif_gep_reg)
summary(k34_sif_nirv_reg)
summary(k34_nirv_pc_reg)
summary(k34_nirv_gep_reg)

# K67
k67_sif_pc_reg   <- lm(k67_sif_cor~k67_pc)
k67_sif_gep_reg  <- lm(k67_sif_cor~k67_gep)
k67_sif_nirv_reg <- lm(k67_sif_cor~k67_nirv)
k67_nirv_pc_reg  <- lm(k67_nirv~k67_pc)
k67_nirv_gep_reg <- lm(k67_nirv~k67_gep)


summary(k67_sif_pc_reg)
summary(k67_sif_gep_reg)
summary(k67_sif_nirv_reg)
summary(k67_nirv_pc_reg)
summary(k67_nirv_gep_reg)

# CAX
cax_sif_pc_reg   <- lm(cax_sif_cor~cax_pc_na)
cax_sif_gep_reg  <- lm(cax_sif_cor~cax_gep_na)
cax_sif_nirv_reg <- lm(cax_sif_cor~cax_nirv)
cax_nirv_pc_reg  <- lm(cax_nirv~cax_pc_na)
cax_nirv_gep_reg <- lm(cax_nirv~cax_gep_na)


summary(cax_sif_pc_reg)
summary(cax_sif_gep_reg)
summary(cax_sif_nirv_reg)
summary(cax_nirv_pc_reg)
summary(cax_nirv_gep_reg)

# RJA
rja_sif_pc_reg   <- lm(rja_sif_cor~rja_pc_na)
rja_sif_gep_reg  <- lm(rja_sif_cor~rja_gep_na)
rja_sif_nirv_reg <- lm(rja_sif_cor~rja_nirv)
rja_nirv_pc_reg  <- lm(rja_nirv~rja_pc_na)
rja_nirv_gep_reg <- lm(rja_nirv~rja_gep_na)


summary(rja_sif_pc_reg)
summary(rja_sif_gep_reg)
summary(rja_sif_nirv_reg)
summary(rja_nirv_pc_reg)
summary(rja_nirv_gep_reg)


