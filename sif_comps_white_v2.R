library(terra)
library(ncdf4)
library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/sif_comps_white_v2.pdf"

# SIF data
tropo_data <- read.csv("G:/SIF_comps/csv/amazon/monthly/Amazon_2019-2021_sif_all.csv", header = TRUE)
oco2_data  <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_sifd_cf.csv", header = TRUE)
oco3_data  <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_cf.csv", header = TRUE)
gome_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_qc2.csv", header = TRUE)

# MCD43C4 data
get_annual_means <- function(ts_data) {
  df <- as.data.frame(split(ts_data, ceiling(seq_along(ts_data)/12)))
  means <- rowMeans(df)
  return(means)
}

mcd_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000

# Make Weighted means ####
get_annual_means_w <- function(ts_data) {
  df_mean   <- as.data.frame(split(ts_data$Mean, ceiling(seq_along(ts_data$Mean)/12)))
  df_n      <- as.data.frame(split(ts_data$n, ceiling(seq_along(ts_data$n)/12)))
  
  for (i in 1:12){
    wm <-  weighted.mean(df_mean[i,], df_n[i,], na.rm = TRUE)
    if (i == 1) {
      ts_wm <- wm
    } else {
      ts_wm <- c(ts_wm, wm)
    }
  }
  return(ts_wm)
}

tropo_mean <- get_annual_means_w(tropo_data)
oco2_mean  <- get_annual_means_w(oco2_data)
oco3_mean  <- get_annual_means_w(oco3_data)
gome_mean  <- get_annual_means_w(gome_data)



#### Plot ####
inf.cols   <- inferno(11)
vir.cols   <- viridis(11)
sif.col    <- vir.cols[3]
gep.col    <- vir.cols[7]
pc.col     <- vir.cols[5]
nirv.col   <- inf.cols[3]
par.col    <- inf.cols[7]
pre.col    <- inf.cols[5]
y_sif      <- c(0.2, 0.7)
y_nirv     <- c(0.10, 0.40)
y_gep      <- c(4, 13)
y_pc_k34   <- c(0.014, 0.032)
y_pc_k67   <- c(0.010, 0.028)
y_pc_cax   <- c(0.014, 0.032)
y_pc_rja   <- c(0.010, 0.028)
y_par      <- c(0, 1000)
y_pre      <- c(0, 1000)
y_lab_sif  <- bquote("SIF"[Daily]*" (mW/m"^"2"*"/sr/nm)")
y_lab_nirv <- bquote("NIRv")
y_lab_gep  <- bquote("GEP (gC m"^"-2"*"day"^"-1"*")")
y_lab_pc   <- bquote("PC (mol CO"[2]*"/mol γ)")
y_lab_par  <- bquote("PAR (µmol m"^"-2"*"s"^"-1"*")")
y_lab_pre  <- bquote("Precipitation (mm)")
x_lab      <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

cairo_pdf(out_name, width = 11, height = 3.75)

par(mfrow = c(2, 4), oma=c(2.5,3.75,0,7.5))

#### FIRST ROW ####
### K67 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")
mtext(3, text = "K67")

# Shaded area
rect(7, 0, 11, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
par(new = TRUE)
plot(k67_sif_cor, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sif.col, col = sif.col, las = 2)

par(new = TRUE)
plot(k67_gep, ylim = y_gep, col = gep.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = gep.col, col = gep.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(k67_pc, ylim = y_pc_k67, col = pc.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pc.col, col = pc.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))

legend("topleft", legend = c("SIF", "GEP", "PC"), horiz = TRUE,
       col = c(sif.col, gep.col, pc.col), lty = c(1,1,1), pch = c(16, 15, 17), lwd = c(1,1,1),
       box.col = "transparent", bg = "transparent")

mtext(2, text = y_lab_sif, col = sif.col, line = 1.8)


### K34 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")
mtext(3, text = "K34")

# Shaded area
rect(8, 0, 9, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
par(new = TRUE)
plot(k34_sif_cor, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = sif.col, col = sif.col, las = 2)

par(new = TRUE)
plot(k34_gep, ylim = y_gep, col = gep.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = gep.col, col = gep.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(k34_pc, ylim = y_pc_k34, col = pc.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pc.col, col = pc.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))


### CAX ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")
mtext(3, text = "CAX")

# Shaded area
rect(8, 0, 11, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
par(new = TRUE)
plot(cax_sif_cor, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sif.col, col = sif.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(c(2, 4, 5, seq(7,12)), cax_gep, xlim = c(1,12), ylim = y_gep, col = gep.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = gep.col, col = gep.col, las = 2)

par(new = TRUE)
plot(c(2, 4, 5, seq(7,12)), cax_pc, xlim = c(1,12), ylim = y_pc_cax, col = pc.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = pc.col, col = pc.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))


### RJA ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")
mtext(3, text = "RJA")

# Shaded area
rect(5, 0, 9, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data

par(new = TRUE)
plot(rja_sif_cor, ylim = y_sif, col = sif.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = sif.col, col = sif.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(seq(2,11), rja_gep, xlim = c(1,12), ylim = y_gep, col = gep.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = gep.col, col = gep.col, las = 2)

par(new = TRUE)
plot(seq(2,11), rja_pc, xlim = c(1,12), ylim = y_pc_rja, col = pc.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = pc.col, col = pc.col, las = 2, line = 4.5)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))

mtext(4, text = y_lab_gep, col = gep.col, line = 2.75, outer = FALSE)
mtext(4, text = y_lab_pc, col = pc.col, line = 7.75, outer = FALSE)

#### SECOND ROW ####

### K67 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")

# Shaded area
rect(7, 0, 11, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
lines(k67_mod_nirv, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
lines(k67_mod_nirv_t, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75, lty = 2)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = nirv.col, col = nirv.col, las = 2)

par(new = TRUE)
plot(k67_wu_par, ylim = y_par, col = par.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = par.col, col = par.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(k67_pre, ylim = y_pre, col = pre.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pre.col, col = pre.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))

legend("topleft", legend = c("NIRv '19-'21", "NIRv", "PAR", "Precip"), ncol = 2,
       col = c(nirv.col, nirv.col, par.col, pre.col), lty = c(2,1,1,1), pch = c(4, 4, 15, 17), lwd = c(1,1,1,1),
       box.col = "transparent", bg = "transparent")

mtext(2, text = y_lab_nirv, col = nirv.col, line = 2, outer = FALSE)
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### K34 ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")

# Shaded area
rect(8, 0, 9, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
lines(k34_mod_nirv, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
lines(k34_mod_nirv_t, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75, lty = 2)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = nirv.col, col = nirv.col, las = 2)

par(new = TRUE)
plot(k34_wu_par, ylim = y_par, col = par.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = par.col, col = par.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(k34_pre, ylim = y_pre, col = pre.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = pre.col, col = pre.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### CAX ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")

# Shaded area
rect(8, 0, 11, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
lines(cax_mod_nirv, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
lines(cax_mod_nirv_t, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75, lty = 2)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = nirv.col, col = nirv.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(c(2, 4, 5, seq(7,12)), cax_wu_par, xlim = c(1,12), ylim = y_par, col = par.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = par.col, col = par.col, las = 2)

par(new = TRUE)
plot(cax_pre, xlim = c(1,12), ylim = y_pre, col = pre.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), labels = FALSE, col.axis = pre.col, col = pre.col, las = 2, line = 1)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))


### RJA ###
op <- par(mar = c(0,0.25,1.5,1.25))

plot(NULL, xlim = c(1,12), ylim = y_nirv, axes = FALSE, xaxs="i")

# Shaded area
rect(5, 0, 9, 1, col = rgb(0.80,0.80,0.80), border = NA)
box()

# Add data
lines(rja_mod_nirv, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75)
lines(rja_mod_nirv_t, col = nirv.col, type = "o", lwd = 1, pch = 4, cex = 0.75, lty = 2)
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = nirv.col, col = nirv.col, las = 2, labels = FALSE)

par(new = TRUE)
plot(seq(2,11), rja_wu_par, xlim = c(1,12), ylim = y_par, col = par.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 15, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = par.col, col = par.col, las = 2)

par(new = TRUE)
plot(rja_pre, xlim = c(1,12), ylim = y_pre, col = pre.col, axes = FALSE, xaxs="i", lty = 1, type = "o", lwd = 1, pch = 17, cex = 0.75)
axis(4, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = pre.col, col = pre.col, las = 2, line = 4.5)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))

# Margins
mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(4, text = y_lab_par, col = par.col, line = 2.75, outer = FALSE)
mtext(4, text = y_lab_pre, col = pre.col, line = 7.55, outer = FALSE)

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


