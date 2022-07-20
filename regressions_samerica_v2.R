
#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/v2/regressions_samerica_v2.pdf"
data_prefix <- "G:/SIF_comps/csv/amazon/Amazon_2019-2021_"

#### Grab the data ####

ts_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_sif_cs_cold <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_sif_cf_cold <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean

ts_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_nirv_cs_cold <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_nirv_cf_cold <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean

ts_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_nirvr_cs_cold <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_nirvr_cf_cold <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean

ts_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_red_cs_cold <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_red_cf_cold <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean

ts_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_nir_cs_cold <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_nir_cf_cold <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean

# convert red to absorbance
ts_red_cs_all  <- 1-ts_red_cs_all
ts_red_cs_cold <- 1-ts_red_cs_cold
ts_red_cf_cold <- 1-ts_red_cf_cold

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

reg_nirvr_cs_all     <- lm(ts_sif_cs_all ~ ts_nirvr_cs_all)
reg_nirvr_cs_all_sum <- summary(reg_nirvr_cs_all)
reg_nirvr_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_nirvr_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_nirvr_cs_all_p   <- round2(reg_nirvr_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_red_cs_all     <- lm(ts_sif_cs_all ~ ts_red_cs_all)
reg_red_cs_all_sum <- summary(reg_red_cs_all)
reg_red_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_red_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_red_cs_all_p   <- round2(reg_red_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_nir_cs_all     <- lm(ts_sif_cs_all ~ ts_nir_cs_all)
reg_nir_cs_all_sum <- summary(reg_nir_cs_all)
reg_nir_cs_all_r   <- bquote(R^2~" = "~.(round2(reg_nir_cs_all_sum$adj.r.squared, 2, FALSE)))
reg_nir_cs_all_p   <- round2(reg_nir_cs_all_sum$coefficients[2,4], 2, TRUE)

reg_nirv_cs_cold     <- lm(ts_sif_cs_cold ~ ts_nirv_cs_cold)
reg_nirv_cs_cold_sum <- summary(reg_nirv_cs_cold)
reg_nirv_cs_cold_r   <- bquote(R^2~" = "~.(round2(reg_nirv_cs_cold_sum$adj.r.squared, 2, FALSE)))
reg_nirv_cs_cold_p   <- round2(reg_nirv_cs_cold_sum$coefficients[2,4], 2, TRUE)

reg_nirvr_cs_cold     <- lm(ts_sif_cs_cold ~ ts_nirvr_cs_cold)
reg_nirvr_cs_cold_sum <- summary(reg_nirvr_cs_cold)
reg_nirvr_cs_cold_r   <- bquote(R^2~" = "~.(round2(reg_nirvr_cs_cold_sum$adj.r.squared, 2, FALSE)))
reg_nirvr_cs_cold_p   <- round2(reg_nirvr_cs_cold_sum$coefficients[2,4], 2, TRUE)

reg_red_cs_cold     <- lm(ts_sif_cs_cold ~ ts_red_cs_cold)
reg_red_cs_cold_sum <- summary(reg_red_cs_cold)
reg_red_cs_cold_r   <- bquote(R^2~" = "~.(round2(reg_red_cs_cold_sum$adj.r.squared, 2, FALSE)))
reg_red_cs_cold_p   <- round2(reg_red_cs_cold_sum$coefficients[2,4], 2, TRUE)

reg_nir_cs_cold     <- lm(ts_sif_cs_cold ~ ts_nir_cs_cold)
reg_nir_cs_cold_sum <- summary(reg_nir_cs_cold)
reg_nir_cs_cold_r   <- bquote(R^2~" = "~.(round2(reg_nir_cs_cold_sum$adj.r.squared, 2, FALSE)))
reg_nir_cs_cold_p   <- round2(reg_nir_cs_cold_sum$coefficients[2,4], 2, TRUE)

reg_nirv_cf_cold     <- lm(ts_sif_cf_cold ~ ts_nirv_cf_cold)
reg_nirv_cf_cold_sum <- summary(reg_nirv_cf_cold)
reg_nirv_cf_cold_r   <- bquote(R^2~" = "~.(round2(reg_nirv_cf_cold_sum$adj.r.squared, 2, FALSE)))
reg_nirv_cf_cold_p   <- round2(reg_nirv_cf_cold_sum$coefficients[2,4], 2, TRUE)

reg_nirvr_cf_cold     <- lm(ts_sif_cf_cold ~ ts_nirvr_cf_cold)
reg_nirvr_cf_cold_sum <- summary(reg_nirvr_cf_cold)
reg_nirvr_cf_cold_r   <- bquote(R^2~" = "~.(round2(reg_nirvr_cf_cold_sum$adj.r.squared, 2, FALSE)))
reg_nirvr_cf_cold_p   <- round2(reg_nirvr_cf_cold_sum$coefficients[2,4], 2, TRUE)

reg_red_cf_cold     <- lm(ts_sif_cf_cold ~ ts_red_cf_cold)
reg_red_cf_cold_sum <- summary(reg_red_cf_cold)
reg_red_cf_cold_r   <- bquote(R^2~" = "~.(round2(reg_red_cf_cold_sum$adj.r.squared, 2, FALSE)))
reg_red_cf_cold_p   <- round2(reg_red_cf_cold_sum$coefficients[2,4], 2, TRUE)

reg_nir_cf_cold     <- lm(ts_sif_cf_cold ~ ts_nir_cf_cold)
reg_nir_cf_cold_sum <- summary(reg_nir_cf_cold)
reg_nir_cf_cold_r   <- bquote(R^2~" = "~.(round2(reg_nir_cf_cold_sum$adj.r.squared, 2, FALSE)))
reg_nir_cf_cold_p   <- round2(reg_nir_cf_cold_sum$coefficients[2,4], 2, TRUE)

# Labels
lab_title   <- "Amazon Tropical Forest"
lab_sif     <- bquote("SIF (mW/m"^"2"*"/sr/nm)")
lab_nirv    <- bquote("NIRv Reflectance")
lab_nirvr   <- bquote("NIRv Radiance (mW/m"^"2"*"/sr/nm)")
lab_665     <- bquote("Red Absorbance")
lab_781     <- bquote("NIR Reflectance")
lab_cs_cold <- bquote("Clear Sky (No Hotspot)")
lab_cs_all  <- bquote("Clear Sky (w/ Hotspot)")
lab_cf_cold <- bquote("Cloud Frac < 0.20 (w/ Hotspot)")

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(4, 3), oma=c(2.5,2,0,1))

ymin <- min(ts_sif_cs_cold, ts_sif_cs_all, ts_sif_cf_cold)
ymax <- max(ts_sif_cs_cold, ts_sif_cs_all, ts_sif_cf_cold)

# RED
xmin <- min(ts_red_cs_cold, ts_red_cs_all, ts_red_cf_cold)
xmax <- max(ts_red_cs_cold, ts_red_cs_all, ts_red_cf_cold)
op <- par(mar = c(0,3,4,0.5))
plot(ts_red_cs_cold, ts_sif_cs_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_red_cs_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_red_cs_cold_r), as.expression(reg_red_cs_cold_p)))
mtext(3, text = lab_cs_cold, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_red_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_red_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_red_cs_all_r), as.expression(reg_red_cs_all_p)))
mtext(3, text = lab_cs_all, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_red_cf_cold, ts_sif_cf_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_red_cf_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_red_cf_cold_r), as.expression(reg_red_cf_cold_p)))
mtext(3, text = lab_cf_cold, line = 0.5)
mtext(1, text = lab_665, line = 1.75)
box()

# NIR
xmin <- min(ts_nir_cs_cold, ts_nir_cs_all, ts_nir_cf_cold)
xmax <- max(ts_nir_cs_cold, ts_nir_cs_all, ts_nir_cf_cold)
op <- par(mar = c(0,3,4,0.5))
plot(ts_nir_cs_cold, ts_sif_cs_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nir_cs_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topright", bty = "n", legend = c(as.expression(reg_nir_cs_cold_r), as.expression(reg_nir_cs_cold_p)))
mtext(1, text = lab_781, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nir_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nir_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nir_cs_all_r), as.expression(reg_nir_cs_all_p)))
mtext(1, text = lab_781, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nir_cf_cold, ts_sif_cf_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nir_cf_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topright", bty = "n", legend = c(as.expression(reg_nir_cf_cold_r), as.expression(reg_nir_cf_cold_p)))
mtext(1, text = lab_781, line = 1.75)
box()

# NIRv
xmin <- min(ts_nirv_cs_cold, ts_nirv_cs_all, ts_nirv_cf_cold)
xmax <- max(ts_nirv_cs_cold, ts_nirv_cs_all, ts_nirv_cf_cold)
op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cs_cold, ts_sif_cs_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirv_cs_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cs_cold_r), as.expression(reg_nirv_cs_cold_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirv_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cs_all_r), as.expression(reg_nirv_cs_all_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirv_cf_cold, ts_sif_cf_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirv_cf_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirv_cf_cold_r), as.expression(reg_nirv_cf_cold_p)))
mtext(1, text = lab_nirv, line = 1.75)
box()

# NIRv Rad
xmin <- min(ts_nirvr_cs_cold, ts_nirvr_cs_all, ts_nirvr_cf_cold)
xmax <- max(ts_nirvr_cs_cold, ts_nirvr_cs_all, ts_nirvr_cf_cold)
op <- par(mar = c(0,3,4,0.5))
plot(ts_nirvr_cs_cold, ts_sif_cs_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirvr_cs_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirvr_cs_cold_r), as.expression(reg_nirvr_cs_cold_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirvr_cs_all, ts_sif_cs_all, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirvr_cs_all)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirvr_cs_all_r), as.expression(reg_nirvr_cs_all_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

op <- par(mar = c(0,3,4,0.5))
plot(ts_nirvr_cf_cold, ts_sif_cf_cold, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA,
     xlim = c(xmin, xmax), ylim = c(ymin, ymax))
abline(reg_nirvr_cf_cold)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
legend("topleft", bty = "n", legend = c(as.expression(reg_nirvr_cf_cold_r), as.expression(reg_nirvr_cf_cold_p)))
mtext(1, text = lab_nirvr, line = 1.75)
box()

mtext(lab_sif, side = 2, outer = TRUE, line = -0.65, cex = 1.5)
mtext(lab_title, side = 3, outer = TRUE, line = -1.65, cex = 1.5)

dev.off()

