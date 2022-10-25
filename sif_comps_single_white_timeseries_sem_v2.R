library(terra)
library(ncdf4)
library(viridis)
library(corrplot)

out_name    <- "G:/SIF_comps/figs/v2/sif_comps_single_white_timeseries_sem_v2.pdf"

# SIF data
tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean_timeseries.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2022_sifd_740_mean_qc_nadir_timeseries.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_740_mean_qc_nadir_timeseries.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2_timeseries.csv", header = TRUE)

tropo_sif_sem <- tropo_sif_data$SEM
oco2_sif_sem  <- oco2_sif_data$SEM
oco3_sif_sem  <- oco3_sif_data$SEM
gome2_sif_sem  <- gome2_sif_data$SEM

# expand data for plotting for 2011 - 2022
tropo_sif_sem <- c(rep(NA, 96), tropo_sif_data$SEM)
oco2_sif_sem  <- c(rep(NA, 48), oco2_sif_data$SEM)
oco3_sif_sem  <- c(rep(NA, 96), oco3_sif_data$SEM)

# # MCD43C4 data
# get_monthly_ts_means <- function(ts_data) {
#   df_m    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,1], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,1])/12)))
#   df_n    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,2], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,2])/12)))
#   
#   for (i in 1:nrow(df_m)) {
#     mean_w <- weighted.mean(df_m[i,], df_n[i,])
#     if (i == 1) {
#       means_w <- mean_w
#     } else {
#       means_w <- c(means_w, mean_w)
#     }
#   }
#   
#   return(means_w)
# }
# mcd_nirv <- get_monthly_ts_means("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv") / 10000


#### Plot ####
inf.cols   <- inferno(11)
vir.cols   <- viridis(11)
sifg.col   <- vir.cols[3]
sift.col   <- vir.cols[5]
sifo2.col  <- vir.cols[7]
sifo3.col  <- vir.cols[9]
nirv.col   <- "#00000025"
pa.col     <- inf.cols[3]
sza.col    <- inf.cols[7]
vza.col    <- inf.cols[5]
y_sem      <- c(0.0001, 0.01)
y_nirv     <- c(0.25, 0.33)
y_lab_sif  <- bquote("Standard Error of the Mean")
y_labs     <- c(bquote("1 x 10"^"-4"), bquote("1 x 10"^"-3"), bquote("1 x 10"^"-2"))
y_lab_nirv <- bquote("NIRv")
x_len      <- c(1,144)
x_lab      <- seq(2011,2022)

### Plot
cairo_pdf(out_name, width = 8, height = 3)

par(mfrow = c(1, 1), oma=c(2.5,4.50,0.5,0.5))

op <- par(mar = c(0,0,0,0))

plot(NA, xlim = x_len, ylim = y_sem, log = "y", col = nirv.col, axes = FALSE, xaxs="i", type = "o", lwd = 2, pch = 4, cex = 0.75, lty = 1)

lines(gome2_sif_sem, col = sifg.col, type = "o", lwd = 1, pch = 17, cex = 0.75, lty = 1)
lines(tropo_sif_sem, col = sift.col, type = "o", lwd = 1, pch = 16, cex = 0.75, lty = 1)
lines(oco2_sif_sem, col = sifo2.col, type = "o", lwd = 1, pch = 15, cex = 0.75, lty = 1)
lines(oco3_sif_sem, col = sifo3.col, type = "o", lwd = 1, pch = 18, cex = 0.75, lty = 1)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = as.expression(y_labs), at = c(0.0001, 0.001, 0.01))

# 
# legend("topleft", legend = c("GOME-2", "TROPOMI", "OCO-2", "OCO-3", "NIRv"), ncol = 3, lwd = c(1,1,1,1,2),
#        col = c(sifg.col, sift.col, sifo2.col, sifo3.col, nirv.col), lty = c(2,1,2,1,1), pch = c(17,16,15,18,4),
#        box.col = "transparent", bg = "transparent")


legend("bottomleft", legend = c("GOME-2", "TROPOMI", "OCO-2", "OCO-3"), ncol = 3, lwd = c(1,1,1,1),
       col = c(sifg.col, sift.col, sifo2.col, sifo3.col), lty = c(1,1,1,1), pch = c(17,16,15,18),
       box.col = "transparent", bg = "transparent")

axis(1, tck = 0.04, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 144, by = 12))
axis(1, tck = 0.03, labels = FALSE, at = seq(7, 144, 12))

mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(2, text = y_lab_sif, line = 3.25)
# mtext(4, text = y_lab_nirv, line = 2, outer = FALSE)
box()

dev.off()
