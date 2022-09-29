library(terra)
library(ncdf4)
library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/sif_comps_single_error_white_v2.pdf"

# SIF data
tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B11_2019-2022_sifd_740_mean_qc_nadir.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_740_mean_qc_nadir.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2.csv", header = TRUE)

tropo_sif_sem <- tropo_sif_data$SEM
oco2_sif_sem  <- oco2_sif_data$SEM
oco3_sif_sem  <- oco3_sif_data$SEM
gome2_sif_sem  <- gome2_sif_data$SEM


# MCD43C4 data
get_monthly_ts_means <- function(ts_data) {
  
  if (typeof(ts_data) != "list") {
    df_m    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,1], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,1])/12)))
    df_n    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,2], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,2])/12)))
  } else {
    df_m    <- as.data.frame(split(ts_data[,1], ceiling(seq_along(ts_data[,1])/12)))
    df_n    <- as.data.frame(split(ts_data[,2], ceiling(seq_along(ts_data[,2])/12)))
  }
  
  
  for (i in 1:nrow(df_m)) {
    mean_w <- weighted.mean(df_m[i,], df_n[i,])
    if (i == 1) {
      means_w <- mean_w
    } else {
      means_w <- c(means_w, mean_w)
    }
  }
  
  return(means_w)
}

# Need to use population n to determine SEM for grouped data
get_monthly_ts_n <- function(ts_data) {
  
  if (typeof(ts_data) != "list") {
    df_n    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,2], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,2])/12)))
  } else {
    df_n    <- as.data.frame(split(ts_data[,2], ceiling(seq_along(ts_data[,2])/12)))
  }
  
  for (i in 1:nrow(df_n)) {
    n <- sum(df_n[i,])
    if (i == 1) {
      ts_n <-n
    } else {
      ts_n <- c(ts_n, n)
    }
  }
  
  return(ts_n)
}

get_monthly_ts_sem <- function(ts_data, ts_means, ts_n, scalar) {
  
  if (typeof(ts_data) != "list") {
    df_m    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,1], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,1])/12))) / scalar
    df_n    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,2], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,2])/12)))
    df_s    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,3], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,3])/12))) / scalar
  } else {
    df_m    <- as.data.frame(split(ts_data[,1], ceiling(seq_along(ts_data[,1])/12))) / scalar
    df_n    <- as.data.frame(split(ts_data[,2], ceiling(seq_along(ts_data[,2])/12)))
    df_s    <- as.data.frame(split(ts_data[,3], ceiling(seq_along(ts_data[,3])/12))) / scalar
  }
  
  for (i in 1:nrow(df_m)) {
    for (j in 1:ncol(df_m)) {
      bgv <- df_n[i, j] * ((df_m[i,j] - ts_means[i])^2)
      wgv <- df_n[i, j] * (df_s[i,j]^2)
      if (j == 1) {
        bgv_all <- bgv
        wgv_all <- wgv
      } else {
        bgv_all <- c(bgv_all, bgv)
        wgv_all <- c(wgv_all, wgv)
      }
    }
    
    pop_var <- sum(bgv_all, wgv_all) / ts_n[i]
    pop_sem <- sqrt(pop_var) / sqrt(ts_n[i])
    
    if (i == 1) {
      sem_all <- pop_sem
    } else {
      sem_all <- c(sem_all, pop_sem)
    }
  }
  
  return(sem_all)
}

mod_nirv    <- get_monthly_ts_means("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv") / 10000
mod_nirv_n   <- get_monthly_ts_n("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv")
mod_nirv_sem <- get_monthly_ts_sem("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv", mod_nirv, mod_nirv_n, 10000)


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
y_sif      <- c(0.00008, 0.0030)
y_nirv     <- c(0.000004, 0.00001)
y_lab_sif  <- bquote("SIF"[Daily]*" (mW/m"^"2"*"/sr/nm)")
y_lab_nirv <- bquote("NIRv")
x_lab      <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

### Plot
cairo_pdf(out_name, width = 6, height = 3)

par(mfrow = c(1, 1), oma=c(2.5,4.50,0.5,4.0))

op <- par(mar = c(0,0,0,0))

plot(mod_nirv_sem, ylim = y_nirv, col = nirv.col, axes = FALSE, xaxs="i", type = "o", lwd = 2, pch = 4, cex = 0.75, lty = 1)
axis(4, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = TRUE)

par(new = TRUE)
plot(gome2_sif_sem, ylim = y_sif, col = sifg.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 17, cex = 0.75, lty = 2)
par(new = TRUE)
plot(tropo_sif_sem, ylim = y_sif, col = sift.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 16, cex = 0.75)
par(new = TRUE)
plot(oco2_sif_sem, ylim = y_sif, col = sifo2.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 15, cex = 0.75, lty = 2)
par(new = TRUE)
plot(oco3_sif_sem, ylim = y_sif, col = sifo3.col, axes = FALSE, xaxs="i", type = "o", lwd = 1, pch = 18, cex = 0.75)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)

legend("topleft", legend = c("GOME-2", "TROPOMI", "OCO-2", "OCO-3", "NIRv"), ncol = 3, lwd = c(1,1,1,1,2),
       col = c(sifg.col, sift.col, sifo2.col, sifo3.col, nirv.col), lty = c(2,1,2,1,1), pch = c(17,16,15,18,4),
       box.col = "transparent", bg = "transparent")

axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))

mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(2, text = y_lab_sif, line = 3)
mtext(4, text = y_lab_nirv, line = 2.5, outer = FALSE)
box()

dev.off()
