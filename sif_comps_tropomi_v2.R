library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/sif_comps_tropomi_white_v2.pdf"

# SIF data
tropo_sifd_data   <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean.csv", header = TRUE)
tropo_no_hot_data <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean_no_hot.csv", header = TRUE)
tropo_cs_data     <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean_clearsky.csv", header = TRUE)
tropo_cs_no_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean_clearsky_no_hot.csv", header = TRUE)

tropo_sifd_mean   <- tropo_sifd_data$Mean
tropo_no_hot_mean <- tropo_no_hot_data$Mean
tropo_cs_mean     <- tropo_cs_data$Mean
tropo_cs_no_mean  <- tropo_cs_no_data$Mean

#### Plot ####
inf.cols   <- inferno(11)
vir.cols   <- viridis(11)
all.col    <- "black"
nohot.col  <- "black"
cs.col     <- vir.cols[7]
csno.col   <- vir.cols[7]

pa.col     <- inf.cols[3]
sza.col    <- inf.cols[7]
vza.col    <- inf.cols[5]
y_sif      <- c(0.2, 0.7)
y_lab_sif  <- bquote("TROPOMI SIF"[Daily]*" (mW/m"^"2"*"/sr/nm)")
x_lab      <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

### Plot
cairo_pdf(out_name, width = 6, height = 3)

par(mfrow = c(1, 1), oma=c(2.5,3.50,0.5,0.5))

op <- par(mar = c(0,0,0,0))

plot(tropo_cs_mean, ylim = y_sif, col = cs.col, axes = FALSE, type = "o", lwd = 2, pch = 15, lty = 1)

par(new = TRUE)
plot(tropo_cs_no_mean, ylim = y_sif, col = csno.col, axes = FALSE, type = "o", lwd = 1, pch = 15, lty = 2)

par(new = TRUE)
plot(tropo_no_hot_mean, ylim = y_sif, col = nohot.col, axes = FALSE, type = "o", lwd = 1, pch = 4, lty = 2)

par(new = TRUE)
plot(tropo_sifd_mean, ylim = y_sif, col = all.col, axes = FALSE, type = "o", lwd = 2, pch = 4, lty = 1)



legend("topleft", legend = c("All data", "No Hotspot", "Clear Sky", "Clear Sky No Hotspot"), ncol = 2, lwd = c(2,1,2,1),
       col = c(all.col, nohot.col, cs.col, csno.col), lty = c(1,2,1,2), pch = c(4,4,15,15),
       box.col = "transparent", bg = "transparent")

axis(1, tck = 0.05, labels = FALSE, at = seq(1, 12, by = 2))
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12))
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = TRUE)

mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(2, text = y_lab_sif, line = 1.8)
box()

dev.off()
