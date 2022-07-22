
#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/v2/error_sa_v2.pdf"
data_prefix <- "G:/SIF_comps/csv/amazon/Amazon_2019-2021_"

#### Grab the data ####

ts_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$SEM
ts_sif_cs_cold <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$SEM
ts_sif_cf_cold <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$SEM

ts_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$SEM
ts_nirv_cs_cold <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$SEM
ts_nirv_cf_cold <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$SEM

ts_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$SEM
ts_nirvr_cs_cold <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$SEM
ts_nirvr_cf_cold <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$SEM

ts_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$SEM
ts_red_cs_cold <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$SEM
ts_red_cf_cold <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$SEM

ts_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$SEM
ts_nir_cs_cold <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$SEM
ts_nir_cf_cold <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$SEM



## PLOT
cairo_pdf(out_name, width = 6.5, height = 8)

par(mfrow = c(5, 3), oma=c(3,1,0,1))

lab_sif   <- bquote("SIF (mW/m"^"2"*"/sr/nm)")
lab_nirv  <- bquote("NIRv (Reflectance)")
lab_nirvr <- bquote("NIRv Radiance (mW/m"^"2"*"/sr/nm)")
lab_665   <- bquote("Red Reflectance")
lab_781   <- bquote("NIR Reflectance")

# SIF
op <- par(mar = c(0,2,4,0))
hist(ts_sif_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)
mtext(3, text = "Clear Sky", line = 0.5)

hist(ts_sif_cs_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(3, text = "Clear Sky No Hotspot", line = 0.5)
mtext(1, text = lab_sif, line = 2)

hist(ts_sif_cf_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(3, text = "CF <0.20 No Hotspot", line = 0.5)

# nirv
hist(ts_nirv_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(ts_nirv_cs_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_nirv, line = 2)
box()

hist(ts_nirv_cf_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# nirv rad
hist(ts_nirvr_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(ts_nirvr_cs_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_nirvr, line = 2)
box()

hist(ts_nirvr_cf_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# red ref
hist(ts_red_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(ts_red_cs_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_665, line = 2)
box()

hist(ts_red_cf_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

# red ref
hist(ts_nir_cs_all, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()
mtext(2, text = "Frequency", line = 1.5)

hist(ts_nir_cs_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(1, text = lab_781, line = 2)
box()

hist(ts_nir_cf_cold, axes = FALSE, xlab = "", ylab = "", main = NA)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0))
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
box()

mtext(3, text = "Monthly Standard Error of the Mean for Amazon Tropical Forest", outer = TRUE, line = -1.5)

dev.off()
