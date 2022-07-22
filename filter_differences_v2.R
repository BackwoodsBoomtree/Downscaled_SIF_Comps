

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/v2/filter_differences_v2.pdf"

## Here we calculate:
## 1. The hotspot effect on clear sky 
## 2. and CF <0.20 data.
## 3. The cloud effect (<0.20) on data with 
## 4. and without hotspot.
## 5. Combined effect of inclusion of cloud and hotspot
## (with hotspot = _all)

# Tropics
data_prefix <- "G:/SIF_comps/csv/tropics/Tropics_2019-2021_"

ts_tropics_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_tropics_sif_cs      <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_tropics_sif_cf      <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean
ts_tropics_sif_cf_all  <- read.csv(paste0(data_prefix, "sif_all.csv"))$Mean

ts_tropics_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_tropics_nirv_cs      <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_tropics_nirv_cf      <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean
ts_tropics_nirv_cf_all  <- read.csv(paste0(data_prefix, "nirv_all.csv"))$Mean

ts_tropics_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_tropics_nirvr_cs      <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_tropics_nirvr_cf      <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean
ts_tropics_nirvr_cf_all  <- read.csv(paste0(data_prefix, "nirvr_all.csv"))$Mean

ts_tropics_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_tropics_red_cs      <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_tropics_red_cf      <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean
ts_tropics_red_cf_all  <- read.csv(paste0(data_prefix, "red_all.csv"))$Mean

ts_tropics_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_tropics_nir_cs      <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_tropics_nir_cf      <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean
ts_tropics_nir_cf_all  <- read.csv(paste0(data_prefix, "nir_all.csv"))$Mean

# Differences
dif_tropics_sif_hotspot_cs   <- (ts_tropics_sif_cs_all - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100
dif_tropics_sif_hotspot_cf   <- (ts_tropics_sif_cf_all - ts_tropics_sif_cf) / abs(ts_tropics_sif_cf) * 100
dif_tropics_sif_cloud        <- (ts_tropics_sif_cf - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100
dif_tropics_sif_cloud_all    <- (ts_tropics_sif_cf_all - ts_tropics_sif_cs_all) / abs(ts_tropics_sif_cs_all) * 100
dif_tropics_sif              <- (ts_tropics_sif_cf_all - ts_tropics_sif_cs) / abs(ts_tropics_sif_cs) * 100

dif_tropics_red_hotspot_cs   <- ts_tropics_red_cs_all - ts_tropics_red_cs
dif_tropics_red_hotspot_cf   <- ts_tropics_red_cf_all - ts_tropics_red_cf
dif_tropics_red_cloud        <- ts_tropics_red_cf - ts_tropics_red_cs
dif_tropics_red_cloud_all    <- ts_tropics_red_cf_all - ts_tropics_red_cs_all
dif_tropics_red              <- ts_tropics_red_cf_all - ts_tropics_red_cs

dif_tropics_nir_hotspot_cs   <- ts_tropics_nir_cs_all - ts_tropics_nir_cs
dif_tropics_nir_hotspot_cf   <- ts_tropics_nir_cf_all - ts_tropics_nir_cf
dif_tropics_nir_cloud        <- ts_tropics_nir_cf - ts_tropics_nir_cs
dif_tropics_nir_cloud_all    <- ts_tropics_nir_cf_all - ts_tropics_nir_cs_all
dif_tropics_nir              <- ts_tropics_nir_cf_all - ts_tropics_nir_cs

dif_tropics_nirv_hotspot_cs   <- (ts_tropics_nirv_cs_all - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100
dif_tropics_nirv_hotspot_cf   <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cf) / abs(ts_tropics_nirv_cf) * 100
dif_tropics_nirv_cloud        <- (ts_tropics_nirv_cf - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100
dif_tropics_nirv_cloud_all    <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cs_all) / abs(ts_tropics_nirv_cs_all) * 100
dif_tropics_nirv              <- (ts_tropics_nirv_cf_all - ts_tropics_nirv_cs) / abs(ts_tropics_nirv_cs) * 100

dif_tropics_nirvr_hotspot_cs   <- (ts_tropics_nirvr_cs_all - ts_tropics_nirvr_cs) / abs(ts_tropics_nirvr_cs) * 100
dif_tropics_nirvr_hotspot_cf   <- (ts_tropics_nirvr_cf_all - ts_tropics_nirvr_cf) / abs(ts_tropics_nirvr_cf) * 100
dif_tropics_nirvr_cloud        <- (ts_tropics_nirvr_cf - ts_tropics_nirvr_cs) / abs(ts_tropics_nirvr_cs) * 100
dif_tropics_nirvr_cloud_all    <- (ts_tropics_nirvr_cf_all - ts_tropics_nirvr_cs_all) / abs(ts_tropics_nirvr_cs_all) * 100
dif_tropics_nirvr              <- (ts_tropics_nirvr_cf_all - ts_tropics_nirvr_cs) / abs(ts_tropics_nirvr_cs) * 100

# Amazon
data_prefix <- "G:/SIF_comps/csv/amazon/Amazon_2019-2021_"

ts_samerica_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_samerica_sif_cs      <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_samerica_sif_cf      <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean
ts_samerica_sif_cf_all  <- read.csv(paste0(data_prefix, "sif_all.csv"))$Mean

ts_samerica_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_samerica_nirv_cs      <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_samerica_nirv_cf      <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean
ts_samerica_nirv_cf_all  <- read.csv(paste0(data_prefix, "nirv_all.csv"))$Mean

ts_samerica_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_samerica_nirvr_cs      <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_samerica_nirvr_cf      <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean
ts_samerica_nirvr_cf_all  <- read.csv(paste0(data_prefix, "nirvr_all.csv"))$Mean

ts_samerica_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_samerica_red_cs      <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_samerica_red_cf      <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean
ts_samerica_red_cf_all  <- read.csv(paste0(data_prefix, "red_all.csv"))$Mean

ts_samerica_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_samerica_nir_cs      <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_samerica_nir_cf      <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean
ts_samerica_nir_cf_all  <- read.csv(paste0(data_prefix, "nir_all.csv"))$Mean

# Differences
dif_samerica_sif_hotspot_cs   <- (ts_samerica_sif_cs_all - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100
dif_samerica_sif_hotspot_cf   <- (ts_samerica_sif_cf_all - ts_samerica_sif_cf) / abs(ts_samerica_sif_cf) * 100
dif_samerica_sif_cloud        <- (ts_samerica_sif_cf - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100
dif_samerica_sif_cloud_all    <- (ts_samerica_sif_cf_all - ts_samerica_sif_cs_all) / abs(ts_samerica_sif_cs_all) * 100
dif_samerica_sif              <- (ts_samerica_sif_cf_all - ts_samerica_sif_cs) / abs(ts_samerica_sif_cs) * 100

dif_samerica_red_hotspot_cs   <- ts_samerica_red_cs_all - ts_samerica_red_cs
dif_samerica_red_hotspot_cf   <- ts_samerica_red_cf_all - ts_samerica_red_cf
dif_samerica_red_cloud        <- ts_samerica_red_cf - ts_samerica_red_cs
dif_samerica_red_cloud_all    <- ts_samerica_red_cf_all - ts_samerica_red_cs_all
dif_samerica_red              <- ts_samerica_red_cf_all - ts_samerica_red_cs

dif_samerica_nir_hotspot_cs   <- ts_samerica_nir_cs_all - ts_samerica_nir_cs
dif_samerica_nir_hotspot_cf   <- ts_samerica_nir_cf_all - ts_samerica_nir_cf
dif_samerica_nir_cloud        <- ts_samerica_nir_cf - ts_samerica_nir_cs
dif_samerica_nir_cloud_all    <- ts_samerica_nir_cf_all - ts_samerica_nir_cs_all
dif_samerica_nir              <- ts_samerica_nir_cf_all - ts_samerica_nir_cs

dif_samerica_nirv_hotspot_cs   <- (ts_samerica_nirv_cs_all - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100
dif_samerica_nirv_hotspot_cf   <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cf) / abs(ts_samerica_nirv_cf) * 100
dif_samerica_nirv_cloud        <- (ts_samerica_nirv_cf - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100
dif_samerica_nirv_cloud_all    <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cs_all) / abs(ts_samerica_nirv_cs_all) * 100
dif_samerica_nirv              <- (ts_samerica_nirv_cf_all - ts_samerica_nirv_cs) / abs(ts_samerica_nirv_cs) * 100

dif_samerica_nirvr_hotspot_cs   <- (ts_samerica_nirvr_cs_all - ts_samerica_nirvr_cs) / abs(ts_samerica_nirvr_cs) * 100
dif_samerica_nirvr_hotspot_cf   <- (ts_samerica_nirvr_cf_all - ts_samerica_nirvr_cf) / abs(ts_samerica_nirvr_cf) * 100
dif_samerica_nirvr_cloud        <- (ts_samerica_nirvr_cf - ts_samerica_nirvr_cs) / abs(ts_samerica_nirvr_cs) * 100
dif_samerica_nirvr_cloud_all    <- (ts_samerica_nirvr_cf_all - ts_samerica_nirvr_cs_all) / abs(ts_samerica_nirvr_cs_all) * 100
dif_samerica_nirvr              <- (ts_samerica_nirvr_cf_all - ts_samerica_nirvr_cs) / abs(ts_samerica_nirvr_cs) * 100

# Africa
data_prefix <- "G:/SIF_comps/csv/africa/Africa_2019-2021_"

ts_africa_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_africa_sif_cs      <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_africa_sif_cf      <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean
ts_africa_sif_cf_all  <- read.csv(paste0(data_prefix, "sif_all.csv"))$Mean

ts_africa_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_africa_nirv_cs      <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_africa_nirv_cf      <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean
ts_africa_nirv_cf_all  <- read.csv(paste0(data_prefix, "nirv_all.csv"))$Mean

ts_africa_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_africa_nirvr_cs      <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_africa_nirvr_cf      <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean
ts_africa_nirvr_cf_all  <- read.csv(paste0(data_prefix, "nirvr_all.csv"))$Mean

ts_africa_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_africa_red_cs      <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_africa_red_cf      <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean
ts_africa_red_cf_all  <- read.csv(paste0(data_prefix, "red_all.csv"))$Mean

ts_africa_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_africa_nir_cs      <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_africa_nir_cf      <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean
ts_africa_nir_cf_all  <- read.csv(paste0(data_prefix, "nir_all.csv"))$Mean

# Differences
dif_africa_sif_hotspot_cs   <- (ts_africa_sif_cs_all - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100
dif_africa_sif_hotspot_cf   <- (ts_africa_sif_cf_all - ts_africa_sif_cf) / abs(ts_africa_sif_cf) * 100
dif_africa_sif_cloud        <- (ts_africa_sif_cf - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100
dif_africa_sif_cloud_all    <- (ts_africa_sif_cf_all - ts_africa_sif_cs_all) / abs(ts_africa_sif_cs_all) * 100
dif_africa_sif              <- (ts_africa_sif_cf_all - ts_africa_sif_cs) / abs(ts_africa_sif_cs) * 100

dif_africa_red_hotspot_cs   <- ts_africa_red_cs_all - ts_africa_red_cs
dif_africa_red_hotspot_cf   <- ts_africa_red_cf_all - ts_africa_red_cf
dif_africa_red_cloud        <- ts_africa_red_cf - ts_africa_red_cs
dif_africa_red_cloud_all    <- ts_africa_red_cf_all - ts_africa_red_cs_all
dif_africa_red              <- ts_africa_red_cf_all - ts_africa_red_cs

dif_africa_nir_hotspot_cs   <- ts_africa_nir_cs_all - ts_africa_nir_cs
dif_africa_nir_hotspot_cf   <- ts_africa_nir_cf_all - ts_africa_nir_cf
dif_africa_nir_cloud        <- ts_africa_nir_cf - ts_africa_nir_cs
dif_africa_nir_cloud_all    <- ts_africa_nir_cf_all - ts_africa_nir_cs_all
dif_africa_nir              <- ts_africa_nir_cf_all - ts_africa_nir_cs

dif_africa_nirv_hotspot_cs   <- (ts_africa_nirv_cs_all - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100
dif_africa_nirv_hotspot_cf   <- (ts_africa_nirv_cf_all - ts_africa_nirv_cf) / abs(ts_africa_nirv_cf) * 100
dif_africa_nirv_cloud        <- (ts_africa_nirv_cf - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100
dif_africa_nirv_cloud_all    <- (ts_africa_nirv_cf_all - ts_africa_nirv_cs_all) / abs(ts_africa_nirv_cs_all) * 100
dif_africa_nirv              <- (ts_africa_nirv_cf_all - ts_africa_nirv_cs) / abs(ts_africa_nirv_cs) * 100

dif_africa_nirvr_hotspot_cs   <- (ts_africa_nirvr_cs_all - ts_africa_nirvr_cs) / abs(ts_africa_nirvr_cs) * 100
dif_africa_nirvr_hotspot_cf   <- (ts_africa_nirvr_cf_all - ts_africa_nirvr_cf) / abs(ts_africa_nirvr_cf) * 100
dif_africa_nirvr_cloud        <- (ts_africa_nirvr_cf - ts_africa_nirvr_cs) / abs(ts_africa_nirvr_cs) * 100
dif_africa_nirvr_cloud_all    <- (ts_africa_nirvr_cf_all - ts_africa_nirvr_cs_all) / abs(ts_africa_nirvr_cs_all) * 100
dif_africa_nirvr              <- (ts_africa_nirvr_cf_all - ts_africa_nirvr_cs) / abs(ts_africa_nirvr_cs) * 100

# SE Asia
data_prefix <- "G:/SIF_comps/csv/seasia/SEAsia_2019-2021_"

ts_seasia_sif_cs_all  <- read.csv(paste0(data_prefix, "sif_cs_all.csv"))$Mean
ts_seasia_sif_cs      <- read.csv(paste0(data_prefix, "sif_cs_cold.csv"))$Mean
ts_seasia_sif_cf      <- read.csv(paste0(data_prefix, "sif_cf_cold.csv"))$Mean
ts_seasia_sif_cf_all  <- read.csv(paste0(data_prefix, "sif_all.csv"))$Mean

ts_seasia_nirv_cs_all  <- read.csv(paste0(data_prefix, "nirv_cs_all.csv"))$Mean
ts_seasia_nirv_cs      <- read.csv(paste0(data_prefix, "nirv_cs_cold.csv"))$Mean
ts_seasia_nirv_cf      <- read.csv(paste0(data_prefix, "nirv_cf_cold.csv"))$Mean
ts_seasia_nirv_cf_all  <- read.csv(paste0(data_prefix, "nirv_all.csv"))$Mean

ts_seasia_nirvr_cs_all  <- read.csv(paste0(data_prefix, "nirvr_cs_all.csv"))$Mean
ts_seasia_nirvr_cs      <- read.csv(paste0(data_prefix, "nirvr_cs_cold.csv"))$Mean
ts_seasia_nirvr_cf      <- read.csv(paste0(data_prefix, "nirvr_cf_cold.csv"))$Mean
ts_seasia_nirvr_cf_all  <- read.csv(paste0(data_prefix, "nirvr_all.csv"))$Mean

ts_seasia_red_cs_all  <- read.csv(paste0(data_prefix, "red_cs_all.csv"))$Mean
ts_seasia_red_cs      <- read.csv(paste0(data_prefix, "red_cs_cold.csv"))$Mean
ts_seasia_red_cf      <- read.csv(paste0(data_prefix, "red_cf_cold.csv"))$Mean
ts_seasia_red_cf_all  <- read.csv(paste0(data_prefix, "red_all.csv"))$Mean

ts_seasia_nir_cs_all  <- read.csv(paste0(data_prefix, "nir_cs_all.csv"))$Mean
ts_seasia_nir_cs      <- read.csv(paste0(data_prefix, "nir_cs_cold.csv"))$Mean
ts_seasia_nir_cf      <- read.csv(paste0(data_prefix, "nir_cf_cold.csv"))$Mean
ts_seasia_nir_cf_all  <- read.csv(paste0(data_prefix, "nir_all.csv"))$Mean

# Differences
dif_seasia_sif_hotspot_cs   <- (ts_seasia_sif_cs_all - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100
dif_seasia_sif_hotspot_cf   <- (ts_seasia_sif_cf_all - ts_seasia_sif_cf) / abs(ts_seasia_sif_cf) * 100
dif_seasia_sif_cloud        <- (ts_seasia_sif_cf - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100
dif_seasia_sif_cloud_all    <- (ts_seasia_sif_cf_all - ts_seasia_sif_cs_all) / abs(ts_seasia_sif_cs_all) * 100
dif_seasia_sif              <- (ts_seasia_sif_cf_all - ts_seasia_sif_cs) / abs(ts_seasia_sif_cs) * 100

dif_seasia_red_hotspot_cs   <- ts_seasia_red_cs_all - ts_seasia_red_cs
dif_seasia_red_hotspot_cf   <- ts_seasia_red_cf_all - ts_seasia_red_cf
dif_seasia_red_cloud        <- ts_seasia_red_cf - ts_seasia_red_cs
dif_seasia_red_cloud_all    <- ts_seasia_red_cf_all - ts_seasia_red_cs_all
dif_seasia_red              <- ts_seasia_red_cf_all - ts_seasia_red_cs

dif_seasia_nir_hotspot_cs   <- ts_seasia_nir_cs_all - ts_seasia_nir_cs
dif_seasia_nir_hotspot_cf   <- ts_seasia_nir_cf_all - ts_seasia_nir_cf
dif_seasia_nir_cloud        <- ts_seasia_nir_cf - ts_seasia_nir_cs
dif_seasia_nir_cloud_all    <- ts_seasia_nir_cf_all - ts_seasia_nir_cs_all
dif_seasia_nir              <- ts_seasia_nir_cf_all - ts_seasia_nir_cs

dif_seasia_nirv_hotspot_cs   <- (ts_seasia_nirv_cs_all - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100
dif_seasia_nirv_hotspot_cf   <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cf) / abs(ts_seasia_nirv_cf) * 100
dif_seasia_nirv_cloud        <- (ts_seasia_nirv_cf - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100
dif_seasia_nirv_cloud_all    <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cs_all) / abs(ts_seasia_nirv_cs_all) * 100
dif_seasia_nirv              <- (ts_seasia_nirv_cf_all - ts_seasia_nirv_cs) / abs(ts_seasia_nirv_cs) * 100

dif_seasia_nirvr_hotspot_cs   <- (ts_seasia_nirvr_cs_all - ts_seasia_nirvr_cs) / abs(ts_seasia_nirvr_cs) * 100
dif_seasia_nirvr_hotspot_cf   <- (ts_seasia_nirvr_cf_all - ts_seasia_nirvr_cf) / abs(ts_seasia_nirvr_cf) * 100
dif_seasia_nirvr_cloud        <- (ts_seasia_nirvr_cf - ts_seasia_nirvr_cs) / abs(ts_seasia_nirvr_cs) * 100
dif_seasia_nirvr_cloud_all    <- (ts_seasia_nirvr_cf_all - ts_seasia_nirvr_cs_all) / abs(ts_seasia_nirvr_cs_all) * 100
dif_seasia_nirvr              <- (ts_seasia_nirvr_cf_all - ts_seasia_nirvr_cs) / abs(ts_seasia_nirvr_cs) * 100


# Labels
x_lab          <- c(1, 13, 25, 36)
lab_tropic     <- "Tropical Forest (TF)"
lab_sa         <- "Amazon TF"
lab_africa     <- "African TF"
lab_seasia     <- "Asia-Pacific TF"
lab_sif        <- bquote("SIF (%)")
lab_nirv       <- bquote("NIRv Ref. (%)")
lab_nirvr      <- bquote("NIRv Rad. (%)")
lab_665        <- bquote("Red Reflectance")
lab_781        <- bquote("NIR Reflectance")
lab_hot_cs     <- bquote("Hotspot effect clearsky")
lab_hot_cf     <- bquote("Hotspot effect CF <0.20")
lab_cloud_hot  <- bquote("Cloud effect w/ hotspot")
lab_cloud_cold <- bquote("Cloud effect no hotspot")
lab_combo      <- bquote("Combined effect")

#### LEGEND PLOT ####
cairo_pdf("G:/SIF_comps/figs/filter_differences_legend.pdf", width = 10, height = 3)

plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', pch = '', ylab = '', xlab = '')

legend("top", bty = "n", legend = c(lab_hot_cf, lab_hot_cs, lab_cloud_hot, lab_cloud_cold, lab_combo),
       lty = c(2, 1, 1, 2, 1),
       lwd = c(1.5, 1.5, 1.5, 1.5, 1.5),
       col = c("red", "red", "blue", "blue", "black"),
       ncol = 3)

dev.off()

#### PLOT ####
cairo_pdf(out_name, width = 7.5, height = 8.5)

par(mfrow = c(5, 4), oma=c(2.5,3,6,0.5))

# SIF
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_tropics_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_sif_cloud, col = "blue", lty = 2)
lines(dif_tropics_sif_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_sif, line = 2)
mtext(3, text = lab_tropic, line = 0.5)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_samerica_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_sif_cloud, col = "blue", lty = 2)
lines(dif_samerica_sif_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_sa, line = 0.5)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_africa_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_sif_cloud, col = "blue", lty = 2)
lines(dif_africa_sif_cloud_all, col = "blue", lty = 1)
lines(dif_africa_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_africa, line = 0.5, lty = 1)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_sif_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-20,20), col = "red")
lines(dif_seasia_sif_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_sif_cloud, col = "blue", lty = 2)
lines(dif_seasia_sif_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_sif)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
mtext(3, text = lab_seasia, line = 0.5)
box()


# red
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_red_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_tropics_red_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_red_cloud, col = "blue", lty = 2)
lines(dif_tropics_red_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_red)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_665, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_red_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_samerica_red_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_red_cloud, col = "blue", lty = 2)
lines(dif_samerica_red_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_red)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_red_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_africa_red_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_red_cloud, col = "blue", lty = 2)
lines(dif_africa_red_cloud_all, col = "blue", lty = 1)
lines(dif_africa_red)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_red_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_seasia_red_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_red_cloud, col = "blue", lty = 2)
lines(dif_seasia_red_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_red)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()


# nir
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_nir_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_tropics_nir_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_nir_cloud, col = "blue", lty = 2)
lines(dif_tropics_nir_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_nir)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_781, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_nir_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_samerica_nir_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_nir_cloud, col = "blue", lty = 2)
lines(dif_samerica_nir_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_nir)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_nir_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_africa_nir_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_nir_cloud, col = "blue", lty = 2)
lines(dif_africa_nir_cloud_all, col = "blue", lty = 1)
lines(dif_africa_nir)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_nir_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-0.06,0.06), col = "red")
lines(dif_seasia_nir_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_nir_cloud, col = "blue", lty = 2)
lines(dif_seasia_nir_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_nir)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
box()


# nirv
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_tropics_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_nirv_cloud, col = "blue", lty = 2)
lines(dif_tropics_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_nirv, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_samerica_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_nirv_cloud, col = "blue", lty = 2)
lines(dif_samerica_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_africa_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_nirv_cloud, col = "blue", lty = 2)
lines(dif_africa_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_africa_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_nirv_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_seasia_nirv_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_nirv_cloud, col = "blue", lty = 2)
lines(dif_seasia_nirv_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_nirv)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = FALSE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()


# nirvr
# tropics
op <- par(mar = c(0,1,1,0.5))
plot(dif_tropics_nirvr_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_tropics_nirvr_hotspot_cf, col = "red", lty = 2)
lines(dif_tropics_nirvr_cloud, col = "blue", lty = 2)
lines(dif_tropics_nirvr_cloud_all, col = "blue", lty = 1)
lines(dif_tropics_nirvr)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)
mtext(2, text = lab_nirvr, line = 2)
box()

# samerica
op <- par(mar = c(0,1,1,0.5))
plot(dif_samerica_nirvr_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_samerica_nirvr_hotspot_cf, col = "red", lty = 2)
lines(dif_samerica_nirvr_cloud, col = "blue", lty = 2)
lines(dif_samerica_nirvr_cloud_all, col = "blue", lty = 1)
lines(dif_samerica_nirvr)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# africa
op <- par(mar = c(0,1,1,0.5))
plot(dif_africa_nirvr_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_africa_nirvr_hotspot_cf, col = "red", lty = 2)
lines(dif_africa_nirvr_cloud, col = "blue", lty = 2)
lines(dif_africa_nirvr_cloud_all, col = "blue", lty = 1)
lines(dif_africa_nirvr)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

# seasia
op <- par(mar = c(0,1,1,0.5))
plot(dif_seasia_nirvr_hotspot_cs, axes = FALSE, xaxt="n", yaxt="n", xlab = NA, ylab = NA, type = "l", lty = 1,
     ylim = c(-25,25), col = "red")
lines(dif_seasia_nirvr_hotspot_cf, col = "red", lty = 2)
lines(dif_seasia_nirvr_cloud, col = "blue", lty = 2)
lines(dif_seasia_nirvr_cloud_all, col = "blue", lty = 1)
lines(dif_seasia_nirvr)
abline(h = 0)
axis(1, tck = 0.03, mgp=c(3, 0.2, 0), at = x_lab, labels = TRUE)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2, labels = FALSE)
box()

mtext(1, text = "Month (2019-2021)", line = 1.5, outer = TRUE)


dev.off()

