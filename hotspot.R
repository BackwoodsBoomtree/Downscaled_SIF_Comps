library(ncdf4)
library(terra)
library(viridis)

out_name   <- "G:/SIF_comps/figs/hotspot_black.pdf"

sol_files <- c("G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-11.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-12.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-13.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-14.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-16.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-17.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-18.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-19.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-21.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-22.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-23.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-24.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-25.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-27.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-28.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-29.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/06/TROPOSIF_L2B_2021-06-30.nc")
equ_files <- c("G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-13.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-14.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-15.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-16.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-17.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-18.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-20.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-21.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-22.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-23.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-25.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-26.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-27.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-28.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-29.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/09/TROPOSIF_L2B_2021-09-30.nc",
               "G:/TROPOMI/esa/original/v2.1/l2b/2021/10/TROPOSIF_L2B_2021-10-01.nc")

# Veg mask (max NDVI > 0.40)
mask_veg                 <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.04.1deg.tif")
mask_veg                 <- as.polygons(mask_veg, dissolve = TRUE)
mask_ebf                 <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_ebf[mask_ebf < 80]  <- NA
samerica_ext             <- ext(c(-82,-34,-20,13))
mask_ebf                 <- crop(mask_ebf, samerica_ext)
mask_ebf[mask_ebf >= 80] <- 1
mask_ebf                 <- as.polygons(mask_ebf, dissolve = TRUE)

# Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Grab data function
grab_data <- function(file, region) {
  
  # Open data
  data <- nc_open(file)
  
  # Get variables
  sif    <- ncvar_get(data, "PRODUCT/SIF_743")
  nirv   <- ncvar_get(data, "PRODUCT/NIRv")
  nirvr  <- ncvar_get(data, "PRODUCT/NIRv_RAD")
  ref665 <- ncvar_get(data, "PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/TOA_RFL")[1,]
  ref781 <- ncvar_get(data, "PRODUCT/SUPPORT_DATA/DETAILED_RESULTS/TOA_RFL")[7,]
  pa     <- ncvar_get(data, "PRODUCT/SUPPORT_DATA/GEOLOCATIONS/phase_angle")
  cf     <- ncvar_get(data, "PRODUCT/SUPPORT_DATA/INPUT_DATA/cloud_fraction_L2")
  lat    <- ncvar_get(data, "PRODUCT/latitude")
  lon    <- ncvar_get(data, "PRODUCT/longitude")
  
  # Create DF
  df     <- data.frame(sif, nirv, nirvr, ref665, ref781, pa, cf, lat, lon)
  
  # Set phase angle to absolute value
  df$pa  <- abs(df$pa)
  
  # Subset for clear sky (cloud fraction == 0)
  df     <- subset(df, cf == 0)
  
  if (region == "Amazonia") {
    df     <- subset(df, lat <= 13)
    df     <- subset(df, lat >= -20)
    df     <- subset(df, lon <= -34)
    df     <- subset(df, lon >= -82)
  } else if (region == "US") {
    df     <- subset(df, lat <= 45)
    df     <- subset(df, lat >= 40)
    df     <- subset(df, lon <= -89)
    df     <- subset(df, lon >= -100)
  }
  
  return(df)
}

# Get data for US at solstice
for (i in 1:length(sol_files)) {
  
  print(paste0("Grabbing ", sol_files[i]))
  df <- grab_data(sol_files[i], "US")
  
  if (i == 1) {
    us_df <- df
  } else {
    us_df <- rbind(us_df, df)
  }
  print(paste0("Added n records: ", nrow(df)))
}

# Get data for Amazonia at equinox
for (j in 1:length(equ_files)) {
  
  print(paste0("Grabbing ", equ_files[j]))
  df <- grab_data(equ_files[j], "Amazonia")
  
  if (j == 1) {
    sa_df <- df
  } else {
    sa_df <- rbind(sa_df, df)
  }
  print(paste0("Added n records: ", nrow(df)))
}


# Transform to SpatVector to mask by veg
vect_us <- vect(us_df, geom = c("lon", "lat"))
vect_sa <- vect(sa_df, geom = c("lon", "lat"))

# Mask by veg
vect_us <- intersect(vect_us, mask_veg)
vect_sa <- intersect(vect_sa, mask_ebf)

plot(vect_sa)
plot(mask_ebf, add = TRUE)
plot(vect_us)
plot(mask_veg, add = TRUE)

# Return to data.frame
us_df <- as.data.frame(vect_us)
sa_df <- as.data.frame(vect_sa)

### Remove outliers
#find absolute value of z-score for each value in each column
us_z_scores <- us_df
sa_z_scores <- sa_df
us_z_scores[1:5] <- as.data.frame(sapply(us_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
sa_z_scores[1:5] <- as.data.frame(sapply(sa_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))

# keep only rows with all z-scores less than absolute value of 3 
us_no_outliers <- us_df[!rowSums(us_z_scores[1:5]>3), ]
sa_no_outliers <- sa_df[!rowSums(sa_z_scores[1:5]>3), ]

# Normalize values
us_norm <- as.data.frame(lapply(us_no_outliers[1:5], min_max_norm))
sa_norm <- as.data.frame(lapply(sa_no_outliers[1:5], min_max_norm))

# Fit polynomials
us_sif_fit    <- lm(us_norm$sif    ~ us_no_outliers$pa + I(us_no_outliers$pa^2))
us_nirv_fit   <- lm(us_norm$nirv   ~ us_no_outliers$pa + I(us_no_outliers$pa^2))
us_nirvr_fit  <- lm(us_norm$nirvr  ~ us_no_outliers$pa + I(us_no_outliers$pa^2))
us_ref665_fit <- lm(us_norm$ref665 ~ us_no_outliers$pa + I(us_no_outliers$pa^2))
us_ref781_fit <- lm(us_norm$ref781 ~ us_no_outliers$pa + I(us_no_outliers$pa^2))

sa_sif_fit    <- lm(sa_norm$sif    ~ sa_no_outliers$pa + I(sa_no_outliers$pa^2))
sa_nirv_fit   <- lm(sa_norm$nirv   ~ sa_no_outliers$pa + I(sa_no_outliers$pa^2))
sa_nirvr_fit  <- lm(sa_norm$nirvr  ~ sa_no_outliers$pa + I(sa_no_outliers$pa^2))
sa_ref665_fit <- lm(sa_norm$ref665 ~ sa_no_outliers$pa + I(sa_no_outliers$pa^2))
sa_ref781_fit <- lm(sa_norm$ref781 ~ sa_no_outliers$pa + I(sa_no_outliers$pa^2))

# get model coefficients
us_sif_coeff    <- round(us_sif_fit$coefficients, 4)
us_nirv_coeff   <- round(us_nirv_fit$coefficients, 4)
us_nirvr_coeff  <- round(us_nirvr_fit$coefficients, 4)
us_ref665_coeff <- round(us_ref665_fit$coefficients, 4)
us_ref781_coeff <- round(us_ref781_fit$coefficients, 4)

sa_sif_coeff    <- round(sa_sif_fit$coefficients, 4)
sa_nirv_coeff   <- round(sa_nirv_fit$coefficients, 4)
sa_nirvr_coeff  <- round(sa_nirvr_fit$coefficients, 4)
sa_ref665_coeff <- round(sa_ref665_fit$coefficients, 4)
sa_ref781_coeff <- round(sa_ref781_fit$coefficients, 4)

# Predict all PAs
us_pa_seq <- seq(0,max(us_no_outliers$pa), length.out = 10000)
us_pred   <- data.frame(pa = us_pa_seq)
sa_pa_seq <- seq(0,max(sa_no_outliers$pa), length.out = 10000)
sa_pred   <- data.frame(pa = sa_pa_seq)

for (i in 1:nrow(us_pred)) {
  us_pred$sif[i]    <- us_sif_coeff[1] + (us_sif_coeff[2] * us_pred$pa[i]) + (us_sif_coeff[3] * (us_pred$pa[i]^2))
  us_pred$nirv[i]   <- us_nirv_coeff[1] + (us_nirv_coeff[2] * us_pred$pa[i]) + (us_nirv_coeff[3] * (us_pred$pa[i]^2))
  us_pred$nirvr[i]  <- us_nirvr_coeff[1] + (us_nirvr_coeff[2] * us_pred$pa[i]) + (us_nirvr_coeff[3] * (us_pred$pa[i]^2))
  us_pred$ref665[i] <- us_ref665_coeff[1] + (us_ref665_coeff[2] * us_pred$pa[i]) + (us_ref665_coeff[3] * (us_pred$pa[i]^2))
  us_pred$ref781[i] <- us_ref781_coeff[1] + (us_ref781_coeff[2] * us_pred$pa[i]) + (us_ref781_coeff[3] * (us_pred$pa[i]^2))
}

for (i in 1:nrow(sa_pred)) {
  sa_pred$sif[i]    <- sa_sif_coeff[1] + (sa_sif_coeff[2] * sa_pred$pa[i]) + (sa_sif_coeff[3] * (sa_pred$pa[i]^2))
  sa_pred$nirv[i]   <- sa_nirv_coeff[1] + (sa_nirv_coeff[2] * sa_pred$pa[i]) + (sa_nirv_coeff[3] * (sa_pred$pa[i]^2))
  sa_pred$nirvr[i]  <- sa_nirvr_coeff[1] + (sa_nirvr_coeff[2] * sa_pred$pa[i]) + (sa_nirvr_coeff[3] * (sa_pred$pa[i]^2))
  sa_pred$ref665[i] <- sa_ref665_coeff[1] + (sa_ref665_coeff[2] * sa_pred$pa[i]) + (sa_ref665_coeff[3] * (sa_pred$pa[i]^2))
  sa_pred$ref781[i] <- sa_ref781_coeff[1] + (sa_ref781_coeff[2] * sa_pred$pa[i]) + (sa_ref781_coeff[3] * (sa_pred$pa[i]^2))
}

# Eqs for US legend
us_sif_eq    <- paste0("SIF = ", us_sif_coeff[1], us_sif_coeff[2], "x", "+", us_sif_coeff[3], "x^2")
us_nirv_eq   <- paste0("NIRv = ", us_nirv_coeff[1], us_nirv_coeff[2], "x", "+", us_nirv_coeff[3], "x^2")
us_nirvr_eq  <- paste0("NIRvR = ", us_nirvr_coeff[1], us_nirvr_coeff[2], "x", "+", us_nirvr_coeff[3], "x^2")
us_ref665_eq <- paste0("REF665 = ", us_ref665_coeff[1], us_ref665_coeff[2], "x", "+", us_ref665_coeff[3], "x^2")
us_ref781_eq <- paste0("REF781 = ", us_ref781_coeff[1], us_ref781_coeff[2], "x", "+", us_ref781_coeff[3], "x^2")


# Eqs for Amazonia legend
sa_sif_eq    <- paste0("SIF = ", sa_sif_coeff[1], sa_sif_coeff[2], "x", "+", sa_sif_coeff[3], "x^2")
sa_nirv_eq   <- paste0("NIRv = ", sa_nirv_coeff[1], sa_nirv_coeff[2], "x", "+", sa_nirv_coeff[3], "x^2")
sa_nirvr_eq  <- paste0("NIRvR = ", sa_nirvr_coeff[1], sa_nirvr_coeff[2], "x", "+", sa_nirvr_coeff[3], "x^2")
sa_ref665_eq <- paste0("REF665 = ", sa_ref665_coeff[1], sa_ref665_coeff[2], "x", "+", sa_ref665_coeff[3], "x^2")
sa_ref781_eq <- paste0("REF781 = ", sa_ref781_coeff[1], sa_ref781_coeff[2], "x", "+", sa_ref781_coeff[3], "x^2")


#### Plot ####
mag.cols <- magma(7)

cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(1, 2), oma=c(3.0,2.75,1.25,0.1), bg = "black")

op <- par(mar = c(0,0.5,0,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "Tropical Amazon Forest September Equniox", col = "white")

# Add polynomial curves
lines(sa_pred$pa, sa_pred$sif, col=mag.cols[2], lwd = 2)
lines(sa_pred$pa, sa_pred$nirv, col=mag.cols[3], lwd = 2, lty = 4)
lines(sa_pred$pa, sa_pred$nirvr, col=mag.cols[4], lwd = 2, lty = 2)
lines(sa_pred$pa, sa_pred$ref665, col=mag.cols[5], lwd = 2, lty = 4)
lines(sa_pred$pa, sa_pred$ref781, col=mag.cols[6], lwd = 2, lty = 2)

axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")
mtext(2, text = "Normalized Value", col = "white", line = 2)

legend("topright", legend = c(sa_sif_eq, sa_nirv_eq, sa_nirvr_eq, sa_ref665_eq, sa_ref781_eq),
       col = c(mag.cols[2],mag.cols[3],mag.cols[4],mag.cols[5],mag.cols[6]), lty = c(1,4,2,4,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.65)

box(col = "white")


#### Plot ####
op <- par(mar = c(0,0.5,0,0.5), bg = "black")

plot(NULL, xlim = c(0, 70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "US Corn Belt June Solstice", col = "white")

# Add polynomial curves
lines(us_pred$pa, us_pred$sif, col=mag.cols[2], lwd = 2)
lines(us_pred$pa, us_pred$nirv, col=mag.cols[3], lwd = 2, lty = 4)
lines(us_pred$pa, us_pred$nirvr, col=mag.cols[4], lwd = 2, lty = 2)
lines(us_pred$pa, us_pred$ref665, col=mag.cols[5], lwd = 2, lty = 4)
lines(us_pred$pa, us_pred$ref781, col=mag.cols[6], lwd = 2, lty = 2)

axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(us_sif_eq, us_nirv_eq, us_nirvr_eq, us_ref665_eq, us_ref781_eq),
       col = c(mag.cols[2],mag.cols[3],mag.cols[4],mag.cols[5],mag.cols[6]), lty = c(1,4,2,4,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.65)

box(col = "white")

mtext(1, text = "Phase Angle (Degrees)", col = "white", outer = TRUE, line = 1.5)

dev.off()
