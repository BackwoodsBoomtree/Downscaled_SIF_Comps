library(ncdf4)
library(terra)
library(viridis)

options(scipen = 999)

out_name   <- "G:/SIF_comps/figs/hotspot_tropics_black.pdf"

# March Equinox files
m_equ_files <- c("G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-12.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-13.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-14.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-15.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-16.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-17.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-18.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-19.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-20.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-21.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-22.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-23.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-25.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-26.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-27.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-28.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/03/TROPOSIF_L2B_2020-03-29.nc")
# Set Equinox files
s_equ_files <- c("G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-11.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-12.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-13.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-14.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-15.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-17.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-19.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-20.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-22.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-23.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-24.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-25.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-26.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-27.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-28.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-29.nc",
                 "G:/TROPOMI/esa/original/v2.1/l2b/2020/09/TROPOSIF_L2B_2020-09-30.nc")

# Veg masks (max NDVI > 0.40)
mask_ebf                 <- rast("G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif")
mask_ebf[mask_ebf < 80]  <- NA
mask_ebf[mask_ebf >= 80] <- 1

samerica_ext <- ext(c(-82,-34,-20,13))
africa_ext   <- ext(c(7,45,-5,5))
seasia_ext   <- ext(c(95,155,-11,10))

samerica_ebf <- crop(mask_ebf, samerica_ext)
africa_ebf   <- crop(mask_ebf, africa_ext)
seasia_ebf   <- crop(mask_ebf, seasia_ext)

samerica_ebf <- as.polygons(samerica_ebf, dissolve = TRUE)
africa_ebf   <- as.polygons(africa_ebf, dissolve = TRUE)
seasia_ebf   <- as.polygons(seasia_ebf, dissolve = TRUE)

# Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Grab data function
grab_data <- function(file) {
  
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
  
  return(df)
}

filter_region <- function(df, region) {
  if (region == "Amazonia") {
    df     <- subset(df, lat <= 13)
    df     <- subset(df, lat >= -20)
    df     <- subset(df, lon <= -34)
    df     <- subset(df, lon >= -82)
  } else if (region == "Africa") {
    df     <- subset(df, lat <= 5)
    df     <- subset(df, lat >= -5)
    df     <- subset(df, lon <= 45)
    df     <- subset(df, lon >= 7)
  } else if (region == "SEAsia") {
    df     <- subset(df, lat <= 10)
    df     <- subset(df, lat >= -11)
    df     <- subset(df, lon <= 155)
    df     <- subset(df, lon >= 95)
  } else if (region == "US") {
    df     <- subset(df, lat <= 45)
    df     <- subset(df, lat >= 40)
    df     <- subset(df, lon <= -89)
    df     <- subset(df, lon >= -100)
  } else {
    print("Region not found.")
  }
  
  return(df)
}

# Get data for March Equinox
for (i in 1:length(m_equ_files)) {
  
  print(paste0("Grabbing for March equinox:", m_equ_files[i]))
  df <- grab_data(m_equ_files[i])
  
  if (i == 1) {
    m_equ_df <- df
  } else {
    m_equ_df <- rbind(m_equ_df, df)
  }
  print(paste0("Added n records: ", nrow(df)))
}

# Get data for Sep Equinox
for (i in 1:length(s_equ_files)) {
  
  print(paste0("Grabbing for Sept equinox:", s_equ_files[i]))
  df <- grab_data(s_equ_files[i])
  
  if (i == 1) {
    s_equ_df <- df
  } else {
    s_equ_df <- rbind(s_equ_df, df)
  }
  print(paste0("Added n records: ", nrow(df)))
}

# Filter data by region
samerica_m_equ_df <- filter_region(m_equ_df, "Amazonia")
africa_m_equ_df    <- filter_region(m_equ_df, "Africa")
seasia_m_equ_df    <- filter_region(m_equ_df, "SEAsia")

samerica_s_equ_df <- filter_region(s_equ_df, "Amazonia")
africa_s_equ_df    <- filter_region(s_equ_df, "Africa")
seasia_s_equ_df    <- filter_region(s_equ_df, "SEAsia")

# Transform to SpatVector to mask by forest
samerica_m_equ_vect <- vect(samerica_m_equ_df, geom = c("lon", "lat"))
africa_m_equ_vect    <- vect(africa_m_equ_df, geom = c("lon", "lat"))
seasia_m_equ_vect    <- vect(seasia_m_equ_df, geom = c("lon", "lat"))

samerica_s_equ_vect <- vect(samerica_s_equ_df, geom = c("lon", "lat"))
africa_s_equ_vect    <- vect(africa_s_equ_df, geom = c("lon", "lat"))
seasia_s_equ_vect    <- vect(seasia_s_equ_df, geom = c("lon", "lat"))

# Mask by veg
samerica_m_equ_vect <- intersect(samerica_m_equ_vect, samerica_ebf)
africa_m_equ_vect    <- intersect(africa_m_equ_vect, africa_ebf)
seasia_m_equ_vect    <- intersect(seasia_m_equ_vect, seasia_ebf)

samerica_s_equ_vect <- intersect(samerica_s_equ_vect, samerica_ebf)
africa_s_equ_vect    <- intersect(africa_s_equ_vect, africa_ebf)
seasia_s_equ_vect    <- intersect(seasia_s_equ_vect, seasia_ebf)

# plot(samerica_m_equ_vect)
# plot(samerica_ebf, add = TRUE)
# plot(africa_s_equ_vect)
# plot(africa_ebf, add = TRUE)
# plot(seasia_s_equ_vect)
# plot(seasia_ebf, add = TRUE)

# Return to data.frame
samerica_m_equ_df <- as.data.frame(samerica_m_equ_vect)
africa_m_equ_df   <- as.data.frame(africa_m_equ_vect)
seasia_m_equ_df   <- as.data.frame(seasia_m_equ_vect)

samerica_s_equ_df <- as.data.frame(samerica_s_equ_vect)
africa_s_equ_df   <- as.data.frame(africa_s_equ_vect)
seasia_s_equ_df   <- as.data.frame(seasia_s_equ_vect)

### Remove outliers
#find absolute value of z-score for each value in each column
samerica_m_equ_z_scores <- samerica_m_equ_df
africa_m_equ_z_scores   <- africa_m_equ_df
seasia_m_equ_z_scores   <- seasia_m_equ_df

samerica_s_equ_z_scores <- samerica_s_equ_df
africa_s_equ_z_scores   <- africa_s_equ_df
seasia_s_equ_z_scores   <- seasia_s_equ_df

samerica_m_equ_z_scores <- as.data.frame(sapply(samerica_m_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
africa_m_equ_z_scores   <- as.data.frame(sapply(africa_m_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
seasia_m_equ_z_scores   <- as.data.frame(sapply(seasia_m_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))

samerica_s_equ_z_scores <- as.data.frame(sapply(samerica_s_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
africa_s_equ_z_scores   <- as.data.frame(sapply(africa_s_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
seasia_s_equ_z_scores   <- as.data.frame(sapply(seasia_s_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))

# keep only rows with all z-scores less than absolute value of 3 
samerica_m_equ_no_outliers <- samerica_m_equ_df[!rowSums(samerica_m_equ_z_scores[1:5]>3), ]
africa_m_equ_no_outliers   <- africa_m_equ_df[!rowSums(africa_m_equ_z_scores[1:5]>3), ]
seasia_m_equ_no_outliers   <- seasia_m_equ_df[!rowSums(seasia_m_equ_z_scores[1:5]>3), ]

samerica_s_equ_no_outliers <- samerica_s_equ_df[!rowSums(samerica_s_equ_z_scores[1:5]>3), ]
africa_s_equ_no_outliers   <- africa_s_equ_df[!rowSums(africa_s_equ_z_scores[1:5]>3), ]
seasia_s_equ_no_outliers   <- seasia_s_equ_df[!rowSums(seasia_s_equ_z_scores[1:5]>3), ]

# Normalize values
samerica_m_equ_norm <- as.data.frame(lapply(samerica_m_equ_no_outliers[1:5], min_max_norm))
africa_m_equ_norm   <- as.data.frame(lapply(africa_m_equ_no_outliers[1:5], min_max_norm))
seasia_m_equ_norm   <- as.data.frame(lapply(seasia_m_equ_no_outliers[1:5], min_max_norm))

samerica_s_equ_norm <- as.data.frame(lapply(samerica_s_equ_no_outliers[1:5], min_max_norm))
africa_s_equ_norm   <- as.data.frame(lapply(africa_s_equ_no_outliers[1:5], min_max_norm))
seasia_s_equ_norm   <- as.data.frame(lapply(seasia_s_equ_no_outliers[1:5], min_max_norm))

# Fit polynomials
samerica_m_equ_sif_fit    <- lm(samerica_m_equ_norm$sif    ~ samerica_m_equ_no_outliers$pa + I(samerica_m_equ_no_outliers$pa^2))
samerica_m_equ_nirv_fit   <- lm(samerica_m_equ_norm$nirv   ~ samerica_m_equ_no_outliers$pa + I(samerica_m_equ_no_outliers$pa^2))
samerica_m_equ_nirvr_fit  <- lm(samerica_m_equ_norm$nirvr  ~ samerica_m_equ_no_outliers$pa + I(samerica_m_equ_no_outliers$pa^2))
samerica_m_equ_ref665_fit <- lm(samerica_m_equ_norm$ref665 ~ samerica_m_equ_no_outliers$pa + I(samerica_m_equ_no_outliers$pa^2))
samerica_m_equ_ref781_fit <- lm(samerica_m_equ_norm$ref781 ~ samerica_m_equ_no_outliers$pa + I(samerica_m_equ_no_outliers$pa^2))

africa_m_equ_sif_fit    <- lm(africa_m_equ_norm$sif    ~ africa_m_equ_no_outliers$pa + I(africa_m_equ_no_outliers$pa^2))
africa_m_equ_nirv_fit   <- lm(africa_m_equ_norm$nirv   ~ africa_m_equ_no_outliers$pa + I(africa_m_equ_no_outliers$pa^2))
africa_m_equ_nirvr_fit  <- lm(africa_m_equ_norm$nirvr  ~ africa_m_equ_no_outliers$pa + I(africa_m_equ_no_outliers$pa^2))
africa_m_equ_ref665_fit <- lm(africa_m_equ_norm$ref665 ~ africa_m_equ_no_outliers$pa + I(africa_m_equ_no_outliers$pa^2))
africa_m_equ_ref781_fit <- lm(africa_m_equ_norm$ref781 ~ africa_m_equ_no_outliers$pa + I(africa_m_equ_no_outliers$pa^2))

seasia_m_equ_sif_fit    <- lm(seasia_m_equ_norm$sif    ~ seasia_m_equ_no_outliers$pa + I(seasia_m_equ_no_outliers$pa^2))
seasia_m_equ_nirv_fit   <- lm(seasia_m_equ_norm$nirv   ~ seasia_m_equ_no_outliers$pa + I(seasia_m_equ_no_outliers$pa^2))
seasia_m_equ_nirvr_fit  <- lm(seasia_m_equ_norm$nirvr  ~ seasia_m_equ_no_outliers$pa + I(seasia_m_equ_no_outliers$pa^2))
seasia_m_equ_ref665_fit <- lm(seasia_m_equ_norm$ref665 ~ seasia_m_equ_no_outliers$pa + I(seasia_m_equ_no_outliers$pa^2))
seasia_m_equ_ref781_fit <- lm(seasia_m_equ_norm$ref781 ~ seasia_m_equ_no_outliers$pa + I(seasia_m_equ_no_outliers$pa^2))

samerica_s_equ_sif_fit    <- lm(samerica_s_equ_norm$sif    ~ samerica_s_equ_no_outliers$pa + I(samerica_s_equ_no_outliers$pa^2))
samerica_s_equ_nirv_fit   <- lm(samerica_s_equ_norm$nirv   ~ samerica_s_equ_no_outliers$pa + I(samerica_s_equ_no_outliers$pa^2))
samerica_s_equ_nirvr_fit  <- lm(samerica_s_equ_norm$nirvr  ~ samerica_s_equ_no_outliers$pa + I(samerica_s_equ_no_outliers$pa^2))
samerica_s_equ_ref665_fit <- lm(samerica_s_equ_norm$ref665 ~ samerica_s_equ_no_outliers$pa + I(samerica_s_equ_no_outliers$pa^2))
samerica_s_equ_ref781_fit <- lm(samerica_s_equ_norm$ref781 ~ samerica_s_equ_no_outliers$pa + I(samerica_s_equ_no_outliers$pa^2))

africa_s_equ_sif_fit    <- lm(africa_s_equ_norm$sif    ~ africa_s_equ_no_outliers$pa + I(africa_s_equ_no_outliers$pa^2))
africa_s_equ_nirv_fit   <- lm(africa_s_equ_norm$nirv   ~ africa_s_equ_no_outliers$pa + I(africa_s_equ_no_outliers$pa^2))
africa_s_equ_nirvr_fit  <- lm(africa_s_equ_norm$nirvr  ~ africa_s_equ_no_outliers$pa + I(africa_s_equ_no_outliers$pa^2))
africa_s_equ_ref665_fit <- lm(africa_s_equ_norm$ref665 ~ africa_s_equ_no_outliers$pa + I(africa_s_equ_no_outliers$pa^2))
africa_s_equ_ref781_fit <- lm(africa_s_equ_norm$ref781 ~ africa_s_equ_no_outliers$pa + I(africa_s_equ_no_outliers$pa^2))

seasia_s_equ_sif_fit    <- lm(seasia_s_equ_norm$sif    ~ seasia_s_equ_no_outliers$pa + I(seasia_s_equ_no_outliers$pa^2))
seasia_s_equ_nirv_fit   <- lm(seasia_s_equ_norm$nirv   ~ seasia_s_equ_no_outliers$pa + I(seasia_s_equ_no_outliers$pa^2))
seasia_s_equ_nirvr_fit  <- lm(seasia_s_equ_norm$nirvr  ~ seasia_s_equ_no_outliers$pa + I(seasia_s_equ_no_outliers$pa^2))
seasia_s_equ_ref665_fit <- lm(seasia_s_equ_norm$ref665 ~ seasia_s_equ_no_outliers$pa + I(seasia_s_equ_no_outliers$pa^2))
seasia_s_equ_ref781_fit <- lm(seasia_s_equ_norm$ref781 ~ seasia_s_equ_no_outliers$pa + I(seasia_s_equ_no_outliers$pa^2))

# Predict all PAs
samerica_m_equ_pa_seq <- seq(0,max(samerica_m_equ_no_outliers$pa), length.out = 10000)
africa_m_equ_pa_seq   <- seq(0,max(africa_m_equ_no_outliers$pa), length.out = 10000)
seasia_m_equ_pa_seq   <- seq(0,max(seasia_m_equ_no_outliers$pa), length.out = 10000)

samerica_s_equ_pa_seq <- seq(0,max(samerica_s_equ_no_outliers$pa), length.out = 10000)
africa_s_equ_pa_seq   <- seq(0,max(africa_s_equ_no_outliers$pa), length.out = 10000)
seasia_s_equ_pa_seq   <- seq(0,max(seasia_s_equ_no_outliers$pa), length.out = 10000)

samerica_m_equ_pred <- data.frame(pa = samerica_m_equ_pa_seq)
africa_m_equ_pred   <- data.frame(pa = africa_m_equ_pa_seq)
seasia_m_equ_pred   <- data.frame(pa = seasia_m_equ_pa_seq)

samerica_s_equ_pred <- data.frame(pa = samerica_s_equ_pa_seq)
africa_s_equ_pred   <- data.frame(pa = africa_s_equ_pa_seq)
seasia_s_equ_pred   <- data.frame(pa = seasia_s_equ_pa_seq)

for (i in 1:nrow(samerica_m_equ_pred)) {
  samerica_m_equ_pred$sif[i]    <- samerica_m_equ_sif_fit$coefficients[1] + (samerica_m_equ_sif_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_sif_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$nirv[i]   <- samerica_m_equ_nirv_fit$coefficients[1] + (samerica_m_equ_nirv_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_nirv_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$nirvr[i]  <- samerica_m_equ_nirvr_fit$coefficients[1] + (samerica_m_equ_nirvr_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_nirvr_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$ref665[i] <- samerica_m_equ_ref665_fit$coefficients[1] + (samerica_m_equ_ref665_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_ref665_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$ref781[i] <- samerica_m_equ_ref781_fit$coefficients[1] + (samerica_m_equ_ref781_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_ref781_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
}

for (i in 1:nrow(africa_m_equ_pred)) {
  africa_m_equ_pred$sif[i]    <- africa_m_equ_sif_fit$coefficients[1] + (africa_m_equ_sif_fit$coefficients[2] * africa_m_equ_pred$pa[i]) + (africa_m_equ_sif_fit$coefficients[3] * (africa_m_equ_pred$pa[i]^2))
  africa_m_equ_pred$nirv[i]   <- africa_m_equ_nirv_fit$coefficients[1] + (africa_m_equ_nirv_fit$coefficients[2] * africa_m_equ_pred$pa[i]) + (africa_m_equ_nirv_fit$coefficients[3] * (africa_m_equ_pred$pa[i]^2))
  africa_m_equ_pred$nirvr[i]  <- africa_m_equ_nirvr_fit$coefficients[1] + (africa_m_equ_nirvr_fit$coefficients[2] * africa_m_equ_pred$pa[i]) + (africa_m_equ_nirvr_fit$coefficients[3] * (africa_m_equ_pred$pa[i]^2))
  africa_m_equ_pred$ref665[i] <- africa_m_equ_ref665_fit$coefficients[1] + (africa_m_equ_ref665_fit$coefficients[2] * africa_m_equ_pred$pa[i]) + (africa_m_equ_ref665_fit$coefficients[3] * (africa_m_equ_pred$pa[i]^2))
  africa_m_equ_pred$ref781[i] <- africa_m_equ_ref781_fit$coefficients[1] + (africa_m_equ_ref781_fit$coefficients[2] * africa_m_equ_pred$pa[i]) + (africa_m_equ_ref781_fit$coefficients[3] * (africa_m_equ_pred$pa[i]^2))
}

for (i in 1:nrow(seasia_m_equ_pred)) {
  seasia_m_equ_pred$sif[i]    <- seasia_m_equ_sif_fit$coefficients[1] + (seasia_m_equ_sif_fit$coefficients[2] * seasia_m_equ_pred$pa[i]) + (seasia_m_equ_sif_fit$coefficients[3] * (seasia_m_equ_pred$pa[i]^2))
  seasia_m_equ_pred$nirv[i]   <- seasia_m_equ_nirv_fit$coefficients[1] + (seasia_m_equ_nirv_fit$coefficients[2] * seasia_m_equ_pred$pa[i]) + (seasia_m_equ_nirv_fit$coefficients[3] * (seasia_m_equ_pred$pa[i]^2))
  seasia_m_equ_pred$nirvr[i]  <- seasia_m_equ_nirvr_fit$coefficients[1] + (seasia_m_equ_nirvr_fit$coefficients[2] * seasia_m_equ_pred$pa[i]) + (seasia_m_equ_nirvr_fit$coefficients[3] * (seasia_m_equ_pred$pa[i]^2))
  seasia_m_equ_pred$ref665[i] <- seasia_m_equ_ref665_fit$coefficients[1] + (seasia_m_equ_ref665_fit$coefficients[2] * seasia_m_equ_pred$pa[i]) + (seasia_m_equ_ref665_fit$coefficients[3] * (seasia_m_equ_pred$pa[i]^2))
  seasia_m_equ_pred$ref781[i] <- seasia_m_equ_ref781_fit$coefficients[1] + (seasia_m_equ_ref781_fit$coefficients[2] * seasia_m_equ_pred$pa[i]) + (seasia_m_equ_ref781_fit$coefficients[3] * (seasia_m_equ_pred$pa[i]^2))
}

for (i in 1:nrow(samerica_s_equ_pred)) {
  samerica_s_equ_pred$sif[i]    <- samerica_s_equ_sif_fit$coefficients[1] + (samerica_s_equ_sif_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_sif_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$nirv[i]   <- samerica_s_equ_nirv_fit$coefficients[1] + (samerica_s_equ_nirv_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_nirv_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$nirvr[i]  <- samerica_s_equ_nirvr_fit$coefficients[1] + (samerica_s_equ_nirvr_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_nirvr_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$ref665[i] <- samerica_s_equ_ref665_fit$coefficients[1] + (samerica_s_equ_ref665_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_ref665_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$ref781[i] <- samerica_s_equ_ref781_fit$coefficients[1] + (samerica_s_equ_ref781_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_ref781_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
}

for (i in 1:nrow(africa_s_equ_pred)) {
  africa_s_equ_pred$sif[i]    <- africa_s_equ_sif_fit$coefficients[1] + (africa_s_equ_sif_fit$coefficients[2] * africa_s_equ_pred$pa[i]) + (africa_s_equ_sif_fit$coefficients[3] * (africa_s_equ_pred$pa[i]^2))
  africa_s_equ_pred$nirv[i]   <- africa_s_equ_nirv_fit$coefficients[1] + (africa_s_equ_nirv_fit$coefficients[2] * africa_s_equ_pred$pa[i]) + (africa_s_equ_nirv_fit$coefficients[3] * (africa_s_equ_pred$pa[i]^2))
  africa_s_equ_pred$nirvr[i]  <- africa_s_equ_nirvr_fit$coefficients[1] + (africa_s_equ_nirvr_fit$coefficients[2] * africa_s_equ_pred$pa[i]) + (africa_s_equ_nirvr_fit$coefficients[3] * (africa_s_equ_pred$pa[i]^2))
  africa_s_equ_pred$ref665[i] <- africa_s_equ_ref665_fit$coefficients[1] + (africa_s_equ_ref665_fit$coefficients[2] * africa_s_equ_pred$pa[i]) + (africa_s_equ_ref665_fit$coefficients[3] * (africa_s_equ_pred$pa[i]^2))
  africa_s_equ_pred$ref781[i] <- africa_s_equ_ref781_fit$coefficients[1] + (africa_s_equ_ref781_fit$coefficients[2] * africa_s_equ_pred$pa[i]) + (africa_s_equ_ref781_fit$coefficients[3] * (africa_s_equ_pred$pa[i]^2))
}

for (i in 1:nrow(seasia_s_equ_pred)) {
  seasia_s_equ_pred$sif[i]    <- seasia_s_equ_sif_fit$coefficients[1] + (seasia_s_equ_sif_fit$coefficients[2] * seasia_s_equ_pred$pa[i]) + (seasia_s_equ_sif_fit$coefficients[3] * (seasia_s_equ_pred$pa[i]^2))
  seasia_s_equ_pred$nirv[i]   <- seasia_s_equ_nirv_fit$coefficients[1] + (seasia_s_equ_nirv_fit$coefficients[2] * seasia_s_equ_pred$pa[i]) + (seasia_s_equ_nirv_fit$coefficients[3] * (seasia_s_equ_pred$pa[i]^2))
  seasia_s_equ_pred$nirvr[i]  <- seasia_s_equ_nirvr_fit$coefficients[1] + (seasia_s_equ_nirvr_fit$coefficients[2] * seasia_s_equ_pred$pa[i]) + (seasia_s_equ_nirvr_fit$coefficients[3] * (seasia_s_equ_pred$pa[i]^2))
  seasia_s_equ_pred$ref665[i] <- seasia_s_equ_ref665_fit$coefficients[1] + (seasia_s_equ_ref665_fit$coefficients[2] * seasia_s_equ_pred$pa[i]) + (seasia_s_equ_ref665_fit$coefficients[3] * (seasia_s_equ_pred$pa[i]^2))
  seasia_s_equ_pred$ref781[i] <- seasia_s_equ_ref781_fit$coefficients[1] + (seasia_s_equ_ref781_fit$coefficients[2] * seasia_s_equ_pred$pa[i]) + (seasia_s_equ_ref781_fit$coefficients[3] * (seasia_s_equ_pred$pa[i]^2))
}

# get model coefficients
samerica_m_equ_sif_coeff    <- round(samerica_m_equ_sif_fit$coefficients, 4)
samerica_m_equ_nirv_coeff   <- round(samerica_m_equ_nirv_fit$coefficients, 4)
samerica_m_equ_nirvr_coeff  <- round(samerica_m_equ_nirvr_fit$coefficients, 4)
samerica_m_equ_ref665_coeff <- round(samerica_m_equ_ref665_fit$coefficients, 4)
samerica_m_equ_ref781_coeff <- round(samerica_m_equ_ref781_fit$coefficients, 4)

africa_m_equ_sif_coeff    <- round(africa_m_equ_sif_fit$coefficients, 4)
africa_m_equ_nirv_coeff   <- round(africa_m_equ_nirv_fit$coefficients, 4)
africa_m_equ_nirvr_coeff  <- round(africa_m_equ_nirvr_fit$coefficients, 4)
africa_m_equ_ref665_coeff <- round(africa_m_equ_ref665_fit$coefficients, 4)
africa_m_equ_ref781_coeff <- round(africa_m_equ_ref781_fit$coefficients, 4)

seasia_m_equ_sif_coeff    <- round(seasia_m_equ_sif_fit$coefficients, 4)
seasia_m_equ_nirv_coeff   <- round(seasia_m_equ_nirv_fit$coefficients, 4)
seasia_m_equ_nirvr_coeff  <- round(seasia_m_equ_nirvr_fit$coefficients, 4)
seasia_m_equ_ref665_coeff <- round(seasia_m_equ_ref665_fit$coefficients, 4)
seasia_m_equ_ref781_coeff <- round(seasia_m_equ_ref781_fit$coefficients, 4)

samerica_s_equ_sif_coeff    <- round(samerica_s_equ_sif_fit$coefficients, 4)
samerica_s_equ_nirv_coeff   <- round(samerica_s_equ_nirv_fit$coefficients, 4)
samerica_s_equ_nirvr_coeff  <- round(samerica_s_equ_nirvr_fit$coefficients, 4)
samerica_s_equ_ref665_coeff <- round(samerica_s_equ_ref665_fit$coefficients, 4)
samerica_s_equ_ref781_coeff <- round(samerica_s_equ_ref781_fit$coefficients, 4)

africa_s_equ_sif_coeff    <- round(africa_s_equ_sif_fit$coefficients, 4)
africa_s_equ_nirv_coeff   <- round(africa_s_equ_nirv_fit$coefficients, 4)
africa_s_equ_nirvr_coeff  <- round(africa_s_equ_nirvr_fit$coefficients, 4)
africa_s_equ_ref665_coeff <- round(africa_s_equ_ref665_fit$coefficients, 4)
africa_s_equ_ref781_coeff <- round(africa_s_equ_ref781_fit$coefficients, 4)

seasia_s_equ_sif_coeff    <- round(seasia_s_equ_sif_fit$coefficients, 4)
seasia_s_equ_nirv_coeff   <- round(seasia_s_equ_nirv_fit$coefficients, 4)
seasia_s_equ_nirvr_coeff  <- round(seasia_s_equ_nirvr_fit$coefficients, 4)
seasia_s_equ_ref665_coeff <- round(seasia_s_equ_ref665_fit$coefficients, 4)
seasia_s_equ_ref781_coeff <- round(seasia_s_equ_ref781_fit$coefficients, 4)

# Get R2
samerica_m_equ_sif_r2    <- round(summary(samerica_m_equ_sif_fit)$adj.r.squared, 2)
samerica_m_equ_nirv_r2   <- round(summary(samerica_m_equ_nirv_fit)$adj.r.squared, 2)
samerica_m_equ_nirvr_r2  <- round(summary(samerica_m_equ_nirvr_fit)$adj.r.squared, 2)
samerica_m_equ_ref665_r2 <- round(summary(samerica_m_equ_ref665_fit)$adj.r.squared, 2)
samerica_m_equ_ref781_r2 <- round(summary(samerica_m_equ_ref781_fit)$adj.r.squared, 2)

africa_m_equ_sif_r2    <- round(summary(africa_m_equ_sif_fit)$adj.r.squared, 2)
africa_m_equ_nirv_r2   <- round(summary(africa_m_equ_nirv_fit)$adj.r.squared, 2)
africa_m_equ_nirvr_r2  <- round(summary(africa_m_equ_nirvr_fit)$adj.r.squared, 2)
africa_m_equ_ref665_r2 <- round(summary(africa_m_equ_ref665_fit)$adj.r.squared, 2)
africa_m_equ_ref781_r2 <- round(summary(africa_m_equ_ref781_fit)$adj.r.squared, 2)

seasia_m_equ_sif_r2    <- round(summary(seasia_m_equ_sif_fit)$adj.r.squared, 2)
seasia_m_equ_nirv_r2   <- round(summary(seasia_m_equ_nirv_fit)$adj.r.squared, 2)
seasia_m_equ_nirvr_r2  <- round(summary(seasia_m_equ_nirvr_fit)$adj.r.squared, 2)
seasia_m_equ_ref665_r2 <- round(summary(seasia_m_equ_ref665_fit)$adj.r.squared, 2)
seasia_m_equ_ref781_r2 <- round(summary(seasia_m_equ_ref781_fit)$adj.r.squared, 2)

samerica_s_equ_sif_r2    <- round(summary(samerica_s_equ_sif_fit)$adj.r.squared, 2)
samerica_s_equ_nirv_r2   <- round(summary(samerica_s_equ_nirv_fit)$adj.r.squared, 2)
samerica_s_equ_nirvr_r2  <- round(summary(samerica_s_equ_nirvr_fit)$adj.r.squared, 2)
samerica_s_equ_ref665_r2 <- round(summary(samerica_s_equ_ref665_fit)$adj.r.squared, 2)
samerica_s_equ_ref781_r2 <- round(summary(samerica_s_equ_ref781_fit)$adj.r.squared, 2)

africa_s_equ_sif_r2    <- round(summary(africa_s_equ_sif_fit)$adj.r.squared, 2)
africa_s_equ_nirv_r2   <- round(summary(africa_s_equ_nirv_fit)$adj.r.squared, 2)
africa_s_equ_nirvr_r2  <- round(summary(africa_s_equ_nirvr_fit)$adj.r.squared, 2)
africa_s_equ_ref665_r2 <- round(summary(africa_s_equ_ref665_fit)$adj.r.squared, 2)
africa_s_equ_ref781_r2 <- round(summary(africa_s_equ_ref781_fit)$adj.r.squared, 2)

seasia_s_equ_sif_r2    <- round(summary(seasia_s_equ_sif_fit)$adj.r.squared, 2)
seasia_s_equ_nirv_r2   <- round(summary(seasia_s_equ_nirv_fit)$adj.r.squared, 2)
seasia_s_equ_nirvr_r2  <- round(summary(seasia_s_equ_nirvr_fit)$adj.r.squared, 2)
seasia_s_equ_ref665_r2 <- round(summary(seasia_s_equ_ref665_fit)$adj.r.squared, 2)
seasia_s_equ_ref781_r2 <- round(summary(seasia_s_equ_ref781_fit)$adj.r.squared, 2)


# Eqs for legend

# samerica_m_equ_sif_eq    <- bquote(SIF~" = "~.(samerica_m_equ_sif_coeff[[1]])*.(samerica_m_equ_sif_coeff[[2]])*x*"+"*.(samerica_m_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_sif_r2))
# samerica_m_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(samerica_m_equ_nirv_coeff[[1]])*.(samerica_m_equ_nirv_coeff[[2]])*x*"+"*.(samerica_m_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_nirv_r2))
# samerica_m_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(samerica_m_equ_nirvr_coeff[[1]])*.(samerica_m_equ_nirvr_coeff[[2]])*x*"+"*.(samerica_m_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_nirvr_r2))
# samerica_m_equ_ref665_eq <- bquote(Red~" = "~.(samerica_m_equ_ref665_coeff[[1]])*.(samerica_m_equ_ref665_coeff[[2]])*x*"+"*.(samerica_m_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_ref665_r2))
# samerica_m_equ_ref781_eq <- bquote(NIR~" = "~.(samerica_m_equ_ref781_coeff[[1]])*.(samerica_m_equ_ref781_coeff[[2]])*x*"+"*.(samerica_m_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_ref781_r2))
# 
# africa_m_equ_sif_eq    <- bquote(SIF~" = "~.(africa_m_equ_sif_coeff[[1]])*.(africa_m_equ_sif_coeff[[2]])*x*"+"*.(africa_m_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_m_equ_sif_r2))
# africa_m_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(africa_m_equ_nirv_coeff[[1]])*.(africa_m_equ_nirv_coeff[[2]])*x*"+"*.(africa_m_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_m_equ_nirv_r2))
# africa_m_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(africa_m_equ_nirvr_coeff[[1]])*.(africa_m_equ_nirvr_coeff[[2]])*x*"+"*.(africa_m_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_m_equ_nirvr_r2))
# africa_m_equ_ref665_eq <- bquote(Red~" = "~.(africa_m_equ_ref665_coeff[[1]])*.(africa_m_equ_ref665_coeff[[2]])*x*"+"*.(africa_m_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_m_equ_ref665_r2))
# africa_m_equ_ref781_eq <- bquote(NIR~" = "~.(africa_m_equ_ref781_coeff[[1]])*.(africa_m_equ_ref781_coeff[[2]])*x*"+"*.(africa_m_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_m_equ_ref781_r2))
# 
# seasia_m_equ_sif_eq    <- bquote(SIF~" = "~.(seasia_m_equ_sif_coeff[[1]])*.(seasia_m_equ_sif_coeff[[2]])*x*"+"*.(seasia_m_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_m_equ_sif_r2))
# seasia_m_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(seasia_m_equ_nirv_coeff[[1]])*.(seasia_m_equ_nirv_coeff[[2]])*x*"+"*.(seasia_m_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_m_equ_nirv_r2))
# seasia_m_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(seasia_m_equ_nirvr_coeff[[1]])*.(seasia_m_equ_nirvr_coeff[[2]])*x*"+"*.(seasia_m_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_m_equ_nirvr_r2))
# seasia_m_equ_ref665_eq <- bquote(Red~" = "~.(seasia_m_equ_ref665_coeff[[1]])*.(seasia_m_equ_ref665_coeff[[2]])*x*"+"*.(seasia_m_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_m_equ_ref665_r2))
# seasia_m_equ_ref781_eq <- bquote(NIR~" = "~.(seasia_m_equ_ref781_coeff[[1]])*.(seasia_m_equ_ref781_coeff[[2]])*x*"+"*.(seasia_m_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_m_equ_ref781_r2))
# 
# samerica_s_equ_sif_eq    <- bquote(SIF~" = "~.(samerica_s_equ_sif_coeff[[1]])*.(samerica_s_equ_sif_coeff[[2]])*x*"+"*.(samerica_s_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_sif_r2))
# samerica_s_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(samerica_s_equ_nirv_coeff[[1]])*.(samerica_s_equ_nirv_coeff[[2]])*x*"+"*.(samerica_s_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_nirv_r2))
# samerica_s_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(samerica_s_equ_nirvr_coeff[[1]])*.(samerica_s_equ_nirvr_coeff[[2]])*x*"+"*.(samerica_s_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_nirvr_r2))
# samerica_s_equ_ref665_eq <- bquote(Red~" = "~.(samerica_s_equ_ref665_coeff[[1]])*.(samerica_s_equ_ref665_coeff[[2]])*x*"+"*.(samerica_s_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_ref665_r2))
# samerica_s_equ_ref781_eq <- bquote(NIR~" = "~.(samerica_s_equ_ref781_coeff[[1]])*.(samerica_s_equ_ref781_coeff[[2]])*x*"+"*.(samerica_s_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_ref781_r2))
# 
# africa_s_equ_sif_eq    <- bquote(SIF~" = "~.(africa_s_equ_sif_coeff[[1]])*.(africa_s_equ_sif_coeff[[2]])*x*"+"*.(africa_s_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_s_equ_sif_r2))
# africa_s_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(africa_s_equ_nirv_coeff[[1]])*.(africa_s_equ_nirv_coeff[[2]])*x*"+"*.(africa_s_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_s_equ_nirv_r2))
# africa_s_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(africa_s_equ_nirvr_coeff[[1]])*.(africa_s_equ_nirvr_coeff[[2]])*x*"+"*.(africa_s_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_s_equ_nirvr_r2))
# africa_s_equ_ref665_eq <- bquote(Red~" = "~.(africa_s_equ_ref665_coeff[[1]])*.(africa_s_equ_ref665_coeff[[2]])*x*"+"*.(africa_s_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_s_equ_ref665_r2))
# africa_s_equ_ref781_eq <- bquote(NIR~" = "~.(africa_s_equ_ref781_coeff[[1]])*.(africa_s_equ_ref781_coeff[[2]])*x*"+"*.(africa_s_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(africa_s_equ_ref781_r2))
# 
# seasia_s_equ_sif_eq    <- bquote(SIF~" = "~.(seasia_s_equ_sif_coeff[[1]])*.(seasia_s_equ_sif_coeff[[2]])*x*"+"*.(seasia_s_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_s_equ_sif_r2))
# seasia_s_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(seasia_s_equ_nirv_coeff[[1]])*.(seasia_s_equ_nirv_coeff[[2]])*x*"+"*.(seasia_s_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_s_equ_nirv_r2))
# seasia_s_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(seasia_s_equ_nirvr_coeff[[1]])*.(seasia_s_equ_nirvr_coeff[[2]])*x*"+"*.(seasia_s_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_s_equ_nirvr_r2))
# seasia_s_equ_ref665_eq <- bquote(Red~" = "~.(seasia_s_equ_ref665_coeff[[1]])*.(seasia_s_equ_ref665_coeff[[2]])*x*"+"*.(seasia_s_equ_ref665_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_s_equ_ref665_r2))
# seasia_s_equ_ref781_eq <- bquote(NIR~" = "~.(seasia_s_equ_ref781_coeff[[1]])*.(seasia_s_equ_ref781_coeff[[2]])*x*"+"*.(seasia_s_equ_ref781_coeff[[3]])*x^2*"; "~R^2*"="*.(seasia_s_equ_ref781_r2))

samerica_m_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(samerica_m_equ_sif_r2))
samerica_m_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(samerica_m_equ_nirv_r2))
samerica_m_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(samerica_m_equ_nirvr_r2))
samerica_m_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(samerica_m_equ_ref665_r2))
samerica_m_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(samerica_m_equ_ref781_r2))

africa_m_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(africa_m_equ_sif_r2))
africa_m_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(africa_m_equ_nirv_r2))
africa_m_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(africa_m_equ_nirvr_r2))
africa_m_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(africa_m_equ_ref665_r2))
africa_m_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(africa_m_equ_ref781_r2))

seasia_m_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(seasia_m_equ_sif_r2))
seasia_m_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(seasia_m_equ_nirv_r2))
seasia_m_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(seasia_m_equ_nirvr_r2))
seasia_m_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(seasia_m_equ_ref665_r2))
seasia_m_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(seasia_m_equ_ref781_r2))

samerica_s_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(samerica_s_equ_sif_r2))
samerica_s_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(samerica_s_equ_nirv_r2))
samerica_s_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(samerica_s_equ_nirvr_r2))
samerica_s_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(samerica_s_equ_ref665_r2))
samerica_s_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(samerica_s_equ_ref781_r2))

africa_s_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(africa_s_equ_sif_r2))
africa_s_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(africa_s_equ_nirv_r2))
africa_s_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(africa_s_equ_nirvr_r2))
africa_s_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(africa_s_equ_ref665_r2))
africa_s_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(africa_s_equ_ref781_r2))

seasia_s_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(seasia_s_equ_sif_r2))
seasia_s_equ_nirv_eq   <- bquote(NIRv~Ref*"; "~R^2~"="~.(seasia_s_equ_nirv_r2))
seasia_s_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(seasia_s_equ_nirvr_r2))
seasia_s_equ_ref665_eq <- bquote(Red*"; "~R^2~"="~.(seasia_s_equ_ref665_r2))
seasia_s_equ_ref781_eq <- bquote(NIR*"; "~R^2~"="~.(seasia_s_equ_ref781_r2))


#### Plot ####
mag.cols <- magma(7)
vir.cols <- viridis(7)

cairo_pdf(out_name, width = 7.5, height = 6.25)

par(mfrow = c(3, 2), oma=c(3.0,2.75,0,0.1), bg = "black")

### SAMERICA ###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "S America Tropical Forest March Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(samerica_m_equ_sif_eq), as.expression(samerica_m_equ_nirv_eq),
                              as.expression(samerica_m_equ_nirvr_eq), as.expression(samerica_m_equ_ref665_eq),
                              as.expression(samerica_m_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")

###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "S America Tropical Forest September Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, labels = FALSE, col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(samerica_s_equ_sif_eq), as.expression(samerica_s_equ_nirv_eq),
                              as.expression(samerica_s_equ_nirvr_eq), as.expression(samerica_s_equ_ref665_eq),
                              as.expression(samerica_s_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")


### AFRICA ###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "Africa Tropical Forest March Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(africa_m_equ_pred$pa, africa_m_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(africa_m_equ_pred$pa, africa_m_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(africa_m_equ_pred$pa, africa_m_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(africa_m_equ_pred$pa, africa_m_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(africa_m_equ_pred$pa, africa_m_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(africa_m_equ_sif_eq), as.expression(africa_m_equ_nirv_eq),
                              as.expression(africa_m_equ_nirvr_eq), as.expression(africa_m_equ_ref665_eq),
                              as.expression(africa_m_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")

###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "Africa Tropical Forest September Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(africa_s_equ_pred$pa, africa_s_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(africa_s_equ_pred$pa, africa_s_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(africa_s_equ_pred$pa, africa_s_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(africa_s_equ_pred$pa, africa_s_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(africa_s_equ_pred$pa, africa_s_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, labels = FALSE, col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(africa_s_equ_sif_eq), as.expression(africa_s_equ_nirv_eq),
                              as.expression(africa_s_equ_nirvr_eq), as.expression(africa_s_equ_ref665_eq),
                              as.expression(africa_s_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")


### SEASIA ###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "SE Asia Tropical Forest March Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(seasia_m_equ_pred$pa, seasia_m_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(seasia_m_equ_pred$pa, seasia_m_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(seasia_m_equ_pred$pa, seasia_m_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(seasia_m_equ_pred$pa, seasia_m_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(seasia_m_equ_pred$pa, seasia_m_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(seasia_m_equ_sif_eq), as.expression(seasia_m_equ_nirv_eq),
                              as.expression(seasia_m_equ_nirvr_eq), as.expression(seasia_m_equ_ref665_eq),
                              as.expression(seasia_m_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")

###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
mtext(3, text = "SE Asia Tropical Forest September Equinox", col = "white")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(seasia_s_equ_pred$pa, seasia_s_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(seasia_s_equ_pred$pa, seasia_s_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(seasia_s_equ_pred$pa, seasia_s_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(seasia_s_equ_pred$pa, seasia_s_equ_pred$ref665, col=vir.cols[6], lwd = 2)
lines(seasia_s_equ_pred$pa, seasia_s_equ_pred$ref781, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.06, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "white", col = "white")
axis(2, tck = 0.06, labels = FALSE, col.axis = "white", col = "white", las = 2)
axis(2, tck = 0.03, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "white", col = "white")

legend("topright", legend = c(as.expression(seasia_s_equ_sif_eq), as.expression(seasia_s_equ_nirv_eq),
                              as.expression(seasia_s_equ_nirvr_eq), as.expression(seasia_s_equ_ref665_eq),
                              as.expression(seasia_s_equ_ref781_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85)

box(col = "white")

###

mtext(1, text = "Phase Angle (Degrees)", col = "white", outer = TRUE, line = 1.5)
mtext(2, text = "Normalized Value", col = "white", outer = TRUE, line = 1.5)

dev.off()
