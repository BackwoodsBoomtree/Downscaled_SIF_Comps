library(viridis)
library(ncdf4)

options(scipen = 999)

out_name   <- "G:/SIF_comps/figs/v2/hotspot_amazon_outliers_v2.pdf"

#### FILE LISTS ####
# Amazon
# March Equinox files
m_amazon_files <- c("G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-12.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-13.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-14.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-15.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-16.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-17.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-18.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-19.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-20.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-21.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-22.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-23.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-25.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-26.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-27.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-28.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-03-29.nc")
# Sep Equinox files
s_amazon_files <- c("G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-11.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-12.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-13.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-14.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-15.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-17.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-19.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-20.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-22.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-23.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-24.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-25.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-26.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-27.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-28.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-29.nc",
                 "G:/TROPOMI/esa/extracted/ebf/amazon/Amazon_TROPOSIF_L2B_2020-09-30.nc")


#### GET DATA ####

# Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Grab data function
grab_data <- function(file_list) {
  
  for (i in 1:length(file_list)) {
    
    # Open data
    data <- nc_open(file_list[i])
    
    # Get variables
    sif     <- ncvar_get(data, "SIF_743")
    nirv    <- ncvar_get(data, "NIRv")
    nirvr   <- ncvar_get(data, "NIRv_RAD")
    red     <- ncvar_get(data, "RED")
    nir     <- ncvar_get(data, "NIR")
    pa      <- ncvar_get(data, "phase_angle")
    cf      <- ncvar_get(data, "cloud_fraction_L2")
    lc_perc <- ncvar_get(data, "LC_PERC_2020")
    
    nc_close(data)
    
    # Create DF
    df     <- data.frame(sif, nirv, nirvr, red, nir, pa, cf, lc_perc)
    
    # Set phase angle to absolute value
    df$pa  <- abs(df$pa)
    
    # Subset for clear sky (cloud fraction == 0)
    df     <- subset(df, cf == 0)
    
    # Subset for 90% cover
    df     <- subset(df, lc_perc >= 90)
    
    if (i == 1) {
      equ_df <- df
    } else {
      equ_df <- rbind(equ_df, df)
    }
    print(paste0("Added n records: ", nrow(df)))
  }
  return(equ_df)
}

# Grab data
samerica_m_equ_df <- grab_data(m_amazon_files)
samerica_s_equ_df <- grab_data(s_amazon_files)


# ### Remove outliers
# #find absolute value of z-score for each value in each column
# samerica_m_equ_z_scores <- samerica_m_equ_df
# 
# samerica_s_equ_z_scores <- samerica_s_equ_df
# samerica_m_equ_z_scores <- as.data.frame(sapply(samerica_m_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
# 
# samerica_s_equ_z_scores <- as.data.frame(sapply(samerica_s_equ_z_scores[1:5], function(df) (abs(df-mean(df))/sd(df))))
# 
# # keep only rows with all z-scores less than absolute value of 3 
# samerica_m_equ_no_outliers <- samerica_m_equ_df[!rowSums(samerica_m_equ_z_scores[1:5]>3), ]
# 
# samerica_s_equ_no_outliers <- samerica_s_equ_df[!rowSums(samerica_s_equ_z_scores[1:5]>3), ]

# Normalize values
samerica_m_equ_norm <- as.data.frame(lapply(samerica_m_equ_df[1:5], min_max_norm))
samerica_s_equ_norm <- as.data.frame(lapply(samerica_s_equ_df[1:5], min_max_norm))

# Fit polynomials
samerica_m_equ_sif_fit    <- lm(samerica_m_equ_norm$sif    ~ samerica_m_equ_df$pa + I(samerica_m_equ_df$pa^2))
samerica_m_equ_nirv_fit   <- lm(samerica_m_equ_norm$nirv   ~ samerica_m_equ_df$pa + I(samerica_m_equ_df$pa^2))
samerica_m_equ_nirvr_fit  <- lm(samerica_m_equ_norm$nirvr  ~ samerica_m_equ_df$pa + I(samerica_m_equ_df$pa^2))
samerica_m_equ_red_fit <- lm(samerica_m_equ_norm$red ~ samerica_m_equ_df$pa + I(samerica_m_equ_df$pa^2))
samerica_m_equ_nir_fit <- lm(samerica_m_equ_norm$nir ~ samerica_m_equ_df$pa + I(samerica_m_equ_df$pa^2))

samerica_s_equ_sif_fit    <- lm(samerica_s_equ_norm$sif    ~ samerica_s_equ_df$pa + I(samerica_s_equ_df$pa^2))
samerica_s_equ_nirv_fit   <- lm(samerica_s_equ_norm$nirv   ~ samerica_s_equ_df$pa + I(samerica_s_equ_df$pa^2))
samerica_s_equ_nirvr_fit  <- lm(samerica_s_equ_norm$nirvr  ~ samerica_s_equ_df$pa + I(samerica_s_equ_df$pa^2))
samerica_s_equ_red_fit <- lm(samerica_s_equ_norm$red ~ samerica_s_equ_df$pa + I(samerica_s_equ_df$pa^2))
samerica_s_equ_nir_fit <- lm(samerica_s_equ_norm$nir ~ samerica_s_equ_df$pa + I(samerica_s_equ_df$pa^2))

# Predict all PAs
samerica_m_equ_pa_seq <- seq(0,max(samerica_m_equ_df$pa), length.out = 10000)
samerica_s_equ_pa_seq <- seq(0,max(samerica_s_equ_df$pa), length.out = 10000)
samerica_m_equ_pred <- data.frame(pa = samerica_m_equ_pa_seq)
samerica_s_equ_pred <- data.frame(pa = samerica_s_equ_pa_seq)

for (i in 1:nrow(samerica_m_equ_pred)) {
  samerica_m_equ_pred$sif[i]   <- samerica_m_equ_sif_fit$coefficients[1] + (samerica_m_equ_sif_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_sif_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$nirv[i]  <- samerica_m_equ_nirv_fit$coefficients[1] + (samerica_m_equ_nirv_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_nirv_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$nirvr[i] <- samerica_m_equ_nirvr_fit$coefficients[1] + (samerica_m_equ_nirvr_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_nirvr_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$red[i]   <- samerica_m_equ_red_fit$coefficients[1] + (samerica_m_equ_red_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_red_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
  samerica_m_equ_pred$nir[i]   <- samerica_m_equ_nir_fit$coefficients[1] + (samerica_m_equ_nir_fit$coefficients[2] * samerica_m_equ_pred$pa[i]) + (samerica_m_equ_nir_fit$coefficients[3] * (samerica_m_equ_pred$pa[i]^2))
}

for (i in 1:nrow(samerica_s_equ_pred)) {
  samerica_s_equ_pred$sif[i]   <- samerica_s_equ_sif_fit$coefficients[1] + (samerica_s_equ_sif_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_sif_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$nirv[i]  <- samerica_s_equ_nirv_fit$coefficients[1] + (samerica_s_equ_nirv_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_nirv_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$nirvr[i] <- samerica_s_equ_nirvr_fit$coefficients[1] + (samerica_s_equ_nirvr_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_nirvr_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$red[i]   <- samerica_s_equ_red_fit$coefficients[1] + (samerica_s_equ_red_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_red_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
  samerica_s_equ_pred$nir[i]   <- samerica_s_equ_nir_fit$coefficients[1] + (samerica_s_equ_nir_fit$coefficients[2] * samerica_s_equ_pred$pa[i]) + (samerica_s_equ_nir_fit$coefficients[3] * (samerica_s_equ_pred$pa[i]^2))
}

# get model coefficients
samerica_m_equ_sif_coeff   <- round(samerica_m_equ_sif_fit$coefficients, 4)
samerica_m_equ_nirv_coeff  <- round(samerica_m_equ_nirv_fit$coefficients, 4)
samerica_m_equ_nirvr_coeff <- round(samerica_m_equ_nirvr_fit$coefficients, 4)
samerica_m_equ_red_coeff   <- round(samerica_m_equ_red_fit$coefficients, 4)
samerica_m_equ_nir_coeff   <- round(samerica_m_equ_nir_fit$coefficients, 4)

samerica_s_equ_sif_coeff   <- round(samerica_s_equ_sif_fit$coefficients, 4)
samerica_s_equ_nirv_coeff  <- round(samerica_s_equ_nirv_fit$coefficients, 4)
samerica_s_equ_nirvr_coeff <- round(samerica_s_equ_nirvr_fit$coefficients, 4)
samerica_s_equ_red_coeff   <- round(samerica_s_equ_red_fit$coefficients, 4)
samerica_s_equ_nir_coeff   <- round(samerica_s_equ_nir_fit$coefficients, 4)

# Get R2
samerica_m_equ_sif_r2   <- round(summary(samerica_m_equ_sif_fit)$adj.r.squared, 2)
samerica_m_equ_nirv_r2  <- round(summary(samerica_m_equ_nirv_fit)$adj.r.squared, 2)
samerica_m_equ_nirvr_r2 <- round(summary(samerica_m_equ_nirvr_fit)$adj.r.squared, 2)
samerica_m_equ_red_r2   <- round(summary(samerica_m_equ_red_fit)$adj.r.squared, 2)
samerica_m_equ_nir_r2   <- round(summary(samerica_m_equ_nir_fit)$adj.r.squared, 2)

samerica_s_equ_sif_r2   <- round(summary(samerica_s_equ_sif_fit)$adj.r.squared, 2)
samerica_s_equ_nirv_r2  <- round(summary(samerica_s_equ_nirv_fit)$adj.r.squared, 2)
samerica_s_equ_nirvr_r2 <- round(summary(samerica_s_equ_nirvr_fit)$adj.r.squared, 2)
samerica_s_equ_red_r2   <- round(summary(samerica_s_equ_red_fit)$adj.r.squared, 2)
samerica_s_equ_nir_r2   <- round(summary(samerica_s_equ_nir_fit)$adj.r.squared, 2)


# Eqs for legend

# samerica_m_equ_sif_eq    <- bquote(SIF~" = "~.(samerica_m_equ_sif_coeff[[1]])*.(samerica_m_equ_sif_coeff[[2]])*x*"+"*.(samerica_m_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_sif_r2))
# samerica_m_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(samerica_m_equ_nirv_coeff[[1]])*.(samerica_m_equ_nirv_coeff[[2]])*x*"+"*.(samerica_m_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_nirv_r2))
# samerica_m_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(samerica_m_equ_nirvr_coeff[[1]])*.(samerica_m_equ_nirvr_coeff[[2]])*x*"+"*.(samerica_m_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_nirvr_r2))
# samerica_m_equ_red_eq <- bquote(Red~" = "~.(samerica_m_equ_red_coeff[[1]])*.(samerica_m_equ_red_coeff[[2]])*x*"+"*.(samerica_m_equ_red_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_red_r2))
# samerica_m_equ_nir_eq <- bquote(NIR~" = "~.(samerica_m_equ_nir_coeff[[1]])*.(samerica_m_equ_nir_coeff[[2]])*x*"+"*.(samerica_m_equ_nir_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_m_equ_nir_r2))

# samerica_s_equ_sif_eq    <- bquote(SIF~" = "~.(samerica_s_equ_sif_coeff[[1]])*.(samerica_s_equ_sif_coeff[[2]])*x*"+"*.(samerica_s_equ_sif_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_sif_r2))
# samerica_s_equ_nirv_eq   <- bquote(NIRv~Ref~" = "~.(samerica_s_equ_nirv_coeff[[1]])*.(samerica_s_equ_nirv_coeff[[2]])*x*"+"*.(samerica_s_equ_nirv_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_nirv_r2))
# samerica_s_equ_nirvr_eq  <- bquote(NIRv~Rad~" = "~.(samerica_s_equ_nirvr_coeff[[1]])*.(samerica_s_equ_nirvr_coeff[[2]])*x*"+"*.(samerica_s_equ_nirvr_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_nirvr_r2))
# samerica_s_equ_red_eq <- bquote(Red~" = "~.(samerica_s_equ_red_coeff[[1]])*.(samerica_s_equ_red_coeff[[2]])*x*"+"*.(samerica_s_equ_red_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_red_r2))
# samerica_s_equ_nir_eq <- bquote(NIR~" = "~.(samerica_s_equ_nir_coeff[[1]])*.(samerica_s_equ_nir_coeff[[2]])*x*"+"*.(samerica_s_equ_nir_coeff[[3]])*x^2*"; "~R^2*"="*.(samerica_s_equ_nir_r2))

samerica_m_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(samerica_m_equ_sif_r2))
samerica_m_equ_nirv_eq   <- bquote(NIRv*"; "~R^2~"="~.(samerica_m_equ_nirv_r2))
samerica_m_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(samerica_m_equ_nirvr_r2))
samerica_m_equ_red_eq    <- bquote(Red*"; "~R^2~"="~.(samerica_m_equ_red_r2))
samerica_m_equ_nir_eq    <- bquote(NIR*"; "~R^2~"="~.(samerica_m_equ_nir_r2))

samerica_s_equ_sif_eq    <- bquote(SIF*"; "~R^2~"="~.(samerica_s_equ_sif_r2))
samerica_s_equ_nirv_eq   <- bquote(NIRv*"; "~R^2~"="~.(samerica_s_equ_nirv_r2))
samerica_s_equ_nirvr_eq  <- bquote(NIRv~Rad*"; "~R^2~"="~.(samerica_s_equ_nirvr_r2))
samerica_s_equ_red_eq    <- bquote(Red*"; "~R^2~"="~.(samerica_s_equ_red_r2))
samerica_s_equ_nir_eq    <- bquote(NIR*"; "~R^2~"="~.(samerica_s_equ_nir_r2))


#### Plot ####
mag.cols <- magma(7)
vir.cols <- viridis(7)

cairo_pdf(out_name, width = 11.5, height = 4.25)

par(mfrow = c(1, 2), oma=c(3.0,2.75,0,0.1))

### SAMERICA ###
op <- par(mar = c(0,0.5,2,0.5))

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "black")
mtext(3, text = "Amazon Tropical Forest March Equinox")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$red, col=vir.cols[6], lwd = 2)
lines(samerica_m_equ_pred$pa, samerica_m_equ_pred$nir, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.04, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "black", col = "gray50")
axis(1, tck = 0.02, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "black", col = "gray50")
axis(2, tck = 0.04, mgp=c(3, 0.2, 0), col.axis = "black", col = "gray50", las = 2)
axis(2, tck = 0.02, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "black", col = "gray50")

legend("topright", legend = c(as.expression(samerica_m_equ_sif_eq), as.expression(samerica_m_equ_nirv_eq),
                              as.expression(samerica_m_equ_nirvr_eq), as.expression(samerica_m_equ_red_eq),
                              as.expression(samerica_m_equ_nir_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85, bg = "black")

box()

###
op <- par(mar = c(0,0.5,2,0.5), bg = "black")

plot(NULL, xlim = c(0,70), ylim = c(0, 1), axes = FALSE, xaxs="i")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "black")
mtext(3, text = "Amazon Tropical Forest September Equinox")

# Shaded area
rect(-10, -10, 20, 100, col = rgb(0.30,0.30,0.30), border = NA)

# Add polynomial curves
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$sif, col=mag.cols[4], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$nirv, col=mag.cols[5], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$nirvr, col=mag.cols[6], lwd = 2, lty = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$red, col=vir.cols[6], lwd = 2)
lines(samerica_s_equ_pred$pa, samerica_s_equ_pred$nir, col=vir.cols[5], lwd = 2, lty = 2)

axis(1, tck = 0.04, mgp=c(3, 0.2, 0), labels = TRUE, at = seq(0, 70, by = 10), col.axis = "black", col = "gray50")
axis(1, tck = 0.02, labels = FALSE, at = seq(5, 65, by = 10), col.axis = "black", col = "gray50")
axis(2, tck = 0.04, labels = FALSE, mgp=c(3, 0.2, 0), col.axis = "black", col = "gray50", las = 2)
axis(2, tck = 0.02, labels = FALSE, at = seq(0.1, 0.9, by = 0.2), col.axis = "black", col = "gray50")

legend("topright", legend = c(as.expression(samerica_s_equ_sif_eq), as.expression(samerica_s_equ_nirv_eq),
                              as.expression(samerica_s_equ_nirvr_eq), as.expression(samerica_s_equ_red_eq),
                              as.expression(samerica_s_equ_nir_eq)),
       col = c(mag.cols[4],mag.cols[5],mag.cols[6],vir.cols[6],vir.cols[5]), lty = c(1,1,2,1,2), lwd = c(2,2,2,2,2),
       box.col = "white", text.col = "white", cex = 0.85, bg = "black")

box()

###

mtext(1, text = "Phase Angle (Degrees)", outer = TRUE, line = 1.5)
mtext(2, text = "Normalized Value", outer = TRUE, line = 1.5)

dev.off()
