library(terra)
library(ncdf4)
library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/comps_ec_NIRV_mod_white_v2.pdf"

# Data from Wu paper

k34_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_PC.csv", header = FALSE)[,2] / 10 # my axis when extracting the data was off by a decimal place
k67_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_PC.csv", header = FALSE)[,2]
rja_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_PC.csv", header = FALSE)[,2]
cax_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_PC.csv", header = FALSE)[,2]

k34_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_GEP.csv", header = FALSE)[,2]
k67_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_GEP.csv", header = FALSE)[,2]
rja_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_GEP.csv", header = FALSE)[,2]
cax_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_GEP.csv", header = FALSE)[,2]

k34_pre <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_precip.csv", header = FALSE)[,2]
k67_pre <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_precip.csv", header = FALSE)[,2]
rja_pre <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_precip.csv", header = FALSE)[,2]
cax_pre <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_precip.csv", header = FALSE)[,2]

k34_wu_lai <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_LAI.csv", header = FALSE)[,2]
k67_wu_lai <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_LAI.csv", header = FALSE)[,2]

k34_wu_evi <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_EVI.csv", header = FALSE)[,2]
k67_wu_evi <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_EVI.csv", header = FALSE)[,2]

k34_wu_par <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_PAR.csv", header = FALSE)[,2]
k67_wu_par <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_PAR.csv", header = FALSE)[,2]
rja_wu_par <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_PAR.csv", header = FALSE)[,2]
cax_wu_par <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_PAR.csv", header = FALSE)[,2]

# MCD43C4 data
get_annual_means <- function(ts_data) {
  df <- as.data.frame(split(ts_data, ceiling(seq_along(ts_data)/12)))
  means <- rowMeans(df)
  return(means)
}

k34_mod_evi <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K34_2001-2006_monthly_EVI.csv", header = TRUE)[,1]) / 10000
k67_mod_evi <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K67_2002-2005_2009-2011_monthly_EVI.csv", header = TRUE)[,1]) / 10000
rja_mod_evi <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/RJA_2001-2002_monthly_EVI.csv", header = TRUE)[,1]) / 10000
cax_mod_evi <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/CAX_2001-2003_monthly_EVI.csv", header = TRUE)[,1]) / 10000

k34_mod_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K34_2001-2006_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
k67_mod_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K67_2002-2005_2009-2011_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
rja_mod_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/RJA_2001-2002_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
cax_mod_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/CAX_2001-2003_monthly_NIRv.csv", header = TRUE)[,1]) / 10000

k34_mod_nirv_t <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K34_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
k67_mod_nirv_t <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/K67_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
rja_mod_nirv_t <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/RJA_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000
cax_mod_nirv_t <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/CAX_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000

### Get data from L2 data clipped out by polygons ####

file_df <- function(input_dir, year, time) {
  file_list <- list.files(input_dir, pattern = "*.nc", full.names = TRUE, recursive = TRUE)
  
  if (time == "8-day") {
    dates <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0((year),"-12-31")), by="days")
    
    # Create data frame with column for each 8-day file list
    for (i in 1:46) {
      
      sub_dates <- dates[(i * 8 - 7):(i * 8)]
      
      sub_files <- c()
      
      for (j in 1:length(sub_dates)) {
        
        check_file <- file_list[grepl(sub_dates[j], file_list)]
        
        if (length(check_file) != 0) {
          sub_files <- c(sub_files, check_file)
        } else {
          sub_files <- c(sub_files, NA)
        }
      }
      if (i == 1) {
        df <- cbind(sub_files)
      } else {
        df <- cbind(df, sub_files)
      }
    }
  }
  
  if (time == "16-day") {
    dates <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0((year + 1),"-12-31")), by="days")
    
    # Create data frame with column for each 16-day file list
    for (i in 1:23) {
      
      sub_dates <- dates[(i * 16 - 15):(i * 16)]
      
      sub_files <- c()
      
      for (j in 1:length(sub_dates)) {
        
        check_file <- file_list[grepl(sub_dates[j], file_list)]
        
        if (length(check_file) != 0) {
          sub_files <- c(sub_files, check_file)
        } else {
          sub_files <- c(sub_files, NA)
        }
      }
      if (i == 1) {
        df <- cbind(sub_files)
      } else {
        df <- cbind(df, sub_files)
      }
    }
  }
  
  if (time == "month") {
    dates <- seq(as.Date(paste0(year,"-01-01")), as.Date(paste0(year,"-12-31")), by="days")
    
    df <- data.frame(matrix(ncol = 12, nrow = 31))
    # Create data frame with column for each month
    for (i in 1:12){
      if (i < 10) {
        m <- paste0("0", i)
      } else {
        m <- as.character(i)
      }
      
      sub_dates <- subset(dates, format.Date(dates, "%m") == m)
      sub_files <- c()
      
      for (j in 1:length(sub_dates)) {
        
        check_file <- file_list[grepl(sub_dates[j], file_list)]
        
        if (length(check_file) != 0) {
          sub_files <- c(sub_files, check_file)
        } else {
          sub_files <- c(sub_files, NA)
        }
      }
      
      # Force length to 31
      if (length(sub_files) < 31) {
        sub_files <- sub_files[1:31]
      }
      
      if (i == 1) {
        df <- cbind(sub_files)
      } else {
        df <- cbind(df, sub_files)
      }
    }
  }
  
  return(df)
}
get_ts  <- function(df_f, variable, time, filters, threshs, direct) {
  
  annual_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(annual_df) <- c("Mean", "SD", "SEM", "n")
  
  if (time == "8-day") {
    t <- 46
  } else if (time == "16-day") {
    t <- 23
  } else if (time == "month") {
    t <- 12
  }
  
  for (i in 1:t) {
    
    df_t <- df_f[, i]
    df_t <- df_t[!is.na(df_t)]
    
    if (length(df_t) != 0) {
      for (j in 1:length(df_t)) {
        nc <- nc_open(df_t[j])
        
        # Get data for this time step
        data <- data.frame(var = ncvar_get(nc, variable))
        colnames(data)[1] <- variable
        
        # Get filters for this time step
        if (!is.null(filters)) {
          for (f in 1:length(filters)){
            data <- cbind(data, f = ncvar_get(nc, filters[f]))
            colnames(data)[(f + 1)] <- filters[f]
          }
        }
        
        nc_close(nc)
        
        if (j == 1){
          ts_data <- data
        } else {
          ts_data <- rbind(ts_data, data)
        }
      }
      
      # filter the data
      if (!is.null(filters)) {
        for (f in 1:length(filters)){
          if (direct[f] == "lt"){
            ts_data <- ts_data[ts_data[, (f + 1)] <= threshs[f],]
          } else if (direct[f] == "gt"){
            ts_data <- ts_data[ts_data[, (f + 1)] >= threshs[f],]
          } else if (direct[f] == "eq"){
            ts_data <- ts_data[ts_data[, (f + 1)] == threshs[f],]
          }
        }
      }
      
      ts_data <- na.omit(ts_data)
      
      if (nrow(ts_data) != 0) {
        annual_df[nrow(annual_df) + 1,] <- c(mean(ts_data[, 1]), sd(ts_data[, 1]), sd(ts_data[, 1]) / (sqrt(length(ts_data[, 1]))), length(ts_data[, 1]))
      } else {
        annual_df[nrow(annual_df) + 1,] <- c(NA, NA, NA, NA)
      }
    } else {
      annual_df[nrow(annual_df) + 1,] <- c(NA, NA, NA, NA)
    }
  }
  
  return(annual_df)
}

k34_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2018", 2018, "month")
k34_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2019", 2019, "month")
k34_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2020", 2020, "month")
k34_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2021", 2021, "month")

k67_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2018", 2018, "month")
k67_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2019", 2019, "month")
k67_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2020", 2020, "month")
k67_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2021", 2021, "month")

rja_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2018", 2018, "month")
rja_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2019", 2019, "month")
rja_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2020", 2020, "month")
rja_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2021", 2021, "month")

cax_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2018", 2018, "month")
cax_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2019", 2019, "month")
cax_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2020", 2020, "month")
cax_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2021", 2021, "month")

# K34 SIF Corr
# k34_sif_cor_2018 <- get_ts(k34_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_2019 <- get_ts(k34_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_2020 <- get_ts(k34_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_2021 <- get_ts(k34_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_df   <- rbind(k34_sif_cor_2019, k34_sif_cor_2020, k34_sif_cor_2021)

# K67 SIF Corr
k67_sif_cor_2018 <- get_ts(k67_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_2019 <- get_ts(k67_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_2020 <- get_ts(k67_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_2021 <- get_ts(k67_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_df   <- rbind(k67_sif_cor_2019, k67_sif_cor_2020, k67_sif_cor_2021)

# RJA SIF Corr
# rja_sif_cor_2018 <- get_ts(rja_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_2019 <- get_ts(rja_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_2020 <- get_ts(rja_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_2021 <- get_ts(rja_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_df   <- rbind(rja_sif_cor_2019, rja_sif_cor_2020, rja_sif_cor_2021)

# CAX SIF Corr
# cax_sif_cor_2018 <- get_ts(cax_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
cax_sif_cor_2019 <- get_ts(cax_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
cax_sif_cor_2020 <- get_ts(cax_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
cax_sif_cor_2021 <- get_ts(cax_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
cax_sif_cor_df   <- rbind(cax_sif_cor_2019, cax_sif_cor_2020, cax_sif_cor_2021)

# Make weighted means
for (i in 1:12){
  
  k34_sif_cor_wm <-  weighted.mean(c(k34_sif_cor_2019[i,1], k34_sif_cor_2020[i,1], k34_sif_cor_2021[i,1]),
                                   c(k34_sif_cor_2019[i,4], k34_sif_cor_2020[i,4], k34_sif_cor_2021[i,4]),
                                   na.rm = TRUE)
  k67_sif_cor_wm <-  weighted.mean(c(k67_sif_cor_2019[i,1], k67_sif_cor_2020[i,1], k67_sif_cor_2021[i,1]),
                                   c(k67_sif_cor_2019[i,4], k67_sif_cor_2020[i,4], k67_sif_cor_2021[i,4]),
                                   na.rm = TRUE)
  rja_sif_cor_wm <-  weighted.mean(c(rja_sif_cor_2019[i,1], rja_sif_cor_2020[i,1], rja_sif_cor_2021[i,1]),
                                   c(rja_sif_cor_2019[i,4], rja_sif_cor_2020[i,4], rja_sif_cor_2021[i,4]),
                                   na.rm = TRUE)
  cax_sif_cor_wm <-  weighted.mean(c(cax_sif_cor_2019[i,1], cax_sif_cor_2020[i,1], cax_sif_cor_2021[i,1]),
                                   c(cax_sif_cor_2019[i,4], cax_sif_cor_2020[i,4], cax_sif_cor_2021[i,4]),
                                   na.rm = TRUE)
  
  if (i == 1) {
    k34_sif_cor <- k34_sif_cor_wm
    k67_sif_cor <- k67_sif_cor_wm
    rja_sif_cor <- rja_sif_cor_wm
    cax_sif_cor <- cax_sif_cor_wm
  } else {
    k34_sif_cor <- c(k34_sif_cor, k34_sif_cor_wm)
    k67_sif_cor <- c(k67_sif_cor, k67_sif_cor_wm)
    rja_sif_cor <- c(rja_sif_cor, rja_sif_cor_wm)
    cax_sif_cor <- c(cax_sif_cor, cax_sif_cor_wm)
  }
}

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


