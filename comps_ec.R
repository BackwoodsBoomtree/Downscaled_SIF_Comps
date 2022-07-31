library(terra)
library(ncdf4)
library(viridis)

out_name    <- "G:/SIF_comps/figs/v2/comps_ec_black.pdf"

# Get data
t_2018 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2018/TROPOMI.ESA.SIF.2018.EBF90.monthly.1deg.CF80.nc"
t_2019 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2019/TROPOMI.ESA.SIF.2019.EBF90.monthly.1deg.CF80.nc"
t_2020 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2020/TROPOMI.ESA.SIF.2020.EBF90.monthly.1deg.CF80.nc"
t_2021 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2021/TROPOMI.ESA.SIF.2021.EBF90.monthly.1deg.CF80.nc"

k34_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_PC.csv", header = FALSE)[,2] / 10 # my axis when extracting the data was off by a decimal place
k67_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_PC.csv", header = FALSE)[,2]
rja_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_PC.csv", header = FALSE)[,2]
cax_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_PC.csv", header = FALSE)[,2]

k34_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_GEP.csv", header = FALSE)[,2]
k67_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_GEP.csv", header = FALSE)[,2]
rja_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/RJA_GEP.csv", header = FALSE)[,2]
cax_gep <- read.csv("G:/SIF_comps/figs/Wu_2016/CAX_GEP.csv", header = FALSE)[,2]

k34_lai <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_LAI.csv", header = FALSE)[,2]
k67_lai <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_LAI.csv", header = FALSE)[,2]

k34_evi <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_EVI.csv", header = FALSE)[,2]
k67_evi <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_EVI.csv", header = FALSE)[,2]

### Get K34 and CAX from gridded data ####
k34_coords <- cbind(-60.2093, -2.6091)
cax_coords <- cbind(-51.53, -1.72)
# k67_coords <- cbind(-54.959, -2.857)
k34        <- vect(k34_coords, crs="+proj=longlat +datum=WGS84")
cax        <- vect(cax_coords, crs="+proj=longlat +datum=WGS84")
# k67        <- vect(k67_coords, crs="+proj=longlat +datum=WGS84")

t_sif_cor_2018 <- rast(t_2018, subds = "SIF_Corr_743")
t_sif_cor_2019 <- rast(t_2019, subds = "SIF_Corr_743")
t_sif_cor_2020 <- rast(t_2020, subds = "SIF_Corr_743")
t_sif_cor_2021 <- rast(t_2021, subds = "SIF_Corr_743")

t_n_2018 <- rast(t_2018, subds = "n")
t_n_2019 <- rast(t_2019, subds = "n")
t_n_2020 <- rast(t_2020, subds = "n")
t_n_2021 <- rast(t_2021, subds = "n")

# t_nirv_2018 <- rast(t_cs_2018, subds = "NIRv")
# t_nirv_2019 <- rast(t_cs_2019, subds = "NIRv")
# t_nirv_2020 <- rast(t_cs_2020, subds = "NIRv")
# t_nirv_2021 <- rast(t_cs_2021, subds = "NIRv")

### Get weighted mean
for (i in 1:12){
  if (i >= 5){
    total_n <- app(c(t_n_2018[[(i - 4)]], t_n_2019[[i]], t_n_2020[[i]], t_n_2021[[i]]), sum, na.rm = TRUE)
    w_2018  <- t_n_2018[[(i - 4)]] / total_n
    w_2019  <- t_n_2019[[i]] / total_n
    w_2020  <- t_n_2020[[i]] / total_n
    w_2021  <- t_n_2021[[i]] / total_n
    
    step_wm_sif <- (t_sif_cor_2018[[(i - 4)]] * w_2018) + (t_sif_cor_2019[[i]] * w_2019) + (t_sif_cor_2020[[i]] * w_2019) + (t_sif_cor_2020[[i]] * w_2020)
    
  } else {
    total_n <- app(c(t_n_2019[[i]], t_n_2020[[i]], t_n_2021[[i]]), sum, na.rm = TRUE)
    w_2019  <- t_n_2019[[i]] / total_n
    w_2020  <- t_n_2020[[i]] / total_n
    w_2021  <- t_n_2021[[i]] / total_n
    
    step_wm_sif <- (t_sif_cor_2019[[i]] * w_2019) + (t_sif_cor_2020[[i]] * w_2019) + (t_sif_cor_2020[[i]] * w_2020)
    
  }
  
  if (i ==1) {
    wm_sif  <- step_wm_sif
  } else {
    wm_sif  <- c(wm_sif, step_wm_sif)
  }
}

k34_sif_cor  <- extract(wm_sif, k34, ID = FALSE)
cax_sif_cor  <- extract(wm_sif, cax, ID = FALSE)
# k67_sif_cor <- extract(t_sif_cor_2021, k67, ID = FALSE)

k34_sif_cor <- as.numeric(k34_sif_cor[1,])
cax_sif_cor <- as.numeric(cax_sif_cor[1,])
# k67_sif_cor <- as.numeric(k67_sif_cor[1,])

### Get K67 and RJA data ####

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

k67_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2018", 2018, "month")
k67_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2019", 2019, "month")
k67_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2020", 2020, "month")
k67_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2021", 2021, "month")

rja_files_2018 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2018", 2018, "month")
rja_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2019", 2019, "month")
rja_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2020", 2020, "month")
rja_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2021", 2021, "month")

# K67 SIF Corr
k67_sif_cor_2018 <- get_ts(k67_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
k67_sif_cor_2019 <- get_ts(k67_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
k67_sif_cor_2020 <- get_ts(k67_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
k67_sif_cor_2021 <- get_ts(k67_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
k67_sif_cor_df   <- rbind(k67_sif_cor_2018, k67_sif_cor_2019, k67_sif_cor_2020, k67_sif_cor_2021)

# RJA SIF Corr
rja_sif_cor_2018 <- get_ts(rja_files_2018, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
rja_sif_cor_2019 <- get_ts(rja_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
rja_sif_cor_2020 <- get_ts(rja_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
rja_sif_cor_2021 <- get_ts(rja_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0.80, 90), c("lt", "gt"))
rja_sif_cor_df   <- rbind(rja_sif_cor_2018, rja_sif_cor_2019, rja_sif_cor_2020, rja_sif_cor_2021)

# K67 NIRv
k67_nirv_2018 <- get_ts(k67_files_2018, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
k67_nirv_2019 <- get_ts(k67_files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
k67_nirv_2020 <- get_ts(k67_files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
k67_nirv_2021 <- get_ts(k67_files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
k67_nirv_df   <- rbind(k67_nirv_2018, k67_nirv_2019, k67_nirv_2020, k67_nirv_2021)

# RJA NIRv
rja_nirv_2018 <- get_ts(rja_files_2018, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
rja_nirv_2019 <- get_ts(rja_files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
rja_nirv_2020 <- get_ts(rja_files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
rja_nirv_2021 <- get_ts(rja_files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.01, 99, 20), c("lt", "gt", "gt"))
rja_nirv_df   <- rbind(rja_nirv_2018, rja_nirv_2019, rja_nirv_2020, rja_nirv_2021)

# Make Weighted means
k67_df_n                <- cbind(k67_sif_cor_2018$n, k67_sif_cor_2019$n, k67_sif_cor_2020$n, k67_sif_cor_2021$n)
k67_df_n                <- cbind(k67_df_n, total_n = rowSums(k67_df_n, na.rm = TRUE))
k67_sif_cor_2018$weight <- k67_sif_cor_2018$n / k67_df_n[,5]
k67_sif_cor_2019$weight <- k67_sif_cor_2019$n / k67_df_n[,5]
k67_sif_cor_2020$weight <- k67_sif_cor_2020$n / k67_df_n[,5]
k67_sif_cor_2021$weight <- k67_sif_cor_2021$n / k67_df_n[,5]

rja_df_n                <- cbind(rja_sif_cor_2018$n, rja_sif_cor_2019$n, rja_sif_cor_2020$n, rja_sif_cor_2021$n)
rja_df_n                <- cbind(rja_df_n, total_n = rowSums(rja_df_n, na.rm = TRUE))
rja_sif_cor_2018$weight <- rja_sif_cor_2018$n / rja_df_n[,5]
rja_sif_cor_2019$weight <- rja_sif_cor_2019$n / rja_df_n[,5]
rja_sif_cor_2020$weight <- rja_sif_cor_2020$n / rja_df_n[,5]
rja_sif_cor_2021$weight <- rja_sif_cor_2021$n / rja_df_n[,5]

k67_df_n                <- cbind(k67_nirv_2018$n, k67_nirv_2019$n, k67_nirv_2020$n, k67_nirv_2021$n)
k67_df_n                <- cbind(k67_df_n, total_n = rowSums(k67_df_n, na.rm = TRUE))
k67_nirv_2018$weight <- k67_nirv_2018$n / k67_df_n[,5]
k67_nirv_2019$weight <- k67_nirv_2019$n / k67_df_n[,5]
k67_nirv_2020$weight <- k67_nirv_2020$n / k67_df_n[,5]
k67_nirv_2021$weight <- k67_nirv_2021$n / k67_df_n[,5]

rja_df_n                <- cbind(rja_nirv_2018$n, rja_nirv_2019$n, rja_nirv_2020$n, rja_nirv_2021$n)
rja_df_n                <- cbind(rja_df_n, total_n = rowSums(rja_df_n, na.rm = TRUE))
rja_nirv_2018$weight <- rja_nirv_2018$n / rja_df_n[,5]
rja_nirv_2019$weight <- rja_nirv_2019$n / rja_df_n[,5]
rja_nirv_2020$weight <- rja_nirv_2020$n / rja_df_n[,5]
rja_nirv_2021$weight <- rja_nirv_2021$n / rja_df_n[,5]

for (i in 1:12){
  
  k67_sif_cor_wm <-  weighted.mean(c(k67_sif_cor_2018[i,1], k67_sif_cor_2019[i,1], k67_sif_cor_2020[i,1], k67_sif_cor_2021[i,1]),
                       c(k67_sif_cor_2018[i,5], k67_sif_cor_2019[i,5], k67_sif_cor_2020[i,5], k67_sif_cor_2021[i,5]),
                       na.rm = TRUE)
  rja_sif_cor_wm <-  weighted.mean(c(rja_sif_cor_2018[i,1], rja_sif_cor_2019[i,1], rja_sif_cor_2020[i,1], rja_sif_cor_2021[i,1]),
                           c(rja_sif_cor_2018[i,5], rja_sif_cor_2019[i,5], rja_sif_cor_2020[i,5], rja_sif_cor_2021[i,5]),
                           na.rm = TRUE)
  k67_nirv_wm <-  weighted.mean(c(k67_nirv_2018[i,1], k67_nirv_2019[i,1], k67_nirv_2020[i,1], k67_nirv_2021[i,1]),
                                   c(k67_nirv_2018[i,5], k67_nirv_2019[i,5], k67_nirv_2020[i,5], k67_nirv_2021[i,5]),
                                   na.rm = TRUE)
  rja_nirv_wm <-  weighted.mean(c(rja_nirv_2018[i,1], rja_nirv_2019[i,1], rja_nirv_2020[i,1], rja_nirv_2021[i,1]),
                                   c(rja_nirv_2018[i,5], rja_nirv_2019[i,5], rja_nirv_2020[i,5], rja_nirv_2021[i,5]),
                                   na.rm = TRUE)
  
  if (i == 1) {
    k67_sif_cor <- k67_sif_cor_wm
    rja_sif_cor <- rja_sif_cor_wm
    k67_nirv <- k67_nirv_wm
    rja_nirv <- rja_nirv_wm
  } else {
    k67_sif_cor <- c(k67_sif_cor, k67_sif_cor_wm)
    rja_sif_cor <- c(rja_sif_cor, rja_sif_cor_wm)
    k67_nirv <- c(k67_nirv, k67_nirv_wm)
    rja_nirv <- c(rja_nirv, rja_nirv_wm)
  }
}


### PLOTTING ####

plot(k34_sif_cor, type = "l", main = "K34")
par(new = TRUE)
plot(k34_pc, type = "l", col = "red")
par(new = TRUE)
plot(k34_nirv, type = "l", col = "blue")

plot(c(2, 4, 5, seq(7,12)), cax_pc, type = "l", xlim = c(1,12), col = "red", main = "CAX")
par(new = TRUE)
plot(cax_sif_cor, type = "l")

plot(k67_sif_cor, type = "l", main = "K67")
par(new = TRUE)
plot(k67_pc, type = "l", col = "red")
par(new = TRUE)
plot(k67_nirv, type = "l", col = "blue")

plot(seq(2,11), rja_pc, type = "l", xlim = c(1,12), col = "red", main = "RJA")
par(new = TRUE)
plot(rja_sif_cor, type = "l")
par(new = TRUE)
plot(rja_nirv, type = "l", col = "blue")


#### Plot ####
mag.cols  <- magma(7)
vir.cols  <- viridis(7)
y_sif     <- c(0.2, 0.7)
y_gep     <- c(4, 13)
y_pc_k34  <- c(0.012, 0.03)
y_pc_k67  <- c(0.010, 0.028)
y_pc_cax  <- c(0.014, 0.032)
y_pc_rja  <- c(0.010, 0.028)
y_lab_sif <- bquote("Daily Average SIF (mW/m"^"2"*"/sr/nm)")
y_lab_gep <- bquote("GEP (gC m"^"-2"*"day"^"-1")
y_lab_pc  <- bquote("GEP (mol CO"[2]*"/mol photons)")
x_lab     <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
# y_pc     <- c(min(k34_pc, k67_pc, cax_pc, rja_pc),
#               max(k34_pc, k67_pc, cax_pc, rja_pc))
# y_gep    <- c(min(k34_gep, k67_gep, cax_gep, rja_gep),
#               max(k34_gep, k67_gep, cax_gep, rja_gep))

cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(2, 2), oma=c(2.5,2.75,0,3), bg = "black")

### K34 ###
op <- par(mar = c(0,0.5,2,4.5), bg = "black")

plot(NULL, xlim = c(1,12), ylim = y_sif, axes = FALSE, xaxs="i")
mtext(3, text = "K34", col = "white")

# Shaded area
rect(8, 0, 9, 1, col = rgb(0.30,0.30,0.30), border = NA)
box(col = "white")

# Add data
lines(k34_sif_cor, col = mag.cols[4], type = "b", lwd = 2, pch = 16)
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = mag.cols[4], col = mag.cols[4], las = 2)

par(new = TRUE)
plot(k34_gep, ylim = y_gep, col = vir.cols[5], axes = FALSE, xaxs="i", lty = 2, type = "b", lwd = 2, pch = 15)
axis(4, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = vir.cols[5], col = vir.cols[5], las = 2)

par(new = TRUE)
plot(k34_pc, ylim = y_pc_k34, col = vir.cols[7], axes = FALSE, xaxs="i", lty = 3, type = "b", lwd = 2, pch = 17)
axis(4, tck = 0.06, at = seq(min(y_pc_k34), max(y_pc_k34), by = 0.004),
     mgp=c(3, 0.2, 0), col.axis = vir.cols[7], col = vir.cols[7], las = 2, line = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12), col.axis = "white", col = "white")

legend("topleft", legend = c("SIF", "GEP", "PC"), horiz = TRUE,
       col = c(mag.cols[4], vir.cols[5], vir.cols[7]), lty = c(1,2,3), pch = c(16, 15, 17), lwd = c(2,2,2),
       box.col = "transparent", bg = "transparent", text.col = "white", cex = 0.85)

### K67 ###
op <- par(mar = c(0,0.5,2,4.5), bg = "black")

plot(NULL, xlim = c(1,12), ylim = y_sif, axes = FALSE, xaxs="i")
mtext(3, text = "K67", col = "white")

# Shaded area
rect(7, 0, 11, 1, col = rgb(0.30,0.30,0.30), border = NA)
box(col = "white")

# Add data
lines(k67_sif_cor, col = mag.cols[4], type = "b", lwd = 2, pch = 16)
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = mag.cols[4], col = mag.cols[4], las = 2, labels = FALSE)

par(new = TRUE)
plot(k67_gep, ylim = y_gep, col = vir.cols[5], axes = FALSE, xaxs="i", lty = 2, type = "b", lwd = 2, pch = 15)
axis(4, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = vir.cols[5], col = vir.cols[5], las = 2)

par(new = TRUE)
plot(k67_pc, ylim = y_pc_k67, col = vir.cols[7], axes = FALSE, xaxs="i", lty = 3, type = "b", lwd = 2, pch = 17)
axis(4, tck = 0.06, at = seq(min(y_pc_k67), max(y_pc_k67), by = 0.004),
     mgp=c(3, 0.2, 0), col.axis = vir.cols[7], col = vir.cols[7], las = 2, line = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12), col.axis = "white", col = "white")


### CAX ###
op <- par(mar = c(0,0.5,2,4.5), bg = "black")

plot(NULL, xlim = c(1,12), ylim = y_sif, axes = FALSE, xaxs="i")
mtext(3, text = "CAX", col = "white")

# Shaded area
rect(8, 0, 11, 1, col = rgb(0.30,0.30,0.30), border = NA)
box(col = "white")

# Add data
lines(cax_sif_cor, col = mag.cols[4], type = "b", lwd = 2, pch = 16)
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = mag.cols[4], col = mag.cols[4], las = 2)

par(new = TRUE)
plot(c(2, 4, 5, seq(7,12)), cax_gep, xlim = c(1,12), ylim = y_gep, col = vir.cols[5],
     axes = FALSE, xaxs="i", lty = 2, type = "b", lwd = 2, pch = 15)
axis(4, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = vir.cols[5], col = vir.cols[5], las = 2)

par(new = TRUE)
plot(c(2, 4, 5, seq(7,12)), cax_pc, xlim = c(1,12), ylim = y_pc_cax, col = vir.cols[7],
     axes = FALSE, xaxs="i", lty = 3, type = "b", lwd = 2, pch = 17)
axis(4, tck = 0.06, at = seq(min(y_pc_cax), max(y_pc_cax), by = 0.004),
     mgp=c(3, 0.2, 0), col.axis = vir.cols[7], col = vir.cols[7], las = 2, line = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12), col.axis = "white", col = "white")
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12), col.axis = "white", col = "white")


### RJA ###
op <- par(mar = c(0,0.5,2,4.5), bg = "black")

plot(NULL, xlim = c(1,12), ylim = y_sif, axes = FALSE, xaxs="i")
mtext(3, text = "RJA", col = "white")

# Shaded area
rect(5, 0, 9, 1, col = rgb(0.30,0.30,0.30), border = NA)
box(col = "white")

# Add data
lines(rja_sif_cor, col = mag.cols[4], type = "b", lwd = 2, pch = 16)
axis(2, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = mag.cols[4], col = mag.cols[4], las = 2, labels = FALSE)

par(new = TRUE)
plot(seq(2,11), rja_gep, xlim = c(1,12), ylim = y_gep, col = vir.cols[5],
     axes = FALSE, xaxs="i", lty = 2, type = "b", lwd = 2, pch = 15)
axis(4, tck = 0.06, mgp=c(3, 0.2, 0), col.axis = vir.cols[5], col = vir.cols[5], las = 2)

par(new = TRUE)
plot(seq(2,11), rja_pc, xlim = c(1,12), ylim = y_pc_rja, col = vir.cols[7],
     axes = FALSE, xaxs="i", lty = 3, type = "b", lwd = 2, pch = 17)
axis(4, tck = 0.06, at = seq(min(y_pc_rja), max(y_pc_rja), by = 0.004),
     mgp=c(3, 0.2, 0), col.axis = vir.cols[7], col = vir.cols[7], las = 2, line = 2)

axis(1, tck = 0.06, labels = FALSE, at = seq(1, 12, by = 2), col.axis = "white", col = "white")
axis(1, tck = 0.03, labels = FALSE, at = seq(2, 12), col.axis = "white", col = "white")
axis(1, tck = FALSE, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 12), col.axis = "white", col = "white")


# Margins
mtext(1, text = "Month of Year", col = "white", line = 1.5, outer = TRUE)
mtext(2, text = y_lab_sif, col = mag.cols[4], line = 1, outer = TRUE)
mtext(4, text = y_lab_gep, col = vir.cols[5], line = 0.5, outer = TRUE)
mtext(4, text = y_lab_pc, col = vir.cols[7], line = 2, outer = TRUE)

dev.off()
