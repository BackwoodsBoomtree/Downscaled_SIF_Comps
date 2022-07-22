library(terra)
library(ncdf4)

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
      
      annual_df[nrow(annual_df) + 1,] <- c(mean(ts_data[, 1], rm.na = TRUE), sd(ts_data[, 1]), sd(ts_data[, 1]) / (sqrt(length(ts_data[, 1]))), length(ts_data[, 1]))
      
    } else {
      annual_df[nrow(annual_df) + 1,] <- c(NA, NA, NA, NA)
    }
  }
  
  return(annual_df)
}

#### Tropics ####
out_dir    <- "G:/SIF_comps/csv/tropics"
out_name   <- "/Tropics_2019-2021_"
files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/tropics/2019", 2019, "month")
files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/tropics/2020", 2020, "month")
files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/tropics/2021", 2021, "month")

#SIF
ts_sif_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all      <- rbind(ts_sif_all_2019, ts_sif_all_2020, ts_sif_all_2021)
write.csv(ts_sif_all, paste0(out_dir, out_name, "sif_all.csv"), row.names = FALSE)

ts_sif_cs_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all      <- rbind(ts_sif_cs_all_2019, ts_sif_cs_all_2020, ts_sif_cs_all_2021)
write.csv(ts_sif_cs_all, paste0(out_dir, out_name, "sif_cs_all.csv"), row.names = FALSE)

ts_sif_cs_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold      <- rbind(ts_sif_cs_cold_2019, ts_sif_cs_cold_2020, ts_sif_cs_cold_2021)
write.csv(ts_sif_cs_cold, paste0(out_dir, out_name, "sif_cs_cold.csv"), row.names = FALSE)

ts_sif_cf_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold      <- rbind(ts_sif_cf_cold_2019, ts_sif_cf_cold_2020, ts_sif_cf_cold_2021)
write.csv(ts_sif_cf_cold, paste0(out_dir, out_name, "sif_cf_cold.csv"), row.names = FALSE)

# NIRv
ts_nirv_all_2019 <- get_ts(files_2019, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2020 <- get_ts(files_2020, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2021 <- get_ts(files_2021, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all      <- rbind(ts_nirv_all_2019, ts_nirv_all_2020, ts_nirv_all_2021)
write.csv(ts_nirv_all, paste0(out_dir, out_name, "nirv_all.csv"), row.names = FALSE)

ts_nirv_cs_all_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all      <- rbind(ts_nirv_cs_all_2019, ts_nirv_cs_all_2020, ts_nirv_cs_all_2021)
write.csv(ts_nirv_cs_all, paste0(out_dir, out_name, "nirv_cs_all.csv"), row.names = FALSE)

ts_nirv_cs_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold      <- rbind(ts_nirv_cs_cold_2019, ts_nirv_cs_cold_2020, ts_nirv_cs_cold_2021)
write.csv(ts_nirv_cs_cold, paste0(out_dir, out_name, "nirv_cs_cold.csv"), row.names = FALSE)

ts_nirv_cf_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold      <- rbind(ts_nirv_cf_cold_2019, ts_nirv_cf_cold_2020, ts_nirv_cf_cold_2021)
write.csv(ts_nirv_cf_cold, paste0(out_dir, out_name, "nirv_cf_cold.csv"), row.names = FALSE)

# NIRv R
ts_nirvr_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all      <- rbind(ts_nirvr_all_2019, ts_nirvr_all_2020, ts_nirvr_all_2021)
write.csv(ts_nirvr_all, paste0(out_dir, out_name, "nirvr_all.csv"), row.names = FALSE)

ts_nirvr_cs_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all      <- rbind(ts_nirvr_cs_all_2019, ts_nirvr_cs_all_2020, ts_nirvr_cs_all_2021)
write.csv(ts_nirvr_cs_all, paste0(out_dir, out_name, "nirvr_cs_all.csv"), row.names = FALSE)

ts_nirvr_cs_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold      <- rbind(ts_nirvr_cs_cold_2019, ts_nirvr_cs_cold_2020, ts_nirvr_cs_cold_2021)
write.csv(ts_nirvr_cs_cold, paste0(out_dir, out_name, "nirvr_cs_cold.csv"), row.names = FALSE)

ts_nirvr_cf_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold      <- rbind(ts_nirvr_cf_cold_2019, ts_nirvr_cf_cold_2020, ts_nirvr_cf_cold_2021)
write.csv(ts_nirvr_cf_cold, paste0(out_dir, out_name, "nirvr_cf_cold.csv"), row.names = FALSE)

# Red
ts_red_all_2019 <- get_ts(files_2019, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2020 <- get_ts(files_2020, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2021 <- get_ts(files_2021, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all      <- rbind(ts_red_all_2019, ts_red_all_2020, ts_red_all_2021)
write.csv(ts_red_all, paste0(out_dir, out_name, "red_all.csv"), row.names = FALSE)

ts_red_cs_all_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all      <- rbind(ts_red_cs_all_2019, ts_red_cs_all_2020, ts_red_cs_all_2021)
write.csv(ts_red_cs_all, paste0(out_dir, out_name, "red_cs_all.csv"), row.names = FALSE)

ts_red_cs_cold_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold      <- rbind(ts_red_cs_cold_2019, ts_red_cs_cold_2020, ts_red_cs_cold_2021)
write.csv(ts_red_cs_cold, paste0(out_dir, out_name, "red_cs_cold.csv"), row.names = FALSE)

ts_red_cf_cold_2019 <- get_ts(files_2019, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2020 <- get_ts(files_2020, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2021 <- get_ts(files_2021, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold      <- rbind(ts_red_cf_cold_2019, ts_red_cf_cold_2020, ts_red_cf_cold_2021)
write.csv(ts_red_cf_cold, paste0(out_dir, out_name, "red_cf_cold.csv"), row.names = FALSE)

# NIR
ts_nir_all_2019 <- get_ts(files_2019, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2020 <- get_ts(files_2020, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2021 <- get_ts(files_2021, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all      <- rbind(ts_nir_all_2019, ts_nir_all_2020, ts_nir_all_2021)
write.csv(ts_nir_all, paste0(out_dir, out_name, "nir_all.csv"), row.names = FALSE)

ts_nir_cs_all_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all      <- rbind(ts_nir_cs_all_2019, ts_nir_cs_all_2020, ts_nir_cs_all_2021)
write.csv(ts_nir_cs_all, paste0(out_dir, out_name, "nir_cs_all.csv"), row.names = FALSE)

ts_nir_cs_cold_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold      <- rbind(ts_nir_cs_cold_2019, ts_nir_cs_cold_2020, ts_nir_cs_cold_2021)
write.csv(ts_nir_cs_cold, paste0(out_dir, out_name, "nir_cs_cold.csv"), row.names = FALSE)

ts_nir_cf_cold_2019 <- get_ts(files_2019, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2020 <- get_ts(files_2020, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2021 <- get_ts(files_2021, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold      <- rbind(ts_nir_cf_cold_2019, ts_nir_cf_cold_2020, ts_nir_cf_cold_2021)
write.csv(ts_nir_cf_cold, paste0(out_dir, out_name, "nir_cf_cold.csv"), row.names = FALSE)

#### Amazon ####
out_dir    <- "G:/SIF_comps/csv/amazon"
out_name   <- "/Amazon_2019-2021_"
files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2019", 2019, "month")
files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2020", 2020, "month")
files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2021", 2021, "month")

#SIF
ts_sif_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all      <- rbind(ts_sif_all_2019, ts_sif_all_2020, ts_sif_all_2021)
write.csv(ts_sif_all, paste0(out_dir, out_name, "sif_all.csv"), row.names = FALSE)

ts_sif_cs_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all      <- rbind(ts_sif_cs_all_2019, ts_sif_cs_all_2020, ts_sif_cs_all_2021)
write.csv(ts_sif_cs_all, paste0(out_dir, out_name, "sif_cs_all.csv"), row.names = FALSE)

ts_sif_cs_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold      <- rbind(ts_sif_cs_cold_2019, ts_sif_cs_cold_2020, ts_sif_cs_cold_2021)
write.csv(ts_sif_cs_cold, paste0(out_dir, out_name, "sif_cs_cold.csv"), row.names = FALSE)

ts_sif_cf_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold      <- rbind(ts_sif_cf_cold_2019, ts_sif_cf_cold_2020, ts_sif_cf_cold_2021)
write.csv(ts_sif_cf_cold, paste0(out_dir, out_name, "sif_cf_cold.csv"), row.names = FALSE)

# NIRv
ts_nirv_all_2019 <- get_ts(files_2019, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2020 <- get_ts(files_2020, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2021 <- get_ts(files_2021, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all      <- rbind(ts_nirv_all_2019, ts_nirv_all_2020, ts_nirv_all_2021)
write.csv(ts_nirv_all, paste0(out_dir, out_name, "nirv_all.csv"), row.names = FALSE)

ts_nirv_cs_all_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all      <- rbind(ts_nirv_cs_all_2019, ts_nirv_cs_all_2020, ts_nirv_cs_all_2021)
write.csv(ts_nirv_cs_all, paste0(out_dir, out_name, "nirv_cs_all.csv"), row.names = FALSE)

ts_nirv_cs_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold      <- rbind(ts_nirv_cs_cold_2019, ts_nirv_cs_cold_2020, ts_nirv_cs_cold_2021)
write.csv(ts_nirv_cs_cold, paste0(out_dir, out_name, "nirv_cs_cold.csv"), row.names = FALSE)

ts_nirv_cf_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold      <- rbind(ts_nirv_cf_cold_2019, ts_nirv_cf_cold_2020, ts_nirv_cf_cold_2021)
write.csv(ts_nirv_cf_cold, paste0(out_dir, out_name, "nirv_cf_cold.csv"), row.names = FALSE)

# NIRv R
ts_nirvr_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all      <- rbind(ts_nirvr_all_2019, ts_nirvr_all_2020, ts_nirvr_all_2021)
write.csv(ts_nirvr_all, paste0(out_dir, out_name, "nirvr_all.csv"), row.names = FALSE)

ts_nirvr_cs_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all      <- rbind(ts_nirvr_cs_all_2019, ts_nirvr_cs_all_2020, ts_nirvr_cs_all_2021)
write.csv(ts_nirvr_cs_all, paste0(out_dir, out_name, "nirvr_cs_all.csv"), row.names = FALSE)

ts_nirvr_cs_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold      <- rbind(ts_nirvr_cs_cold_2019, ts_nirvr_cs_cold_2020, ts_nirvr_cs_cold_2021)
write.csv(ts_nirvr_cs_cold, paste0(out_dir, out_name, "nirvr_cs_cold.csv"), row.names = FALSE)

ts_nirvr_cf_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold      <- rbind(ts_nirvr_cf_cold_2019, ts_nirvr_cf_cold_2020, ts_nirvr_cf_cold_2021)
write.csv(ts_nirvr_cf_cold, paste0(out_dir, out_name, "nirvr_cf_cold.csv"), row.names = FALSE)

# Red
ts_red_all_2019 <- get_ts(files_2019, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2020 <- get_ts(files_2020, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2021 <- get_ts(files_2021, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all      <- rbind(ts_red_all_2019, ts_red_all_2020, ts_red_all_2021)
write.csv(ts_red_all, paste0(out_dir, out_name, "red_all.csv"), row.names = FALSE)

ts_red_cs_all_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all      <- rbind(ts_red_cs_all_2019, ts_red_cs_all_2020, ts_red_cs_all_2021)
write.csv(ts_red_cs_all, paste0(out_dir, out_name, "red_cs_all.csv"), row.names = FALSE)

ts_red_cs_cold_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold      <- rbind(ts_red_cs_cold_2019, ts_red_cs_cold_2020, ts_red_cs_cold_2021)
write.csv(ts_red_cs_cold, paste0(out_dir, out_name, "red_cs_cold.csv"), row.names = FALSE)

ts_red_cf_cold_2019 <- get_ts(files_2019, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2020 <- get_ts(files_2020, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2021 <- get_ts(files_2021, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold      <- rbind(ts_red_cf_cold_2019, ts_red_cf_cold_2020, ts_red_cf_cold_2021)
write.csv(ts_red_cf_cold, paste0(out_dir, out_name, "red_cf_cold.csv"), row.names = FALSE)

# NIR
ts_nir_all_2019 <- get_ts(files_2019, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2020 <- get_ts(files_2020, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2021 <- get_ts(files_2021, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all      <- rbind(ts_nir_all_2019, ts_nir_all_2020, ts_nir_all_2021)
write.csv(ts_nir_all, paste0(out_dir, out_name, "nir_all.csv"), row.names = FALSE)

ts_nir_cs_all_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all      <- rbind(ts_nir_cs_all_2019, ts_nir_cs_all_2020, ts_nir_cs_all_2021)
write.csv(ts_nir_cs_all, paste0(out_dir, out_name, "nir_cs_all.csv"), row.names = FALSE)

ts_nir_cs_cold_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold      <- rbind(ts_nir_cs_cold_2019, ts_nir_cs_cold_2020, ts_nir_cs_cold_2021)
write.csv(ts_nir_cs_cold, paste0(out_dir, out_name, "nir_cs_cold.csv"), row.names = FALSE)

ts_nir_cf_cold_2019 <- get_ts(files_2019, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2020 <- get_ts(files_2020, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2021 <- get_ts(files_2021, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold      <- rbind(ts_nir_cf_cold_2019, ts_nir_cf_cold_2020, ts_nir_cf_cold_2021)
write.csv(ts_nir_cf_cold, paste0(out_dir, out_name, "nir_cf_cold.csv"), row.names = FALSE)

#### Africa ####
out_dir    <- "G:/SIF_comps/csv/africa"
out_name   <- "/Africa_2019-2021_"
files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/africa/2019", 2019, "month")
files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/africa/2020", 2020, "month")
files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/africa/2021", 2021, "month")

#SIF
ts_sif_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all      <- rbind(ts_sif_all_2019, ts_sif_all_2020, ts_sif_all_2021)
write.csv(ts_sif_all, paste0(out_dir, out_name, "sif_all.csv"), row.names = FALSE)

ts_sif_cs_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all      <- rbind(ts_sif_cs_all_2019, ts_sif_cs_all_2020, ts_sif_cs_all_2021)
write.csv(ts_sif_cs_all, paste0(out_dir, out_name, "sif_cs_all.csv"), row.names = FALSE)

ts_sif_cs_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold      <- rbind(ts_sif_cs_cold_2019, ts_sif_cs_cold_2020, ts_sif_cs_cold_2021)
write.csv(ts_sif_cs_cold, paste0(out_dir, out_name, "sif_cs_cold.csv"), row.names = FALSE)

ts_sif_cf_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold      <- rbind(ts_sif_cf_cold_2019, ts_sif_cf_cold_2020, ts_sif_cf_cold_2021)
write.csv(ts_sif_cf_cold, paste0(out_dir, out_name, "sif_cf_cold.csv"), row.names = FALSE)

# NIRv
ts_nirv_all_2019 <- get_ts(files_2019, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2020 <- get_ts(files_2020, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2021 <- get_ts(files_2021, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all      <- rbind(ts_nirv_all_2019, ts_nirv_all_2020, ts_nirv_all_2021)
write.csv(ts_nirv_all, paste0(out_dir, out_name, "nirv_all.csv"), row.names = FALSE)

ts_nirv_cs_all_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all      <- rbind(ts_nirv_cs_all_2019, ts_nirv_cs_all_2020, ts_nirv_cs_all_2021)
write.csv(ts_nirv_cs_all, paste0(out_dir, out_name, "nirv_cs_all.csv"), row.names = FALSE)

ts_nirv_cs_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold      <- rbind(ts_nirv_cs_cold_2019, ts_nirv_cs_cold_2020, ts_nirv_cs_cold_2021)
write.csv(ts_nirv_cs_cold, paste0(out_dir, out_name, "nirv_cs_cold.csv"), row.names = FALSE)

ts_nirv_cf_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold      <- rbind(ts_nirv_cf_cold_2019, ts_nirv_cf_cold_2020, ts_nirv_cf_cold_2021)
write.csv(ts_nirv_cf_cold, paste0(out_dir, out_name, "nirv_cf_cold.csv"), row.names = FALSE)

# NIRv R
ts_nirvr_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all      <- rbind(ts_nirvr_all_2019, ts_nirvr_all_2020, ts_nirvr_all_2021)
write.csv(ts_nirvr_all, paste0(out_dir, out_name, "nirvr_all.csv"), row.names = FALSE)

ts_nirvr_cs_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all      <- rbind(ts_nirvr_cs_all_2019, ts_nirvr_cs_all_2020, ts_nirvr_cs_all_2021)
write.csv(ts_nirvr_cs_all, paste0(out_dir, out_name, "nirvr_cs_all.csv"), row.names = FALSE)

ts_nirvr_cs_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold      <- rbind(ts_nirvr_cs_cold_2019, ts_nirvr_cs_cold_2020, ts_nirvr_cs_cold_2021)
write.csv(ts_nirvr_cs_cold, paste0(out_dir, out_name, "nirvr_cs_cold.csv"), row.names = FALSE)

ts_nirvr_cf_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold      <- rbind(ts_nirvr_cf_cold_2019, ts_nirvr_cf_cold_2020, ts_nirvr_cf_cold_2021)
write.csv(ts_nirvr_cf_cold, paste0(out_dir, out_name, "nirvr_cf_cold.csv"), row.names = FALSE)

# Red
ts_red_all_2019 <- get_ts(files_2019, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2020 <- get_ts(files_2020, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2021 <- get_ts(files_2021, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all      <- rbind(ts_red_all_2019, ts_red_all_2020, ts_red_all_2021)
write.csv(ts_red_all, paste0(out_dir, out_name, "red_all.csv"), row.names = FALSE)

ts_red_cs_all_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all      <- rbind(ts_red_cs_all_2019, ts_red_cs_all_2020, ts_red_cs_all_2021)
write.csv(ts_red_cs_all, paste0(out_dir, out_name, "red_cs_all.csv"), row.names = FALSE)

ts_red_cs_cold_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold      <- rbind(ts_red_cs_cold_2019, ts_red_cs_cold_2020, ts_red_cs_cold_2021)
write.csv(ts_red_cs_cold, paste0(out_dir, out_name, "red_cs_cold.csv"), row.names = FALSE)

ts_red_cf_cold_2019 <- get_ts(files_2019, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2020 <- get_ts(files_2020, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2021 <- get_ts(files_2021, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold      <- rbind(ts_red_cf_cold_2019, ts_red_cf_cold_2020, ts_red_cf_cold_2021)
write.csv(ts_red_cf_cold, paste0(out_dir, out_name, "red_cf_cold.csv"), row.names = FALSE)

# NIR
ts_nir_all_2019 <- get_ts(files_2019, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2020 <- get_ts(files_2020, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2021 <- get_ts(files_2021, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all      <- rbind(ts_nir_all_2019, ts_nir_all_2020, ts_nir_all_2021)
write.csv(ts_nir_all, paste0(out_dir, out_name, "nir_all.csv"), row.names = FALSE)

ts_nir_cs_all_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all      <- rbind(ts_nir_cs_all_2019, ts_nir_cs_all_2020, ts_nir_cs_all_2021)
write.csv(ts_nir_cs_all, paste0(out_dir, out_name, "nir_cs_all.csv"), row.names = FALSE)

ts_nir_cs_cold_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold      <- rbind(ts_nir_cs_cold_2019, ts_nir_cs_cold_2020, ts_nir_cs_cold_2021)
write.csv(ts_nir_cs_cold, paste0(out_dir, out_name, "nir_cs_cold.csv"), row.names = FALSE)

ts_nir_cf_cold_2019 <- get_ts(files_2019, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2020 <- get_ts(files_2020, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2021 <- get_ts(files_2021, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold      <- rbind(ts_nir_cf_cold_2019, ts_nir_cf_cold_2020, ts_nir_cf_cold_2021)
write.csv(ts_nir_cf_cold, paste0(out_dir, out_name, "nir_cf_cold.csv"), row.names = FALSE)

#### SEAsia ####
out_dir    <- "G:/SIF_comps/csv/seasia"
out_name   <- "/SEAsia_2019-2021_"
files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/seasia/2019", 2019, "month")
files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/seasia/2020", 2020, "month")
files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/seasia/2021", 2021, "month")

#SIF
ts_sif_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_sif_all      <- rbind(ts_sif_all_2019, ts_sif_all_2020, ts_sif_all_2021)
write.csv(ts_sif_all, paste0(out_dir, out_name, "sif_all.csv"), row.names = FALSE)

ts_sif_cs_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all      <- rbind(ts_sif_cs_all_2019, ts_sif_cs_all_2020, ts_sif_cs_all_2021)
write.csv(ts_sif_cs_all, paste0(out_dir, out_name, "sif_cs_all.csv"), row.names = FALSE)

ts_sif_cs_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold      <- rbind(ts_sif_cs_cold_2019, ts_sif_cs_cold_2020, ts_sif_cs_cold_2021)
write.csv(ts_sif_cs_cold, paste0(out_dir, out_name, "sif_cs_cold.csv"), row.names = FALSE)

ts_sif_cf_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold      <- rbind(ts_sif_cf_cold_2019, ts_sif_cf_cold_2020, ts_sif_cf_cold_2021)
write.csv(ts_sif_cf_cold, paste0(out_dir, out_name, "sif_cf_cold.csv"), row.names = FALSE)

# NIRv
ts_nirv_all_2019 <- get_ts(files_2019, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2020 <- get_ts(files_2020, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all_2021 <- get_ts(files_2021, "NIRv", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirv_all      <- rbind(ts_nirv_all_2019, ts_nirv_all_2020, ts_nirv_all_2021)
write.csv(ts_nirv_all, paste0(out_dir, out_name, "nirv_all.csv"), row.names = FALSE)

ts_nirv_cs_all_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all      <- rbind(ts_nirv_cs_all_2019, ts_nirv_cs_all_2020, ts_nirv_cs_all_2021)
write.csv(ts_nirv_cs_all, paste0(out_dir, out_name, "nirv_cs_all.csv"), row.names = FALSE)

ts_nirv_cs_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold      <- rbind(ts_nirv_cs_cold_2019, ts_nirv_cs_cold_2020, ts_nirv_cs_cold_2021)
write.csv(ts_nirv_cs_cold, paste0(out_dir, out_name, "nirv_cs_cold.csv"), row.names = FALSE)

ts_nirv_cf_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold      <- rbind(ts_nirv_cf_cold_2019, ts_nirv_cf_cold_2020, ts_nirv_cf_cold_2021)
write.csv(ts_nirv_cf_cold, paste0(out_dir, out_name, "nirv_cf_cold.csv"), row.names = FALSE)

# NIRv R
ts_nirvr_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nirvr_all      <- rbind(ts_nirvr_all_2019, ts_nirvr_all_2020, ts_nirvr_all_2021)
write.csv(ts_nirvr_all, paste0(out_dir, out_name, "nirvr_all.csv"), row.names = FALSE)

ts_nirvr_cs_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all      <- rbind(ts_nirvr_cs_all_2019, ts_nirvr_cs_all_2020, ts_nirvr_cs_all_2021)
write.csv(ts_nirvr_cs_all, paste0(out_dir, out_name, "nirvr_cs_all.csv"), row.names = FALSE)

ts_nirvr_cs_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold      <- rbind(ts_nirvr_cs_cold_2019, ts_nirvr_cs_cold_2020, ts_nirvr_cs_cold_2021)
write.csv(ts_nirvr_cs_cold, paste0(out_dir, out_name, "nirvr_cs_cold.csv"), row.names = FALSE)

ts_nirvr_cf_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold      <- rbind(ts_nirvr_cf_cold_2019, ts_nirvr_cf_cold_2020, ts_nirvr_cf_cold_2021)
write.csv(ts_nirvr_cf_cold, paste0(out_dir, out_name, "nirvr_cf_cold.csv"), row.names = FALSE)

# Red
ts_red_all_2019 <- get_ts(files_2019, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2020 <- get_ts(files_2020, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all_2021 <- get_ts(files_2021, "RED", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_red_all      <- rbind(ts_red_all_2019, ts_red_all_2020, ts_red_all_2021)
write.csv(ts_red_all, paste0(out_dir, out_name, "red_all.csv"), row.names = FALSE)

ts_red_cs_all_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all      <- rbind(ts_red_cs_all_2019, ts_red_cs_all_2020, ts_red_cs_all_2021)
write.csv(ts_red_cs_all, paste0(out_dir, out_name, "red_cs_all.csv"), row.names = FALSE)

ts_red_cs_cold_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold      <- rbind(ts_red_cs_cold_2019, ts_red_cs_cold_2020, ts_red_cs_cold_2021)
write.csv(ts_red_cs_cold, paste0(out_dir, out_name, "red_cs_cold.csv"), row.names = FALSE)

ts_red_cf_cold_2019 <- get_ts(files_2019, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2020 <- get_ts(files_2020, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2021 <- get_ts(files_2021, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold      <- rbind(ts_red_cf_cold_2019, ts_red_cf_cold_2020, ts_red_cf_cold_2021)
write.csv(ts_red_cf_cold, paste0(out_dir, out_name, "red_cf_cold.csv"), row.names = FALSE)

# NIR
ts_nir_all_2019 <- get_ts(files_2019, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2020 <- get_ts(files_2020, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all_2021 <- get_ts(files_2021, "NIR", "month", c("LC_PERC_2020"), c(90), c("gt"))
ts_nir_all      <- rbind(ts_nir_all_2019, ts_nir_all_2020, ts_nir_all_2021)
write.csv(ts_nir_all, paste0(out_dir, out_name, "nir_all.csv"), row.names = FALSE)

ts_nir_cs_all_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all      <- rbind(ts_nir_cs_all_2019, ts_nir_cs_all_2020, ts_nir_cs_all_2021)
write.csv(ts_nir_cs_all, paste0(out_dir, out_name, "nir_cs_all.csv"), row.names = FALSE)

ts_nir_cs_cold_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold      <- rbind(ts_nir_cs_cold_2019, ts_nir_cs_cold_2020, ts_nir_cs_cold_2021)
write.csv(ts_nir_cs_cold, paste0(out_dir, out_name, "nir_cs_cold.csv"), row.names = FALSE)

ts_nir_cf_cold_2019 <- get_ts(files_2019, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2020 <- get_ts(files_2020, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2021 <- get_ts(files_2021, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold      <- rbind(ts_nir_cf_cold_2019, ts_nir_cf_cold_2020, ts_nir_cf_cold_2021)
write.csv(ts_nir_cf_cold, paste0(out_dir, out_name, "nir_cf_cold.csv"), row.names = FALSE)
