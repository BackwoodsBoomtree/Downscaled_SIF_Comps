library(terra)
library(ncdf4)
library(raster)
library(viridis)
library(rgdal)
library(rgeos)
library(RColorBrewer)

#### Output PDF name ####
out_name   <- "G:/SIF_comps/figs/v2/comps_samerica_map_3yr_cold_all_black_v2.pdf"

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

#### Grab the data ####
files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2019", 2019, "month")
files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2020", 2020, "month")
files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/amazon/2021", 2021, "month")

ts_sif_cs_all_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_sif_cs_all      <- c(ts_sif_cs_all_2019$Mean, ts_sif_cs_all_2020$Mean, ts_sif_cs_all_2021$Mean)

ts_sif_cs_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_sif_cs_cold      <- c(ts_sif_cs_cold_2019$Mean, ts_sif_cs_cold_2020$Mean, ts_sif_cs_cold_2021$Mean)

ts_sif_cf_cold_2019 <- get_ts(files_2019, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2020 <- get_ts(files_2020, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold_2021 <- get_ts(files_2021, "SIF_743", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_sif_cf_cold      <- c(ts_sif_cf_cold_2019$Mean, ts_sif_cf_cold_2020$Mean, ts_sif_cf_cold_2021$Mean)

ts_nirv_cs_all_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirv_cs_all      <- c(ts_nirv_cs_all_2019$Mean, ts_nirv_cs_all_2020$Mean, ts_nirv_cs_all_2021$Mean)

ts_nirv_cs_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirv_cs_cold      <- c(ts_nirv_cs_cold_2019$Mean, ts_nirv_cs_cold_2020$Mean, ts_nirv_cs_cold_2021$Mean)

ts_nirv_cf_cold_2019 <- get_ts(files_2019, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2020 <- get_ts(files_2020, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold_2021 <- get_ts(files_2021, "NIRv", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirv_cf_cold      <- c(ts_nirv_cf_cold_2019$Mean, ts_nirv_cf_cold_2020$Mean, ts_nirv_cf_cold_2021$Mean)

ts_nirvr_cs_all_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nirvr_cs_all      <- c(ts_nirvr_cs_all_2019$Mean, ts_nirvr_cs_all_2020$Mean, ts_nirvr_cs_all_2021$Mean)

ts_nirvr_cs_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nirvr_cs_cold      <- c(ts_nirvr_cs_cold_2019$Mean, ts_nirvr_cs_cold_2020$Mean, ts_nirvr_cs_cold_2021$Mean)

ts_nirvr_cf_cold_2019 <- get_ts(files_2019, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2020 <- get_ts(files_2020, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold_2021 <- get_ts(files_2021, "NIRv_RAD", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nirvr_cf_cold      <- c(ts_nirvr_cf_cold_2019$Mean, ts_nirvr_cf_cold_2020$Mean, ts_nirvr_cf_cold_2021$Mean)

ts_red_cs_all_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_red_cs_all      <- c(ts_red_cs_all_2019$Mean, ts_red_cs_all_2020$Mean, ts_red_cs_all_2021$Mean)

ts_red_cs_cold_2019 <- get_ts(files_2019, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2020 <- get_ts(files_2020, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold_2021 <- get_ts(files_2021, "RED", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_red_cs_cold      <- c(ts_red_cs_cold_2019$Mean, ts_red_cs_cold_2020$Mean, ts_red_cs_cold_2021$Mean)

ts_red_cf_cold_2019 <- get_ts(files_2019, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2020 <- get_ts(files_2020, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold_2021 <- get_ts(files_2021, "RED", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_red_cf_cold      <- c(ts_red_cf_cold_2019$Mean, ts_red_cf_cold_2020$Mean, ts_red_cf_cold_2021$Mean)

ts_nir_cs_all_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "LC_PERC_2020"), c(0, 90), c("eq", "gt"))
ts_nir_cs_all      <- c(ts_nir_cs_all_2019$Mean, ts_nir_cs_all_2020$Mean, ts_nir_cs_all_2021$Mean)

ts_nir_cs_cold_2019 <- get_ts(files_2019, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2020 <- get_ts(files_2020, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold_2021 <- get_ts(files_2021, "NIR", "month", c("cloud_fraction_L2", "phase_angle", "LC_PERC_2020"), c(0, 20, 90), c("eq", "gt", "gt"))
ts_nir_cs_cold      <- c(ts_nir_cs_cold_2019$Mean, ts_nir_cs_cold_2020$Mean, ts_nir_cs_cold_2021$Mean)

ts_nir_cf_cold_2019 <- get_ts(files_2019, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2020 <- get_ts(files_2020, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold_2021 <- get_ts(files_2021, "NIR", "month", c("phase_angle", "LC_PERC_2020"), c(20, 90), c("gt", "gt"))
ts_nir_cf_cold      <- c(ts_nir_cf_cold_2019$Mean, ts_nir_cf_cold_2020$Mean, ts_nir_cf_cold_2021$Mean)

# convert red to absorbance
ts_red_cs_all  <- 1-ts_red_cs_all
ts_red_cs_cold <- 1-ts_red_cs_cold
ts_red_cf_cold <- 1-ts_red_cf_cold

#### Get shapes and forest cover ####

coastlines     <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
roi            <- vect("F:/BACKUPS/Russell/Projects/Amazon/Amazon_poly.shp") # Amazon
samerica_ext   <- extent(c(-82,-34,-20,13))

mcd12_majority <- rast("G:/MCD12C1/2020/reprocessed/percent/MCD12C1.A2020001.006.Percent_LC_03.tif")
mcd12_majority <- terra::crop(mcd12_majority, roi, mask = TRUE)
mcd12_majority[mcd12_majority < 90]  <- NA

#### Plot Settings ####
x           <- 1:36
xlabs       <- c("Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct", "Jan", "Apr", "Jul", "Oct")
y_lab_map   <- list(bquote("Forest Cover"), bquote("Percent  (%)"))
y_lab_sif   <- list(bquote("SIF"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_n     <- list(bquote("Number of "), bquote("Soundings"))
y_lab_nirv  <- list(bquote("NIRv"), bquote("(Reflectance)"))
y_lab_nirvr <- list(bquote("NIRv Radiance"), bquote("(mW/m"^"2"*"/sr/nm)"))
y_lab_665   <- list(bquote("Red"), bquote("Absorbance"))
y_lab_781   <- list(bquote("NIR"), bquote("Reflectance"))

mag.cols <- magma(7)
vir.cols <- viridis(7)
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(11))

#### Plot ####
cairo_pdf(out_name, width = 7.5, height = 4.25)

par(mfrow = c(3, 2), oma=c(2.0,0.1,1.25,0.1), bg = "black")

# Map
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xlim = c(-85, -30), ylim = c(-22, 7), mar = c(0,6,0,0.5))
plot(mcd12_majority, col = map.cols, add = TRUE, legend = FALSE)
lines(roi, col = "black")
mtext(3, text = "Amazon Tropical Forest", col = "white")
box(col = "white")
# Legend
plot(raster(mcd12_majority), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = do.call(expression, y_lab_map), side = 2, line = c(3.0, 1.0), col = "white"),
     axis.args = list(line = -1.75, cex.axis=1, tick=F, at=c(90, 100), labels=c("90","100"), col.axis = "white", hadj = 1),
     smallplot=c(0.175,0.200,0.10,0.90)); par(mar = par("mar"))

# Line Plots
# SIF
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_sif_cs_cold, col = mag.cols[4], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_sif_cs_all, ts_sif_cf_cold) - 0.10 * min(ts_sif_cs_all, ts_sif_cf_cold),
              max(ts_sif_cs_all, ts_sif_cf_cold) + 0.10 * max(ts_sif_cs_all, ts_sif_cf_cold)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_sif_cs_cold, col = mag.cols[4], lwd = 1.5)
lines(x, ts_sif_cf_cold, col = mag.cols[4], lwd = 1.5, lty = 2)
lines(x, ts_sif_cs_all, col = mag.cols[4], lwd = 1.5, lty = 3)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_sif), col = "white", line = c(4.25, 2.25))
legend("topleft", legend=c("Clear Sky (w/ Hotspot)", "Clear Sky (No Hotspot)", "Cloud <0.20 (No Hotspot)"), col=c("white", "white", "white"),
       lty=c(3, 1, 2), box.col = "white", text.col = "white", horiz = FALSE, y.intersp = 1, cex = 0.75)
box(col = "white")

# RED
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_red_cs_cold, col = vir.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_red_cs_cold, ts_red_cf_cold, ts_red_cs_all) - 0.01,
              max(ts_red_cs_cold, ts_red_cf_cold, ts_red_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_red_cs_cold, col = vir.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_red_cf_cold, col = vir.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_red_cs_all, col = vir.cols[6], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_665), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRv
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirv_cs_cold, col = mag.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirv_cs_cold, ts_nirv_cf_cold) - 0.10 * min(ts_nirv_cs_cold, ts_nirv_cf_cold),
              max(ts_nirv_cs_cold, ts_nirv_cf_cold) + 0.10 * max(ts_nirv_cs_cold, ts_nirv_cf_cold)))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirv_cs_cold, col = mag.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nirv_cf_cold, col = mag.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_nirv_cs_all, col = mag.cols[5], lty = 3, lwd = 1.5)
axis(1, tck = 0.03, labels = FALSE, at = x, col.axis = "white", col = "white")
axis(1, tck = 0.06, labels = FALSE, at = seq(1, 36, by = 3), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirv), col = "white", line = c(4.25, 2.25))
box(col = "white")

# REF 781
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nir_cs_cold, col = vir.cols[5], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nir_cs_cold, ts_nir_cf_cold, ts_nir_cs_all) - 0.01,
              max(ts_nir_cs_cold, ts_nir_cf_cold, ts_nir_cs_all) + 0.01))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nir_cs_cold, col = vir.cols[5], lty = 1, lwd = 1.5)
lines(x, ts_nir_cf_cold, col = vir.cols[5], lty = 2, lwd = 1.5)
lines(x, ts_nir_cs_all, col = vir.cols[5], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_781), col = "white", line = c(4.25, 2.25))
box(col = "white")

# NIRV Rad
op <- par(mar = c(0,6,0,0.5), bg = "black")
plot(x, ts_nirvr_cs_cold, col = mag.cols[6], type = "l", axes = FALSE, lwd = 1.5, xaxs="i",
     ylim = c(min(ts_nirvr_cs_cold, ts_nirvr_cf_cold, ts_nirvr_cs_all) - 2,
              max(ts_nirvr_cs_cold, ts_nirvr_cf_cold, ts_nirvr_cs_all) + 2))
rect(13, 0, 25, 100, col = rgb(0.30,0.30,0.30), border = NA)
lines(x, ts_nirvr_cs_cold, col = mag.cols[6], lty = 1, lwd = 1.5)
lines(x, ts_nirvr_cf_cold, col = mag.cols[6], lty = 2, lwd = 1.5)
lines(x, ts_nirvr_cs_all, col = mag.cols[6], lty = 3, lwd = 1.5)
axis(1, labels = FALSE, tck = 0.03, at = x,  mgp=c(3, 0.1, 0), col.axis = "white", col = "white")
axis(1, labels = xlabs, tck = 0.06, at = seq(1, 36, by = 3), mgp=c(3, 0.1, 0), col.axis = "white", col = "white", cex.axis = 0.85)
axis(1, labels = c("2019", "2020", "2021"), tck = FALSE, at = c(6.5, 18.5, 30.5), mgp=c(3, 1.1, 0), col.axis = "white", col = "white")
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), col.axis = "white", col = "white", las = 2)
mtext(2, text = do.call(expression, y_lab_nirvr), col = "white", line = c(4.25, 2.25))
box(col = "white")

dev.off()

