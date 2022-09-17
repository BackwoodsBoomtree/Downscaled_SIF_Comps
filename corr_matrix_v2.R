library(corrplot)

out_name    <- "G:/SIF_comps/figs/v2/corr_matrix_v2.pdf"

# P function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

### BASIN WIDE ####
# SIF data
tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_sifd_mean_qc_nadir.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_mean_qc_nadir.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2.csv", header = TRUE)

tropo_sif_mean <- tropo_sif_data$Mean
oco2_sif_mean  <- oco2_sif_data$Mean
oco3_sif_mean  <- oco3_sif_data$Mean
gome2_sif_mean <- gome2_sif_data$Mean

# Oct - March
tropo_sif_mean_2 <- c(tropo_sif_data$Mean[9:12], tropo_sif_data$Mean[1:4])
oco2_sif_mean_2  <- c(oco2_sif_data$Mean[9:12], oco2_sif_data$Mean[1:4])
oco3_sif_mean_2  <- c(oco3_sif_data$Mean[9:12], oco3_sif_data$Mean[1:4])
gome2_sif_mean_2 <- c(gome2_sif_data$Mean[9:12], gome2_sif_data$Mean[1:4])

# MCD43C4 data
get_annual_means <- function(ts_data) {
  df <- as.data.frame(split(ts_data, ceiling(seq_along(ts_data)/12)))
  means <- rowMeans(df)
  return(means)
}

mcd_nirv <- get_annual_means(read.csv("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv", header = TRUE)[,1]) / 10000

# Oct - March
mcd_nirv_2 <- c(mcd_nirv[9:12], mcd_nirv[1:4])

df <- cbind(tropo_sif_mean, oco2_sif_mean, oco3_sif_mean, gome2_sif_mean, mcd_nirv)

# Oct - March
df <- cbind(tropo_sif_mean_2, oco2_sif_mean_2, oco3_sif_mean_2, gome2_sif_mean_2, mcd_nirv_2)
colnames(df) <- c("TROPOMI SIF", "OCO-2 SIF", "OCO-3 SIF", "GOME-2 SIF", "NIRv")

### TOWER DATA ####

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

k34_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2019", 2019, "month")
k34_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2020", 2020, "month")
k34_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/k34/2021", 2021, "month")

k67_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2019", 2019, "month")
k67_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2020", 2020, "month")
k67_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/k67/2021", 2021, "month")

rja_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2019", 2019, "month")
rja_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2020", 2020, "month")
rja_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/rja/2021", 2021, "month")

cax_files_2019 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2019", 2019, "month")
cax_files_2020 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2020", 2020, "month")
cax_files_2021 <- file_df("G:/TROPOMI/esa/extracted/ebf/cax/2021", 2021, "month")

# K34 SIF Corr
k34_sif_cor_2019 <- get_ts(k34_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_2020 <- get_ts(k34_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_2021 <- get_ts(k34_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k34_sif_cor_df   <- rbind(k34_sif_cor_2019, k34_sif_cor_2020, k34_sif_cor_2021)

# K67 SIF Corr
k67_sif_cor_2019 <- get_ts(k67_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_2020 <- get_ts(k67_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_2021 <- get_ts(k67_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
k67_sif_cor_df   <- rbind(k67_sif_cor_2019, k67_sif_cor_2020, k67_sif_cor_2021)

# RJA SIF Corr
rja_sif_cor_2019 <- get_ts(rja_files_2019, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_2020 <- get_ts(rja_files_2020, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_2021 <- get_ts(rja_files_2021, "SIF_Corr_743", "month", c("cloud_fraction_L2", "LC_PERC_2020", "phase_angle"), c(0.80, 90, 0), c("lt", "gt", "gt"))
rja_sif_cor_df   <- rbind(rja_sif_cor_2019, rja_sif_cor_2020, rja_sif_cor_2021)

# CAX SIF Corr
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



### All SITES

cax_gep_na  <- c(NA, cax_gep[1], NA, cax_gep[2:3], NA, cax_gep[4:9])
cax_pc_na   <- c(NA, cax_pc[1], NA, cax_pc[2:3], NA, cax_pc[4:9])
cax_par_na  <- c(NA, cax_wu_par[1], NA, cax_wu_par[2:3], NA, cax_wu_par[4:9])
cax_pre_na  <- c(NA, cax_pre[1], NA, cax_pre[2:3], NA, cax_pre[4:9])
rja_gep_na  <- c(NA, rja_gep, NA)
rja_pc_na   <- c(NA, rja_pc, NA)
rja_par_na  <- c(NA, rja_wu_par, NA)
rja_pre_na  <- c(NA, rja_pre, NA)

sif_cat  <- c(k34_sif_cor, cax_sif_cor, k67_sif_cor, rja_sif_cor)
pc_cat   <- c(k34_pc, cax_pc_na, k67_pc, rja_pc_na)
gep_cat  <- c(k34_gep, cax_gep_na, k67_gep, rja_gep_na)
nirv_cat <- c(k34_mod_nirv, cax_mod_nirv, k67_mod_nirv, rja_mod_nirv)
par_cat  <- c(k34_wu_par, cax_par_na, k67_wu_par, rja_par_na)
pre_cat  <- c(k34_pre, cax_pre, k67_pre, rja_pre)

df <- cbind(gep_cat, pc_cat, sif_cat, nirv_cat, pre_cat, par_cat)
df <- na.omit(df)
colnames(df) <- c("GEP", "PC", "SIF", "NIRv", "Precip", "PAR")

df_cor <- round(cor(df),  2)
p.mat  <- cor.mtest(df)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

cairo_pdf(out_name, width = 6, height = 5)

corrplot(df_cor, method="color", col=col(40),  
         order="original", cl.cex = 1,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "pch",
         pch.cex = 6, pch.col = "#00000050",
         diag=TRUE)

dev.off()


### K34

df <- cbind(k34_gep, k34_pc, k34_mod_nirv, k34_sif_cor, k34_pre, k34_wu_evi, k34_wu_lai, k34_wu_par)
colnames(df) <- c("GEP", "PC", "NIRv", "SIF", "Precip", "EVI", "LAI", "PAR")

df_cor <- round(cor(df),  2)
p.mat  <- cor.mtest(df)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(df_cor, method="color", col=col(200),  
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01,
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)

### K67

df <- cbind(k67_gep, k67_pc, k67_mod_nirv, k67_sif_cor, k67_pre, k67_wu_evi, k67_wu_lai, k67_wu_par)
colnames(df) <- c("GEP", "PC", "NIRv", "SIF", "Precip", "EVI", "LAI", "PAR")

df_cor <- round(cor(df),  2)
p.mat  <- cor.mtest(df)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(df_cor, method="color", col=col(200),  
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)

### K34 and K67

df_34 <- cbind(k34_gep, k34_pc, k34_sif_cor, k34_mod_nirv, k34_wu_evi, k34_pre, k34_wu_par)
df_67 <- cbind(k67_gep, k67_pc, k67_sif_cor, k67_mod_nirv, k67_wu_evi, k67_pre, k67_wu_par)
df    <- rbind(df_34, df_67)
colnames(df) <- c("GEP", "PC", "SIF", "NIRv", "EVI", "Precip", "PAR")

df_cor <- round(cor(df),  2)
p.mat  <- cor.mtest(df)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(df_cor, method="color", col=col(200),  
         order="original",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "pch", pch = "X", pch.cex = 2,
         diag=TRUE 
)
