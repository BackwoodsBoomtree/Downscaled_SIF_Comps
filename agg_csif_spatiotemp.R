library(terra)

in_dir   <- "G:/CSIF/CMG4day"
out_file <- "G:/CSIF/1deg/CSIF.daily.2019-2020.1deg.16-day.nc"
factor   <- 20

csif_2019_files <- list.files(paste0(in_dir, "/2019"), pattern = "*.nc", full.names = TRUE)
csif_2020_files <- list.files(paste0(in_dir, "/2020"), pattern = "*.nc", full.names = TRUE)

# Create output dir if needed
if (!dir.exists(dirname(out_file))) {
  dir.create(dirname(out_file), recursive = TRUE)
}

# Create a stack for each year
for (i in 1:92) {
  csif_2019_t <- rast(csif_2019_files[i], subds = "clear_daily_SIF")
  csif_2020_t <- rast(csif_2020_files[i], subds = "clear_daily_SIF")
  
  if (i == 1) {
    csif_2019 <- csif_2019_t
    csif_2020 <- csif_2020_t
  } else {
    csif_2019 <- c(csif_2019, csif_2019_t)
    csif_2020 <- c(csif_2020, csif_2020_t)
  }
}

# Aggregate spatially and project
csif_2019 <- aggregate(csif_2019, fact = factor, fun = "mean", na.rm = TRUE)
csif_2020 <- aggregate(csif_2020, fact = factor, fun = "mean", na.rm = TRUE)
csif_2019 <- project(csif_2019, "+proj=longlat +datum=WGS84")
csif_2020 <- project(csif_2020, "+proj=longlat +datum=WGS84")

# Aggregate temporally
for (i in seq(1, 92, 4)) {
  csif_2019_t <- mean(csif_2019[[i : (i + 3)]], na.rm = TRUE)
  csif_2020_t <- mean(csif_2020[[i : (i + 3)]], na.rm = TRUE)
  
  if (i == 1) {
    csif_2019_16 <- csif_2019_t
    csif_2020_16 <- csif_2020_t
  } else {
    csif_2019_16 <- c(csif_2019_16, csif_2019_t)
    csif_2020_16 <- c(csif_2020_16, csif_2020_t)
  }
}

csif <- c(csif_2019_16, csif_2020_16)

# Times
date_list <- c(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = 16),
               seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 16))

for (t in 1:46) {
  time(csif[[t]]) <- date_list[t]
}

# Write out
writeCDF(csif, out_file,
         varname = "clear_daily_SIF", longname = "CSIF 16-day Mean", unit = "mW/m-2/sr/nm",
         missval = -9999, overwrite = TRUE, compression = 4)

