library(terra)
library(viridis)

in_dir   <- "G:/GOSIF/original"
out_file <- "G:/GOSIF/16-day/GOSIF.2019-2020.1deg.16-day.nc"
factor   <- 20

gosif_2019_files <- list.files(paste0(in_dir, "/2019"), pattern = "*.tif", full.names = TRUE)
gosif_2020_files <- list.files(paste0(in_dir, "/2020"), pattern = "*.tif", full.names = TRUE)

# Create output dir if needed
if (!dir.exists(dirname(out_file))) {
  dir.create(dirname(out_file), recursive = TRUE)
}

# Create a stack for each year
for (i in 1:46) {
  gosif_2019_t <- rast(gosif_2019_files[i])
  gosif_2020_t <- rast(gosif_2020_files[i])
  
  if (i == 1) {
    gosif_2019 <- gosif_2019_t
    gosif_2020 <- gosif_2020_t
  } else {
    gosif_2019 <- c(gosif_2019, gosif_2019_t)
    gosif_2020 <- c(gosif_2020, gosif_2020_t)
  }
}

# Scale and get rid of fill values
fill_m     <- matrix(c(32766, NA, 32767, NA, -32768, NA, -32767, NA, -32766, NA), ncol = 2, byrow = TRUE)
gosif_2019 <- classify(gosif_2019, fill_m)
gosif_2020 <- classify(gosif_2020, fill_m)
gosif_2019 <- gosif_2019 * 0.0001
gosif_2020 <- gosif_2020 * 0.0001

# Aggregate spatially and project
gosif_2019 <- aggregate(gosif_2019, fact = factor, fun = "mean", na.rm = TRUE)
gosif_2020 <- aggregate(gosif_2020, fact = factor, fun = "mean", na.rm = TRUE)
gosif_2019 <- project(gosif_2019, "+proj=longlat +datum=WGS84")
gosif_2020 <- project(gosif_2020, "+proj=longlat +datum=WGS84")

# Aggregate temporally
for (i in seq(1, 46, 2)) {
  gosif_2019_t <- mean(gosif_2019[[i : (i + 1)]], na.rm = TRUE)
  gosif_2020_t <- mean(gosif_2020[[i : (i + 1)]], na.rm = TRUE)
  
  if (i == 1) {
    gosif_2019_16 <- gosif_2019_t
    gosif_2020_16 <- gosif_2020_t
  } else {
    gosif_2019_16 <- c(gosif_2019_16, gosif_2019_t)
    gosif_2020_16 <- c(gosif_2020_16, gosif_2020_t)
  }
}

gosif <- c(gosif_2019_16, gosif_2020_16)

# Times
date_list <- c(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by = 16),
               seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 16))
for (t in 1:46) {
  time(gosif[[t]]) <- date_list[t]
}

# Write out
writeCDF(gosif, out_file,
         varname = "gosif", longname = "GOSIF 16-day Mean", unit = "mW/m-2/sr/nm",
         missval = -9999, overwrite = TRUE, compression = 4)

