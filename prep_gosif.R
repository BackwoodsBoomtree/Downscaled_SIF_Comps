library(terra)
library(viridis)


file_list <- list.files("G:/GOSIF/2020", pattern = "*.tif$", full.names = TRUE, recursive = TRUE)
out_dir   <- "G:/GOSIF/2020/5km"

gosif     <- rast(file_list)

# Set fill values to NA
# Fill values: 32767 (water bodies) and 32766 (lands under snow/ice throughout the year)
gosif[gosif >= 32766] <- NA

# Scale
gosif <- gosif * 0.0001

# Times
date_list <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 8)

for (t in 1:46) {
  time(gosif[[t]]) <- date_list[t]
}

# Write out
writeCDF(gosif, paste0(out_dir, "/GOSIF.2020.5km.nc"),
         varname = "gosif", longname = "GOSIF 8-day Mean", unit = "mW/m-2/sr/nm",
         missval = -9999, overwrite = TRUE, compression = 4)

### Aggregate spatially in bash using agg_gosif.R
