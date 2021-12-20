library(terra)
library(viridis)


in_file  <- "G:/GOSIF/2020/5km/GOSIF.2020.5km.nc"
out_file <- "G:/GOSIF/2020/1deg/GOSIF.2020.1deg.nc"
factor   <- 20

gosif    <- rast(in_file)

gosif    <- aggregate(gosif, fact = factor, fun = "mean", na.rm = TRUE)

# Create output dir if needed
if (!dir.exists(dirname(out_file))) {
  dir.create(dirname(out_file), recursive = TRUE)
}

# Times
date_list <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = 8)

for (t in 1:46) {
  time(gosif[[t]]) <- date_list[t]
}

# Write out
writeCDF(gosif, out_file,
         varname = "gosif", longname = "GOSIF 8-day Mean", unit = "mW/m-2/sr/nm",
         missval = -9999, overwrite = TRUE, compression = 4)

### Aggregate spatially in bash using agg_gosif.py
