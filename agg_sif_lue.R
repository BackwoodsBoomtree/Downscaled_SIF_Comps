library(terra)
library(viridis)


in_file  <- "G:/SIF-LUE/orig/GOME_PK_dcSIF_005deg_8day_2018.nc"
out_file <- "G:/SIF-LUE/1deg/GOME_PK_dcSIF_005deg_8day_2018_1deg.nc"
factor   <- 20

sif_lue  <- rast(in_file)

sif_lue  <- aggregate(sif_lue, fact = factor, fun = "mean", na.rm = TRUE)

sif_lue <- gosif

# Create output dir if needed
if (!dir.exists(dirname(out_file))) {
  dir.create(dirname(out_file), recursive = TRUE)
}

# Times
date_list <- seq(as.Date("2018-01-01"), as.Date("2018-12-31"), by = 8)

for (t in 1:46) {
  time(sif_lue[[t]]) <- date_list[t]
}

# Write out
writeCDF(sif_lue, out_file,
         varname = "sif_lue", longname = "SIF-LUE 8-day Mean", unit = "mW/m-2/sr/nm",
         missval = -9999, overwrite = TRUE, compression = 4)
