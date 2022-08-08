library(terra)
library(tools)

in_files <- list.files("G:/SIF_OCO2_005/original", pattern = "*.nc", full.names = TRUE)
out_dir  <- "G:/SIF_OCO2_005/1deg"
factor   <- 20

# Create output dir if needed
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

for (i in 1:length(in_files)) {
  sun <- rast(in_files[i], subds = "sif_ann")
  sun <- aggregate(sun, fact = factor, fun = "mean", na.rm = TRUE)
  sun <- project(sun, "+proj=longlat +datum=WGS84")
  
  out_file <- paste0(out_dir, "/", file_path_sans_ext(basename(in_files[i])), "_1deg.nc")
  writeCDF(sun, out_file,
           varname = "sif_ann", longname = "sif_ann", unit = "mW/m-2/sr/nm",
           missval = -9999, overwrite = TRUE, compression = 4)
}
