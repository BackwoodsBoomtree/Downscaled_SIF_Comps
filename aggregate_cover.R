library(terra)

# Evergreen forest percent from IGBP
mcd12   <- rast("G:/MCD12C1/MCD12C1.A2020001.006.2021362215328.hdf")[[3]]

# Reproject
my_proj <- "+proj=longlat +datum=WGS84"
mcd12 <- project(mcd12, my_proj)

# Aggregate to 1 degree
mcd12_1deg <- aggregate(mcd12, fact = 20, fun = "mean", na.rm = TRUE)

writeRaster(mcd12_1deg, "G:/MCD12C1/MCD12C1.A2020001.006.EBF.1deg.tif", overwrite = TRUE)
