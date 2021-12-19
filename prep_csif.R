library(terra)
library(viridis)


file_list <- list.files("G:/CSIF/CMG4day/2020", pattern = "*.nc$", full.names = TRUE, recursive = TRUE)

### Do instant and clear variables seperately (uses lots of memory)

doy_list  <- seq(1, 365, 8)
doy_count <- 1

# Get 8-day means
for (f in seq(1, 92, 2)) {
  first  <- rast(file_list[f])
  second <- rast(file_list[(f + 1)])
  
  m <- (first + second) / 2
  
  doy       <- sprintf("%03d", doy_list[doy_count])
  doy_count <- doy_count + 1
  
  # Place inst and clear rasters into different files

  writeCDF(m[[1]], paste0("G:/CSIF/8-day/inst/CSIF.inst.2020.", doy, ".v2.nc"),
           varname = "csif_inst", longname = "CSIF Instant 8-day Mean", unit = "mW/m-2/sr/nm",
           missval = -9999, overwrite = TRUE, compression = 4)
  
  writeCDF(m[[2]], paste0("G:/CSIF/8-day/clear/CSIF.clear.2020.", doy, ".v2.nc"),
           varname = "csif_clear", longname = "CSIF Daily 8-day Mean", unit = "mW/m-2/sr/nm",
           missval = -9999, overwrite = TRUE, compression = 4)
}



# Aggregate spatially in bash using these commands:

inst_list  <- list.files("G:/CSIF/8-day/inst", pattern = "*.nc$", full.names = TRUE, recursive = TRUE)
clear_list <- list.files("G:/CSIF/8-day/clear", pattern = "*.nc$", full.names = TRUE, recursive = TRUE)

for (f in 1:length(inst_list)) {
  
}

system(sprintf("cdo -b f32 remapcon,gridfile_1.0.txt /mnt/g/ChloFluo/input/Temp/daytime/25km/8day/Temp.mean.daytime.8day.era.25km.2019.nc /mnt/g/ChloFluo/input/Temp/daytime/1deg/8day/Temp.mean.daytime.8day.era.1deg.2019.nc"))