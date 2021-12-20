library(raster)

gosif_file <- "G:/GOSIF/2020/1deg/GOSIF.2020.1deg.nc"
sif_file <- "G:/TROPOMI/esa/gridded/1deg/8day/TROPOMI.ESA.SIF.201805-202109.global.8day.1deg.clearsky.nc"
sif_rang   <- 78:123
y_name     <- "gosif"
x_name     <- "SIF_Corr_743"
out_dir    <- "G:/SIF_comps/gosif/2020/1deg/raster_regressions"
out_name   <- "GOSIF_vs_TROPO_SIF.1deg.clearsky.2020"
f_name     <- NA # Filter by value. Example, error, std, or n. If none use NA.
f_thresh   <- 30  # Values => will be kept


y <- stack(gosif_file, varname = y_name)
x <- stack(sif_file, varname = x_name)[[sif_rang]]


rastlm <- function(x) {
  full <- length(x)
  half <- full / 2
  
  if (all(is.na(x[1:half])) || all(is.na(x[(half + 1):full]))){ 
    
    return(c(NA,NA,NA,NA,NA,NA))
    
  } else { 
    reg       <- lm(x[1:half] ~ x[(half +1):full])
    s         <- summary(reg)
    r2        <- s$r.squared
    pval      <- s$coefficients[8]
    slope     <- s$coefficients[2]
    intercept <- s$coefficients[1]
    rmse      <- sqrt(mean(s$residuals^2))
    n         <- nobs(reg)
    
    return(c(r2, pval, slope, intercept, rmse, n)) 
  }
}

rast_reg <- function(y_stack, x_stack, out_dir, out_name) {
  
  # Create output dir if needed
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  y_stack <- mask(y_stack, x_stack)
  x_stack <- mask(x_stack, y_stack)
  
  # Filter as needed
  if (!is.na(f_name)) {
    print(paste0("Filtering data using: ", f_name))
    print(paste0("Filter threshold is: ", f_thresh))
    
    f <- brick(in_file, varname = f_name)
    f[f < f_thresh] <- NA
    
    y <- mask(y, f)
    x <- mask(x, f)
    
  }
  
  # Combine stack into single stack. lm convention is y~x
  yx <- stack(y, x)
  
  beginCluster(12)
  lm.result <- clusterR(yx, calc, args = list(fun = rastlm))
  endCluster()
  
  writeRaster(lm.result[[1]], paste0(out_dir, "/", out_name, "_Rsquare.tif"), overwrite = TRUE)
  writeRaster(lm.result[[2]], paste0(out_dir, "/", out_name, "_Pval.tif"), overwrite = TRUE)
  writeRaster(lm.result[[3]], paste0(out_dir, "/", out_name, "_Slope.tif"), overwrite = TRUE)
  writeRaster(lm.result[[4]], paste0(out_dir, "/", out_name, "_Intercept.tif"), overwrite = TRUE)
  writeRaster(lm.result[[5]], paste0(out_dir, "/", out_name, "_RMSE.tif"), overwrite = TRUE)
  writeRaster(lm.result[[6]], paste0(out_dir, "/", out_name, "_Nobs.tif"), overwrite = TRUE)
  
  remove(x_brick, y_brick, yx) # get it out of memory
  
}

rast_reg(y, x, out_dir, out_name)