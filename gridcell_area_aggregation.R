
### This script is used to account for area when aggregating to coarser resolutions ###

cf_2018 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2018/TROPOMI.ESA.SIF.2018.global.monthly.1deg.CF20.nc"
test    <- rast(cf_2018, subds = "SIF_743")

size.grid.small <- cellSize(test, unit = "m")

total     <- test * size.grid

test.agg.sum <- aggregate(total, fact = 2, fun = "sum", na.rm = TRUE)

size.grid.large <- cellSize(test.agg.sum, unit = "m")

final <- test.agg.sum / size.grid.large

plot(final[[1]])
