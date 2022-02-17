

cf_2018 <- "G:/TROPOMI/esa/gridded/1deg/monthly/2018/TROPOMI.ESA.SIF.2018.global.monthly.1deg.CF20.nc"
test    <- rast(cf_2018, subds = "SIF_743")

size.grid.small <- cellSize(test, unit = "m")

total     <- test * size.grid

test.agg.sum <- aggregate(total, fact = 2, fun = "sum", na.rm = TRUE)

size.grid.large <- cellSize(test.agg.sum, unit = "m")

final <- test.agg.sum / size.grid.large

plot(final[[1]])


test.agg.m  <- aggregate(test, fact = 2, fun = "mean", na.rm = TRUE)
test.agg.wm <- aggregate(test, fact = 2, fun = "weighted.mean", na.rm = TRUE, size.grid)

test.agg.m
test.agg.wm

s          <- rast(nrows=90, ncols=180, xmin=-180, xmax=180, ymin=-90, ymax=90)
test.resam <- resample(test, s, method="bilinear")

test.resam

plot(test[[1]])
plot(test.agg.m[[1]])
plot(test.resam[[1]])

size.grid <- cellSize(test, unit = "m")

agg.45 <- aggregate(total, fact = c(4, 5), sun = "sum", na.rm = TRUE)
