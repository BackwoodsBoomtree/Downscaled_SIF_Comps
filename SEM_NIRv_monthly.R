
library(raster)
library(terra)
library(viridis)
library(rgdal)
library(pals)

options(scipen=999)

#### Load Map ####

coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
class(coastlines)
extent(coastlines)
crs(coastlines)

# Round up
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# Get list of colors for barplot
col_list <- function(x) {
  
  l <- c()
  
  for (i in 1:length(x)) {
    if (!is.na(x[i])) {
      if (x[i] <= 0.1) {
        l <- c(l, n.col[1])
      } else if (x[i] <= 0.2) {
        l <- c(l, n.col[2])
      } else if (x[i] <= 0.3) {
        l <- c(l, n.col[3])
      } else if (x[i] <= 0.4) {
        l <- c(l, n.col[4])
      } else if (x[i] <= 0.5) {
        l <- c(l, n.col[5])
      } else if (x[i] <= 0.6) {
        l <- c(l, n.col[6])
      } else if (x[i] <= 0.7) {
        l <- c(l, n.col[7])
      } else if (x[i] <= 0.8) {
        l <- c(l, n.col[8])
      } else if (x[i] <= 0.9) {
        l <- c(l, n.col[9])
      } else if (x[i] <= 1.0) {
        l <- c(l, n.col[10])
      }
    } else {
      l <- c(l, NA)
    }
  }
  return(l)
}

# Back out error for NIRv when error of bands is known
calc_err_nirv <- function (nir, red, nir_err, red_err) {
  
  # Define my function (equation for NIRv)
  f  <- expression(((nir - red) / (nir + red)) * nir)
  
  # Get expressions for partial derivatives of each function variable
  dnir <- D(f, 'nir')
  dred <- D(f, 'red')
  
  # Propagate error which equals the sum the derivative^2 * error for each variable
  err_prop <- (eval(dnir)^2 * nir_err) + (eval(dred)^2 * red_err)
  
  return(err_prop)
}

# Mask Raster
m  <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

### Data ####

# Clear Sky NIRv
n_cs_mar          <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "n")[[3]], m)
std_cs_mar_ref665 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "REF_665_std")[[3]], m)
std_cs_mar_ref781 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "REF_781_std")[[3]], m)

cs_mar_ref665 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "REF_665")[[3]], m)
cs_mar_ref781 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "REF_781")[[3]], m)

cs_mar_nirvr  <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "NIRv_RAD")[[3]], m)
cs_mar_nirv   <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "NIRv")[[3]], m)

sem_cs_mar_ref665 <- std_cs_mar_ref665 / sqrt(n_cs_mar)
sem_cs_mar_ref781 <- std_cs_mar_ref781 / sqrt(n_cs_mar)

# CF20 NIRv
n_cf20_mar          <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "n")[[3]], m)
std_cf20_mar_ref665 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "REF_665_std")[[3]], m)
std_cf20_mar_ref781 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "REF_781_std")[[3]], m)

cf20_mar_ref665 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "REF_665")[[3]], m)
cf20_mar_ref781 <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "REF_781")[[3]], m)

cf20_mar_nirvr  <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "NIRv_RAD")[[3]], m)
cf20_mar_nirv   <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF20.nc", subds = "NIRv")[[3]], m)

sem_cf20_mar_ref665 <- std_cf20_mar_ref665 / sqrt(n_cf20_mar)
sem_cf20_mar_ref781 <- std_cf20_mar_ref781 / sqrt(n_cf20_mar)

# Calc SEM for nirv

sem_cs_mar_nirv      <- calc_err_nirv(cs_mar_ref781, cs_mar_ref665, sem_cs_mar_ref781, sem_cs_mar_ref665)
sem_cf20_mar_nirv    <- calc_err_nirv(cf20_mar_ref781, cf20_mar_ref665, sem_cf20_mar_ref781, sem_cf20_mar_ref665)

sem_cs_mar_nirv_perc   <- sem_cs_mar_nirv / cs_mar_nirv * 100
sem_cf20_mar_nirv_perc <- sem_cf20_mar_nirv / cf20_mar_nirv * 100

sem_cs_mar_nirv_perc[sem_cs_mar_nirv_perc < 0]       <- 0
sem_cs_mar_nirv_perc[sem_cs_mar_nirv_perc > 100]     <- 100
sem_cf20_mar_nirv_perc[sem_cf20_mar_nirv_perc < 0]   <- 0
sem_cf20_mar_nirv_perc[sem_cf20_mar_nirv_perc > 100] <- 100

# Covert to raster object for plotting
n_cs_mar               <- raster(n_cs_mar)
n_cf20_mar             <- raster(n_cf20_mar)
sem_cs_mar_nirv        <- raster(sem_cs_mar_nirv)
sem_cf20_mar_nirv      <- raster(sem_cf20_mar_nirv)
sem_cs_mar_nirv_perc   <- raster(sem_cs_mar_nirv_perc)
sem_cf20_mar_nirv_perc <- raster(sem_cf20_mar_nirv_perc)

# Diff plots
n_diff   <- n_cf20_mar - n_cs_mar
sem_diff <- sem_cf20_mar_nirv - sem_cs_mar_nirv

# # Median
n_cs_mar_median <- as.vector(n_cs_mar)
n_cs_mar_median <- n_cs_mar_median[!is.na(n_cs_mar_median)]
n_cs_mar_median <- sort(n_cs_mar_median)
n_cs_mar_median <- median(n_cs_mar_median)

sem_cs_mar_nirv_median <- as.vector(sem_cs_mar_nirv)
sem_cs_mar_nirv_median <- sem_cs_mar_nirv_median[!is.na(sem_cs_mar_nirv_median)]
sem_cs_mar_nirv_median <- sort(sem_cs_mar_nirv_median)
sem_cs_mar_nirv_median <- median(sem_cs_mar_nirv_median)

n_cf20_mar_median <- as.vector(n_cf20_mar)
n_cf20_mar_median <- n_cf20_mar_median[!is.na(n_cf20_mar_median)]
n_cf20_mar_median <- sort(n_cf20_mar_median)
n_cf20_mar_median <- median(n_cf20_mar_median)

sem_cf20_mar_nirv_median <- as.vector(sem_cf20_mar_nirv)
sem_cf20_mar_nirv_median <- sem_cf20_mar_nirv_median[!is.na(sem_cf20_mar_nirv_median)]
sem_cf20_mar_nirv_median <- sort(sem_cf20_mar_nirv_median)
sem_cf20_mar_nirv_median <- median(sem_cf20_mar_nirv_median)

n_diff_median <- as.vector(n_diff)
n_diff_median <- n_diff_median[!is.na(n_diff_median)]
n_diff_median <- sort(n_diff_median)
n_diff_median <- median(n_diff_median)

sem_diff_median <- as.vector(sem_diff)
sem_diff_median <- sem_diff_median[!is.na(sem_diff_median)]
sem_diff_median <- sort(sem_diff_median)
sem_diff_median <- median(sem_diff_median)

# Colors
n.col        <- plasma(10)
sem.col      <- viridis(10)
n.diff.col   <- coolwarm(20)[11:20]
sem.diff.col <- coolwarm(10)

# Labels
labs <- c(paste0("Clearsky Number of Soundings per Gridcell"),
          paste0("NIRv Clearsky Standard Error of Mean"),
          paste0("CF ", intToUtf8(8804), "0.20 Number of Soundings per Gridcell"),
          paste0("NIRv CF ", intToUtf8(8804), "0.20 Standard Error of Mean"),
          paste0("Difference in Number of Soundings (c - a)"),
          paste0("Difference in Standard Error of Mean (d - b)"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/SEM_NIRv_monthly_black.pdf", width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky N ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
n_cs_mar[n_cs_mar > 200] <- 200
plot(n_cs_mar, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(n_cs_mar, col=n.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

plot(n_cs_mar, legend.only=TRUE, col=n.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Number of soundings")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,100,200), labels=c("0","100",">200"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(n_cs_mar, col=n.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_cs_mar_median, col="red")
axis(3, tck=F, labels = round2(n_cs_mar_median, 0), at = n_cs_mar_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_cs_mar, col=n.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### SEM Clearsky
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_cs_mar_nirv[sem_cs_mar_nirv < 0]   <- 0
sem_cs_mar_nirv[sem_cs_mar_nirv > 0.02] <- 0.02
plot(sem_cs_mar_nirv, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_cs_mar_nirv, col=sem.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_cs_mar_nirv, legend.only=TRUE, col=sem.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.01,0.02), labels=c("0","0.01",">0.02"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_cs_mar_nirv, col=sem.col, breaks=10, ylim=c(0,150000), xlim=c(0,0.02), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_cs_mar_nirv_median, col="red")
axis(3, tck=F, labels = round2(sem_cs_mar_nirv_median, 3), at = sem_cs_mar_nirv_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_cs_mar_nirv, col=sem.col, breaks=10, ylim=c(0,150000), xlim=c(0,0.02), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.01","0.02"),
     mgp=c(3,0.3,0), at=c(0, 0.01, 0.02), col.axis = "white")
box(col = "white")



### N CF20 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
n_cf20_mar[n_cf20_mar > 200] <- 200
plot(n_cf20_mar, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(n_cf20_mar, col=n.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(n_cf20_mar, legend.only=TRUE, col=n.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Number of soundings")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,100,200), labels=c("0","100",">200"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(n_cf20_mar, col=n.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_cf20_mar_median, col="red")
axis(3, tck=F, labels= round2(n_cf20_mar_median, 0), at=n_cf20_mar_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_cf20_mar, col=n.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### SEM CF20
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_cf20_mar_nirv[sem_cf20_mar_nirv < 0]   <- 0
sem_cf20_mar_nirv[sem_cf20_mar_nirv > 0.02] <- 0.02
plot(sem_cf20_mar_nirv, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_cf20_mar_nirv, col=sem.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_cf20_mar_nirv, legend.only=TRUE, col=sem.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.01,0.02), labels=c("0","0.01",">0.02"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_cf20_mar_nirv, col=sem.col, breaks=10, ylim=c(0,150000), xlim=c(0,0.02), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_cf20_mar_nirv_median, col="red")
axis(3, tck=F, labels = round2(sem_cf20_mar_nirv_median, 3), at = sem_cf20_mar_nirv_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_cf20_mar_nirv, col=sem.col, breaks=10, ylim=c(0,150000), xlim=c(0,0.02), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.01","0.02"),
     mgp=c(3,0.3,0), at=c(0, 0.01, 0.02), col.axis = "white")
box(col = "white")


### Diff N ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
n_diff[n_diff > 200] <- 200
plot(n_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(n_diff, col=n.diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[5], cex=0.85, col = "white")
mtext(3, text="e", cex= 0.85, adj=0, font=2, col = "white")

plot(n_diff, legend.only=TRUE, col=n.diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Number of Soundings")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,100,200), labels=c("0","100",">200"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(n_diff, col=n.diff.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_diff_median, col="red")
axis(3, tck=F, labels= round2(n_diff_median, 0), at=n_diff_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_diff, col=n.diff.col, breaks=10, ylim=c(0,150000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### Diff SEM ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_diff[sem_diff < -0.01] <- -0.01
sem_diff[sem_diff > 0.01]  <- 0.01
plot(sem_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_diff, col=sem.diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[6], cex=0.85, col = "white")
mtext(3, text="f", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_diff, legend.only=TRUE, col=sem.diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-0.01,0,0.01), labels=c("<-0.01","0",">0.01"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_diff, col=sem.diff.col, breaks=10, ylim=c(0,150000), xlim=c(-0.01,0.01), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_diff_median, col="red")
axis(3, tck=F, labels= round2(sem_diff_median, 3), at=sem_diff_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_diff, col=sem.diff.col, breaks=10, ylim=c(0,150000), xlim=c(-0.01,0.01), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("-0.01","0","0.01"),
     mgp=c(3,0.3,0), at=c(-0.01, 0, 0.01), col.axis = "white")
box(col = "white")

dev.off()


