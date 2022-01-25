
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

# Mask Raster
m  <- rast("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

### Data ####

# Clear Sky SIF
n_cs_mar       <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "n")[[3]], m)
std_cs_mar_sif <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "SIF_743_std")[[3]], m)
cs_mar_sif     <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.clearsky.nc", subds = "SIF_743")[[3]], m)
sem_cs_mar_sif <- std_cs_mar_sif / sqrt(n_cs_mar)

# CF20 NIRv
n_cf80_mar       <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF80.nc", subds = "n")[[3]], m)
std_cf80_mar_sif <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF80.nc", subds = "SIF_743_std")[[3]], m)
cf80_mar_sif <- mask(rast("G:/TROPOMI/esa/gridded/20km/monthly/2020/TROPOMI.ESA.SIF.2020.global.monthly.20km.CF80.nc", subds = "SIF_743")[[3]], m)
sem_cf80_mar_sif <- std_cf80_mar_sif / sqrt(n_cf80_mar)

# # Percent
# sem_cs_mar_nirv_perc   <- sem_cs_mar_nirv / cs_mar_nirv * 100
# sem_cf80_mar_nirv_perc <- sem_cf80_mar_nirv / cf80_mar_nirv * 100
# 
# sem_cs_mar_nirv_perc[sem_cs_mar_nirv_perc < 0]       <- 0
# sem_cs_mar_nirv_perc[sem_cs_mar_nirv_perc > 100]     <- 100
# sem_cf80_mar_nirv_perc[sem_cf80_mar_nirv_perc < 0]   <- 0
# sem_cf80_mar_nirv_perc[sem_cf80_mar_nirv_perc > 100] <- 100

# Covert to raster object for plotting
n_cs_mar               <- raster(n_cs_mar)
n_cf80_mar             <- raster(n_cf80_mar)
sem_cs_mar_sif         <- raster(sem_cs_mar_sif)
sem_cf80_mar_sif       <- raster(sem_cf80_mar_sif)
# sem_cs_mar_nirv_perc   <- raster(sem_cs_mar_nirv_perc)
# sem_cf80_mar_nirv_perc <- raster(sem_cf80_mar_nirv_perc)

# Diff plots
n_diff   <- n_cf80_mar - n_cs_mar
sem_diff <- sem_cf80_mar_sif - sem_cs_mar_sif

# # Median
n_cs_mar_median <- as.vector(n_cs_mar)
n_cs_mar_median <- n_cs_mar_median[!is.na(n_cs_mar_median)]
n_cs_mar_median <- sort(n_cs_mar_median)
n_cs_mar_median <- median(n_cs_mar_median)

sem_cs_mar_sif_median <- as.vector(sem_cs_mar_sif)
sem_cs_mar_sif_median <- sem_cs_mar_sif_median[!is.na(sem_cs_mar_sif_median)]
sem_cs_mar_sif_median <- sort(sem_cs_mar_sif_median)
sem_cs_mar_sif_median <- median(sem_cs_mar_sif_median)

n_cf80_mar_median <- as.vector(n_cf80_mar)
n_cf80_mar_median <- n_cf80_mar_median[!is.na(n_cf80_mar_median)]
n_cf80_mar_median <- sort(n_cf80_mar_median)
n_cf80_mar_median <- median(n_cf80_mar_median)

sem_cf80_mar_sif_median <- as.vector(sem_cf80_mar_sif)
sem_cf80_mar_sif_median <- sem_cf80_mar_sif_median[!is.na(sem_cf80_mar_sif_median)]
sem_cf80_mar_sif_median <- sort(sem_cf80_mar_sif_median)
sem_cf80_mar_sif_median <- median(sem_cf80_mar_sif_median)

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
          paste0("SIF Clearsky Standard Error of Mean"),
          paste0("CF ", intToUtf8(8804), "0.80 Number of Soundings per Gridcell"),
          paste0("SIF CF ", intToUtf8(8804), "0.80 Standard Error of Mean"),
          paste0("Difference in Number of Soundings (c - a)"),
          paste0("Difference in Standard Error of Mean (d - b)"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/SEM_SIF_monthly_CF80_black.pdf", width=7.5, height=6)

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
hist(n_cs_mar, col=n.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_cs_mar_median, col="red")
axis(3, tck=F, labels = round2(n_cs_mar_median, 0), at = n_cs_mar_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_cs_mar, col=n.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### SEM Clearsky
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_cs_mar_sif[sem_cs_mar_sif < 0]   <- 0
sem_cs_mar_sif[sem_cs_mar_sif > 0.2] <- 0.2
plot(sem_cs_mar_sif, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_cs_mar_sif, col=sem.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_cs_mar_sif, legend.only=TRUE, col=sem.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.1,0.2), labels=c("0","0.1",">0.2"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_cs_mar_sif, col=sem.col, breaks=10, ylim=c(0,175000), xlim=c(0,0.2), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_cs_mar_sif_median, col="red")
axis(3, tck=F, labels = round2(sem_cs_mar_sif_median, 3), at = sem_cs_mar_sif_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_cs_mar_sif, col=sem.col, breaks=10, ylim=c(0,175000), xlim=c(0,0.2), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.1","0.2"),
     mgp=c(3,0.3,0), at=c(0, 0.1, 0.2), col.axis = "white")
box(col = "white")



### N CF80 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
n_cf80_mar[n_cf80_mar > 200] <- 200
plot(n_cf80_mar, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(n_cf80_mar, col=n.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(n_cf80_mar, legend.only=TRUE, col=n.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Number of soundings")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,100,200), labels=c("0","100",">200"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(n_cf80_mar, col=n.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_cf80_mar_median, col="red")
axis(3, tck=F, labels= round2(n_cf80_mar_median, 0), at=n_cf80_mar_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_cf80_mar, col=n.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### SEM CF20
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_cf80_mar_sif[sem_cf80_mar_sif < 0]   <- 0
sem_cf80_mar_sif[sem_cf80_mar_sif > 0.2] <- 0.2
plot(sem_cf80_mar_sif, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_cf80_mar_sif, col=sem.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_cf80_mar_sif, legend.only=TRUE, col=sem.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.1,0.2), labels=c("0","0.1",">0.2"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_cf80_mar_sif, col=sem.col, breaks=10, ylim=c(0,175000), xlim=c(0,0.2), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_cf80_mar_sif_median, col="red")
axis(3, tck=F, labels = round2(sem_cf80_mar_sif_median, 3), at = sem_cf80_mar_sif_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_cf80_mar_sif, col=sem.col, breaks=10, ylim=c(0,175000), xlim=c(0,0.2), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.1","0.2"),
     mgp=c(3,0.3,0), at=c(0, 0.1, 0.2), col.axis = "white")
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
hist(n_diff, col=n.diff.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=n_diff_median, col="red")
axis(3, tck=F, labels= round2(n_diff_median, 0), at=n_diff_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(n_diff, col=n.diff.col, breaks=10, ylim=c(0,175000), xlim=c(0,200), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","100","200"),
     mgp=c(3,0.3,0), at=c(0, 100, 200), col.axis = "white")
box(col = "white")


### Diff SEM ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
sem_diff[sem_diff < -0.1] <- -0.1
sem_diff[sem_diff > 0.1]  <- 0.1
plot(sem_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(sem_diff, col=sem.diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[6], cex=0.85, col = "white")
mtext(3, text="f", cex= 0.85, adj=0, font=2, col = "white")

plot(sem_diff, legend.only=TRUE, col=sem.diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Standard Error of Mean")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-0.1,0,0.1), labels=c("<-0.1","0",">0.1"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(sem_diff, col=sem.diff.col, breaks=10, ylim=c(0,175000), xlim=c(-0.1,0.1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=sem_diff_median, col="red")
axis(3, tck=F, labels= round2(sem_diff_median, 3), at=sem_diff_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(sem_diff, col=sem.diff.col, breaks=10, ylim=c(0,175000), xlim=c(-0.1,0.1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("-0.1","0","0.1"),
     mgp=c(3,0.3,0), at=c(-0.1, 0, 0.1), col.axis = "white")
box(col = "white")

dev.off()


