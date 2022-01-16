
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
        l <- c(l, r2.col[1])
      } else if (x[i] <= 0.2) {
        l <- c(l, r2.col[2])
      } else if (x[i] <= 0.3) {
        l <- c(l, r2.col[3])
      } else if (x[i] <= 0.4) {
        l <- c(l, r2.col[4])
      } else if (x[i] <= 0.5) {
        l <- c(l, r2.col[5])
      } else if (x[i] <= 0.6) {
        l <- c(l, r2.col[6])
      } else if (x[i] <= 0.7) {
        l <- c(l, r2.col[7])
      } else if (x[i] <= 0.8) {
        l <- c(l, r2.col[8])
      } else if (x[i] <= 0.9) {
        l <- c(l, r2.col[9])
      } else if (x[i] <= 1.0) {
        l <- c(l, r2.col[10])
      }
    } else {
      l <- c(l, NA)
    }
  }
  return(l)
}

### Data
cs_r2   <- raster("G:/SIF_comps/nirv_rad/2020/8day/20km/clearsky/raster_regressions/TROPOSIF_vs_TROPONIRv_RAD.8day.20km.clearsky.2020_Rsquare.tif")
cf20_r2 <- raster("G:/SIF_comps/nirv_rad/2020/8day/20km/cf20/raster_regressions/TROPOSIF_vs_TROPONIRv_RAD.8day.20km.CF20.2020_Rsquare.tif")
cf80_r2 <- raster("G:/SIF_comps/nirv_rad/2018/8day/20km/raster_regressions/SIF-LUE_vs_TROPO_SIF.1deg.clearsky.2018_Rsquare.tif")

cs_pval   <- raster("G:/SIF_comps/nirv_rad/2020/8day/20km/clearsky/raster_regressions/TROPOSIF_vs_TROPONIRv_RAD.8day.20km.clearsky.2020_Pval.tif")
cf20_pval <- raster("G:/SIF_comps/nirv_rad/2020/8day/20km/cf20/raster_regressions/TROPOSIF_vs_TROPONIRv_RAD.8day.20km.CF20.2020_Pval.tif")
cf80_pval <- raster("G:/SIF_comps/nirv_rad/2018/8day/20km/raster_regressions/SIF-LUE_vs_TROPO_SIF.1deg.clearsky.2018_Pval.tif")

cs_diff   <- raster("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad-SIF.max.8day.20km.clearsky.2020.tif")
cf20_diff <- raster("G:/SIF_comps/min_max/20km/cf20/NIRv_Rad-SIF.max.8day.20km.CF20.2020.tif")
cf80_diff <- raster("G:/SIF_comps/min_max/20km/cf20/NIRv_Rad-SIF.max.8day.20km.CF80.2020.tif")

sif_mask  <- rast("G:/TROPOMI/esa/gridded/20km/monthly/TROPOMI.ESA.SIF.201805-202111.global.monthly.20km.CF20.VIs.nc", subds = "SIF_743")[[21:32]]
ndvi_mask <- rast("G:/TROPOMI/esa/gridded/20km/monthly/TROPOMI.ESA.SIF.201805-202111.global.monthly.20km.CF20.VIs.nc", subds = "NDVI")[[21:32]]

# Get max annual values for masking
sif_mask  <- max(sif_mask, na.rm = TRUE)
ndvi_mask <- max(ndvi_mask, na.rm = TRUE)
sif_mask[sif_mask < 0.2] <- NA
ndvi_mask[ndvi_mask < 0.1] <- NA

plot(sif_mask, col = viridis(100))
plot(ndvi_mask, col = viridis(100))


# Get median of difference rasters before plotting (where we set thresholds)
cs_mean   <- cellStats(cs_diff, stat = 'mean', na.rm = TRUE)
cf20_mean <- cellStats(cf20_diff, stat = 'mean', na.rm = TRUE)
cf80_mean <- cellStats(cf80_diff, stat = 'mean', na.rm = TRUE)


# Row means for latitude mean difference
cs_r2_lat   <- rev(rowMeans(as.matrix(cs_r2), na.rm = TRUE))
cf20_r2_lat <- rev(rowMeans(as.matrix(cf20_r2), na.rm = TRUE))
cf80_r2_lat <- rev(rowMeans(as.matrix(cf80_r2), na.rm = TRUE))

# Colors
r2.col   <- rev(plasma(10))
diff.col <- coolwarm(11)

# Color lists for barplots
cs_col   <- col_list(cs_r2_lat)
cf20_col <- col_list(cf20_r2_lat)
cf80_col <- col_list(cf80_r2_lat)

# Labels
labs <- c(expression(paste("8-day SIF vs NIRv: Clear Sky")),
          expression(paste("Difference in 8-day Peak in (a)")),
          expression(paste("8-day SIF vs NIRv: Cloud Fraction ≤ 0.20")),
          expression(paste("Difference in 8-day Peak in (c)")),
          expression(paste("8-day SIF vs NIRv: Cloud Fraction ≤ 0.80 & ≤ 0.20")),
          expression(paste("Difference in 8-day Peak in (e)")))

##### PLOTS ####

pdf("G:/SIF_comps/figs/SIF_vs_NIRv_8day_black.pdf", width=7.5, height=6, compress=FALSE)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cs_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

plot(cs_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- op <- par(mar = c(3,2,8,21)) # Set margins
barplot(cs_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(cs_r2_lat, col = cs_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
title <- as.list(expression(paste("Mean R"^"2"), "by Latitude"))
mtext(do.call(expression, title), side = 3, line = c(1,-0.25), cex = 0.85, col = "white")
box(col = "white")


### Clear Sky Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cs_diff[cs_diff < -5] <- -5
cs_diff[cs_diff > 5]  <- 5
plot(cs_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cs_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(cs_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("8-day Difference")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-5, 0, 5), labels=c("≤-5","0","≥5"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(2.1,0.25,8,21)) # Set margins
hist(cs_diff, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(-5,5), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=cs_mean, col="red")
axis(3, tck=F, labels= round2(cs_mean, 2), at=cs_mean, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cs_diff, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(-5,5), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)
box(col = "white")


### CF20 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf20_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf20_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(cf20_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,0.99), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- op <- par(mar = c(3,2,8,21)) # Set margins
barplot(cf20_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(cf20_r2_lat, col = cf20_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
title <- as.list(expression(paste("Mean R"^"2"), "by Latitude"))
mtext(do.call(expression, title), side = 3, line = c(1,-0.25), cex = 0.85, col = "white")
box(col = "white")


### CF20 Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf20_diff[cf20_diff < -5] <- -5
cf20_diff[cf20_diff > 5]  <- 5
plot(cf20_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf20_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(cf20_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("8-day Difference")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-5, 0, 5), labels=c("≤-5","0","≥5"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(2.1,0.25,8,21)) # Set margins
hist(cf20_diff, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(-5,5), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=cf20_mean, col="red")
axis(3, tck=F, labels= round2(cf20_mean, 2), at=cf20_mean, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf20_diff, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(-5,5), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)
box(col = "white")


### CF80 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf80_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf80_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(cf80_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3,2,8,21)) # Set margins
barplot(cf80_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(cf80_r2_lat, col = cf80_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
title <- as.list(expression(paste("Mean R"^"2"), "by Latitude"))
mtext(do.call(expression, title), side = 3, line = c(1,-0.25), cex = 0.85, col = "white")
box(col = "white")

dev.off()


