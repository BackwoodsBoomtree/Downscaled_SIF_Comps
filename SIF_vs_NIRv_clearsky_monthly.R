
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
cs_r2   <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.clearsky_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")
cf20_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF20_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")
cf40_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF40_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")
cf60_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF60_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")
cf80_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF80_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")


# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

cs_r2   <- mask(cs_r2, m)
cf20_r2 <- mask(cf20_r2, m)
cf40_r2 <- mask(cf40_r2, m)
cf60_r2 <- mask(cf60_r2, m)
cf80_r2 <- mask(cf80_r2, m)

# Get difference map and its mean
diff_map <- cs_r2 - cf80_r2
diff_map_mean   <- cellStats(diff_map, stat = 'mean', na.rm = TRUE)

# Median (seems to be 0 always for difference, so use mean)
cs_r2_median <- as.vector(cs_r2)
cs_r2_median <- cs_r2_median[!is.na(cs_r2_median)]
cs_r2_median <- sort(cs_r2_median)
cs_r2_median <- median(cs_r2_median)

cf20_r2_median <- as.vector(cf20_r2)
cf20_r2_median <- cf20_r2_median[!is.na(cf20_r2_median)]
cf20_r2_median <- sort(cf20_r2_median)
cf20_r2_median <- median(cf20_r2_median)

cf40_r2_median <- as.vector(cf40_r2)
cf40_r2_median <- cf40_r2_median[!is.na(cf40_r2_median)]
cf40_r2_median <- sort(cf40_r2_median)
cf40_r2_median <- median(cf40_r2_median)

cf60_r2_median <- as.vector(cf60_r2)
cf60_r2_median <- cf60_r2_median[!is.na(cf60_r2_median)]
cf60_r2_median <- sort(cf60_r2_median)
cf60_r2_median <- median(cf60_r2_median)

cf80_r2_median <- as.vector(cf80_r2)
cf80_r2_median <- cf80_r2_median[!is.na(cf80_r2_median)]
cf80_r2_median <- sort(cf80_r2_median)
cf80_r2_median <- median(cf80_r2_median)

# Colors
r2.col   <- rev(plasma(10))
diff.col <- coolwarm(6)

# Labels
labs <- c(paste0("Monthly Clear Sky SIF vs Clear Sky NIRv"),
          paste0("Monthly SIF CF ", intToUtf8(8804),"0.20 vs Clear Sky NIRv"),
          paste0("Monthly SIF CF ", intToUtf8(8804),"0.40 vs Clear Sky NIRv"),
          paste0("Monthly SIF CF ", intToUtf8(8804),"0.60 vs Clear Sky NIRv"),
          paste0("Monthly SIF CF ", intToUtf8(8804),"0.80 vs Clear Sky NIRv"))

diff_lab <- expression(paste("Difference in R"^"2", " between Clear Sky and CF 0.8 (a - e)"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/SIF_vs_NIRv_clearsky_monthly_black.pdf", width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

plot(cs_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cs_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cs_r2_median, col="red")
axis(3, tck=F, labels= round2(cs_r2_median, 2), at=cs_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cs_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF20 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf20_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf20_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(cf20_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cf20_r2, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cf20_r2_median, col="red")
axis(3, tck=F, labels= round2(cf20_r2_median, 2), at=cf20_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf20_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF40 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf40_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf40_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(cf40_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cf40_r2, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cf40_r2_median, col="red")
axis(3, tck=F, labels= round2(cf40_r2_median, 2), at=cf40_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf40_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")

## CF60 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf60_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf60_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(cf60_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cf60_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cf60_r2_median, col="red")
axis(3, tck=F, labels= round2(cf60_r2_median, 2), at=cf60_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf60_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF80 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf80_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf80_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[5], cex=0.85, col = "white")
mtext(3, text="e", cex= 0.85, adj=0, font=2, col = "white")

plot(cf80_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cf80_r2, col=rgb(0.30,0.30,0.30), breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cf80_r2_median, col="red")
axis(3, tck=F, labels= round2(cf80_r2_median, 2), at=cf80_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf80_r2, col = r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF80 Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
diff_map[diff_map < -0.3] <- -0.3
diff_map[diff_map > 0.3]  <- 0.3
plot(diff_map, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(diff_map, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=diff_lab, cex=0.85, col = "white")
mtext(3, text="f", cex= 0.85, adj=0, font=2, col = "white")

plot(diff_map, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-0.3, 0, 0.3), labels=c(paste0(intToUtf8(8804), "-0.3"),"0",paste0(intToUtf8(8805), "0.3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,1.0,8,21)) # Set margins
hist(diff_map, breaks=6, ylim=c(0,250000), xlim=c(-0.3,0.3), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=diff_map_mean, col="red")
axis(3, tck=F, labels= round2(diff_map_mean, 2), at=diff_map_mean, mgp=c(3, 0.1, 0), col.axis = "white")
hist(diff_map, col = diff.col, breaks=6, ylim=c(0,250000), xlim=c(-0.3,0.3), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-0.3"),"0",paste0(intToUtf8(8805), "0.3")),
     mgp=c(3,0.3,0), at=c(-0.3, 0, 0.3), col.axis = "white")
box(col = "white")

dev.off()


