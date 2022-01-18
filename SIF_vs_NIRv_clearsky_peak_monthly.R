
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
cs_r2   <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky/raster_regressions/TROPOSIF_vs_TROPONIRv_RAD.monthly.20km.clearsky.2020_Rsquare.tif")
cf40_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF40_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")
cf80_r2 <- raster("G:/SIF_comps/nirv_rad/2020/monthly/20km/clearsky_nirvr/raster_regressions/TROPOSIF.CF80_vs_TROPONIRv_RAD.clearsky.monthly.20km.2020_Rsquare.tif")

cs_diff   <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.clearsky.monthly.20km.2020.tif")
cf40_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF40.monthly.20km.2020.tif")
cf80_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF80.monthly.20km.2020.tif")

# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

cs_r2   <- mask(cs_r2, m)
cf40_r2 <- mask(cf40_r2, m)
cf80_r2 <- mask(cf80_r2, m)

cs_diff   <- mask(cs_diff, m)
cf40_diff <- mask(cf40_diff, m)
cf80_diff <- mask(cf80_diff, m)

# Median (seems to be 0 always for difference, so use mean)
cs_r2_median <- as.vector(cs_r2)
cs_r2_median <- cs_r2_median[!is.na(cs_r2_median)]
cs_r2_median <- sort(cs_r2_median)
cs_r2_median <- median(cs_r2_median)

cf40_r2_median <- as.vector(cf40_r2)
cf40_r2_median <- cf40_r2_median[!is.na(cf40_r2_median)]
cf40_r2_median <- sort(cf40_r2_median)
cf40_r2_median <- median(cf40_r2_median)

cf80_r2_median <- as.vector(cf80_r2)
cf80_r2_median <- cf80_r2_median[!is.na(cf80_r2_median)]
cf80_r2_median <- sort(cf80_r2_median)
cf80_r2_median <- median(cf80_r2_median)

# Get mean of rasters before plotting (where we set thresholds)
cs_diff_mean   <- cellStats(cs_diff, stat = 'mean', na.rm = TRUE)
cf40_diff_mean <- cellStats(cf40_diff, stat = 'mean', na.rm = TRUE)
cf80_diff_mean <- cellStats(cf80_diff, stat = 'mean', na.rm = TRUE)

# Colors
r2.col   <- rev(plasma(10))
diff.col <- coolwarm(7)

# Labels
labs <- c(paste0("Monthly SIF vs NIRv: Clear Sky"),
          paste0("Peak NIRv - Peak SIF: Clear Sky"),
          paste0("Monthly SIF CF ", intToUtf8(8804), "0.40 vs Clear Sky NIRv"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.40"),
          paste0("Monthly SIF CF ", intToUtf8(8804), "0.80 vs Clear Sky NIRv"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.80"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/SIF_vs_NIRv_monthly_clearsky_peak_black.pdf", width=7.5, height=6)

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
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cs_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=cs_r2_median, col="red")
abline(v = 0.5, col = "white")
axis(3, tck=F, labels= round2(cs_r2_median, 2), at=cs_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cs_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### Clear Sky Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cs_diff[cs_diff < -3] <- -3
cs_diff[cs_diff > 3]  <- 3
plot(cs_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cs_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(cs_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(cs_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (cs_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(cs_diff_mean, 3), at = (cs_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(cs_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")



### CF40 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf40_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf40_r2, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(cf40_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,0.99), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cf40_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=cf40_r2_median, col="red")
abline(v = 0.5, col = "white")
axis(3, tck=F, labels= round2(cf40_r2_median, 2), at=cf40_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf40_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF40 Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf40_diff[cf40_diff < -3] <- -3
cf40_diff[cf40_diff > 3]  <- 3
plot(cf40_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf40_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(cf40_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(cf40_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (cf40_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(cf40_diff_mean, 3), at = (cf40_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(cf40_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")


### CF80 R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cf80_r2, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
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
hist(cf80_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v=cf80_r2_median, col="red")
abline(v = 0.5, col = "white")
axis(3, tck=F, labels= round2(cf80_r2_median, 2), at=cf80_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cf80_r2, col=r2.col, breaks=10, ylim=c(0,250000), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")


### CF80 Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf80_diff[cf80_diff < -3] <- -3
cf80_diff[cf80_diff > 3]  <- 3
plot(cf80_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(cf80_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[6], cex=0.85, col = "white")
mtext(3, text="f", cex= 0.85, adj=0, font=2, col = "white")

plot(cf80_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(cf80_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (cf80_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(cf80_diff_mean, 3), at = (cf80_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(cf80_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")

dev.off()


