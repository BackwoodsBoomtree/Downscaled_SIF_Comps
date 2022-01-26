
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
cs_diff   <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.clearsky.monthly.20km.2020.tif")
cf20_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF20.monthly.20km.2020.tif")
cf40_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF40.monthly.20km.2020.tif")
cf60_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF60.monthly.20km.2020.tif")
cf80_diff <- raster("G:/SIF_comps/min_max_diff/20km/clearsky_vis/NIRv_Rad.clearsky-SIF.max.CF80.monthly.20km.2020.tif")

# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

cs_diff   <- mask(cs_diff, m)
cf20_diff <- mask(cf20_diff, m)
cf40_diff <- mask(cf40_diff, m)
cf60_diff <- mask(cf60_diff, m)
cf80_diff <- mask(cf80_diff, m)

# Median (seems to be 0 always for difference, so use mean)
# Get mean of rasters before plotting (where we set thresholds)
cs_diff_mean   <- cellStats(cs_diff, stat = 'mean', na.rm = TRUE)
cf20_diff_mean <- cellStats(cf20_diff, stat = 'mean', na.rm = TRUE)
cf40_diff_mean <- cellStats(cf40_diff, stat = 'mean', na.rm = TRUE)
cf60_diff_mean <- cellStats(cf60_diff, stat = 'mean', na.rm = TRUE)
cf80_diff_mean <- cellStats(cf80_diff, stat = 'mean', na.rm = TRUE)

# Colors
diff.col <- coolwarm(7)

# Labels
labs <- c(paste0("Peak Clear Sky NIRv - Peak Clear Sky SIF"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.20"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.40"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.60"),
          paste0("Peak Clear Sky NIRv - Peak SIF CF ", intToUtf8(8804), "0.80"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/SIF_vs_NIRv_clearsky_monthly_peak_black.pdf", width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cs_diff[cs_diff < -3] <- -3
cs_diff[cs_diff > 3]  <- 3
plot(cs_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

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


### CF20
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf20_diff[cf20_diff < -3] <- -3
cf20_diff[cf20_diff > 3]  <- 3
plot(cf20_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf20_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(cf20_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(cf20_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (cf20_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(cf20_diff_mean, 3), at = (cf20_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(cf20_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")



### CF40 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf40_diff[cf40_diff < -3] <- -3
cf40_diff[cf40_diff > 3]  <- 3
plot(cf40_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf40_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

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


### CF60
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf60_diff[cf60_diff < -3] <- -3
cf60_diff[cf60_diff > 3]  <- 3
plot(cf60_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf60_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[4], cex=0.85, col = "white")
mtext(3, text="d", cex= 0.85, adj=0, font=2, col = "white")

plot(cf60_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(cf60_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (cf60_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(cf60_diff_mean, 3), at = (cf60_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(cf60_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")


### CF80 Max Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
cf80_diff[cf80_diff < -3] <- -3
cf80_diff[cf80_diff > 3]  <- 3
plot(cf80_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cf80_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[5], cex=0.85, col = "white")
mtext(3, text="e", cex= 0.85, adj=0, font=2, col = "white")

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


