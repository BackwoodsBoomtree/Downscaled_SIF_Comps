
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
nirvr_cs   <- raster("G:/SIF_comps/min_max/20km/clearsky/NIRv_Rad.max.monthly.20km.clearsky.2020.tif")
nirvr_cf20 <- raster("G:/SIF_comps/min_max/20km/cf20/NIRv_Rad.max.monthly.20km.CF20.2020.tif")
nirvr_diff <- raster("G:/SIF_comps/min_max_diff/20km/nirvr/NIRv_Rad.clearsky-NIRv_Rad.CF20.max.monthly.20km.2020.tif")

# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask

nirvr_cs   <- mask(nirvr_cs, m)
nirvr_cf20 <- mask(nirvr_cf20, m)
nirvr_diff <- mask(nirvr_diff, m)

# Median (seems to be 0 always for difference, so use mean)
nirvr_diff_median <- as.vector(nirvr_diff)
nirvr_diff_median <- nirvr_diff_median[!is.na(nirvr_diff_median)]
nirvr_diff_median <- sort(nirvr_diff_median)
nirvr_diff_median <- median(nirvr_diff_median)

# Get mean of rasters before plotting (where we set thresholds)
nirvr_diff_mean   <- cellStats(nirvr_diff, stat = 'mean', na.rm = TRUE)

# Colors
r2.col   <- rev(plasma(12))
diff.col <- coolwarm(7)

# Labels
labs <- c(paste0("Peak Monthly NIRv Clear Sky"),
          paste0("Peak Monthly NIRv CF ", intToUtf8(8804), "0.20"),
          paste0("Difference in Peak Monthly NIRv (a - b)"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/NIRv_Peak_Difference_black.pdf", width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Peak NIRv Clearsky ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(nirvr_cs, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(nirvr_cs, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

plot(nirvr_cs, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Month of NIRv Clear Sky Peak")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(1,12), labels=c("Jan","Dec"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))


### Peak NIRv CF20 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(nirvr_cf20, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(nirvr_cf20, col=r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(nirvr_cf20, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Month of NIRv CF 0.20 Peak")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(1,12), labels=c("Jan","Dec"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))


### Peak Difference
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
nirvr_diff[nirvr_diff < -3] <- -3
nirvr_diff[nirvr_diff > 3]  <- 3
plot(nirvr_diff, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(nirvr_diff, col=diff.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(nirvr_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-3, 0, 3), labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
barplot(nirvr_diff, col=diff.col, space = 0, border = NA, ylim = c(0,275000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
abline(v = (nirvr_diff_mean + 3.5), col = "red")
axis(3, tck = F, labels = round2(nirvr_diff_mean, 3), at = (nirvr_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
barplot(nirvr_diff, col=diff.col, space = 0, border = NA, ylim = c(0,275000),
        xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
     mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
box(col = "white")


dev.off()


