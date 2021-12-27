library(raster)
library(viridis)
library(rgdal)
library(RColorBrewer)

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
gosif_r2    <- raster("G:/SIF_comps/gosif/2020/1deg/raster_regressions/GOSIF_vs_TROPO_SIF.1deg.clearsky.2020_Rsquare.tif")
csif_r2     <- raster("G:/SIF_comps/csif/daily/2020/1deg/raster_regressions/CSIF_vs_TROPO_SIF.1deg.clearsky.2020_Rsquare.tif")
siflue_r2   <- raster("G:/SIF_comps/sif-lue/2018/1deg/raster_regressions/SIF-LUE_vs_TROPO_SIF.1deg.clearsky.2018_Rsquare.tif")

gosif_pval  <- raster("G:/SIF_comps/gosif/2020/1deg/raster_regressions/GOSIF_vs_TROPO_SIF.1deg.clearsky.2020_Pval.tif")
csif_pval   <- raster("G:/SIF_comps/csif/daily/2020/1deg/raster_regressions/CSIF_vs_TROPO_SIF.1deg.clearsky.2020_Pval.tif")
siflue_pval <- raster("G:/SIF_comps/sif-lue/2018/1deg/raster_regressions/SIF-LUE_vs_TROPO_SIF.1deg.clearsky.2018_Pval.tif")


# Row means for latitude mean difference
gosif_r2_lat  <- rev(rowMeans(as.matrix(gosif_r2), na.rm = TRUE))
csif_r2_lat   <- rev(rowMeans(as.matrix(csif_r2), na.rm = TRUE))
siflue_r2_lat <- rev(rowMeans(as.matrix(siflue_r2), na.rm = TRUE))

# Colors
r2.col <- rev(plasma(10))

# Color lists for barplots
gosif_col  <- col_list(gosif_r2_lat)
csif_col   <- col_list(csif_r2_lat)
siflue_col <- col_list(siflue_r2_lat)

# Labels
labs <- c(expression(paste("GOSIF vs TROPOMI SIF 2020")),
          expression(paste("CSIF vs TROPOMI SIF 2020")),
          expression(paste("SIF-LUE vs TROPOMI SIF 2018")))

##### R2 PLOTS ####

pdf("G:/SIF_comps/figs/Downscale_SIF_vs_TROPOSIF_black.pdf", width=6, height=8, compress=FALSE)

par(mfrow=c(3,1),oma=c(0,0.25,1.25,0), bg = "black")

### GOSIF ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(gosif_r2, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(gosif_r2, col=r2.col, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

plot(gosif_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.45,0.80,0.15,0.20)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,2.25,10.25,34)) # Set margins
barplot(gosif_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(gosif_r2_lat, col = gosif_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
mtext(3, text = expression(paste("Mean R"^"2", " by Latitude")), cex = 0.85, col = "white")
box(col = "white")

### CSIF ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(csif_r2, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(csif_r2, col=r2.col, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

plot(csif_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,0.99), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.45,0.80,0.15,0.20)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,2.25,10.25,34)) # Set margins
barplot(csif_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(csif_r2_lat, col = csif_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
mtext(3, text = expression(paste("Mean R"^"2", " by Latitude")), cex = 0.85, col = "white")
box(col = "white")


### SIF-LUE ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(siflue_r2, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.20,0.20,0.20))
plot(siflue_r2, col=r2.col, ext=c(-180,180,-60,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add=T)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[3], cex=0.85, col = "white")
mtext(3, text="c", cex= 0.85, adj=0, font=2, col = "white")

plot(siflue_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -2.0, cex=0.85, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0,0.5,1.0), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.45,0.80,0.15,0.20)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,2.25,10.25,34)) # Set margins
barplot(siflue_r2_lat, col = NA, axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
rect(-100,55,100,125, col = rgb(0.20,0.20,0.20), border = NA)
rect(-100,67,100,113, col = rgb(0.30,0.30,0.30), border = NA)
par(new=T)
barplot(siflue_r2_lat, col = siflue_col,
        axes=F, tck=F, xpd=F, mgp=c(3,0.3,0), ann=FALSE, xaxs = "i", yaxs = "i", horiz = TRUE, border = NA, space = 0, xlim = c(0, 1.0), ylim = c(31,170))
abline(v = 0.5, col = "white")

# axis
axis(side=2, tck=F, las=1, cex.axis=1, labels = c("-60°", "0°", "80°"), mgp=c(3,0.3,0), at=c(31,90,170), col.axis = "white")
axis(side=1, tck=F, las=1, cex.axis=1, labels = c("0", "0.5", "1.0"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1.0), col.axis = "white")
mtext(3, text = expression(paste("Mean R"^"2", " by Latitude")), cex = 0.85, col = "white")
box(col = "white")

dev.off()
