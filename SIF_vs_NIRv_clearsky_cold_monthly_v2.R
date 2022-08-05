
library(raster)
library(terra)
library(viridis)
library(rgdal)
library(pals)

options(scipen=999)

out_file <- "G:/SIF_comps/figs/v2/SIF_vs_NIRv_clearsky_cold_monthly_black_v2.pdf"

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
cs_nirv_r2  <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_cold_n30_2018-2021_Rsquare.tif")
cs_nirvr_r2 <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_cold_n30_2018-2021_Rsquare.tif")

cs_nirv_n   <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_cold_n30_2018-2021_Nobs.tif")
cs_nirvr_n  <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_cold_n30_2018-2021_Nobs.tif")

### Mask out by n
cs_nirv_n[cs_nirv_n < 40]   <- NA
cs_nirvr_n[cs_nirvr_n < 40] <- NA

cs_nirv_r2  <- mask(cs_nirv_r2, cs_nirv_n)
cs_nirvr_r2 <- mask(cs_nirvr_r2, cs_nirvr_n)

### Crop to tropics
t_ext      <- extent(c(-180, 180, -23, 23))
cs_nirv_r2 <- crop(cs_nirv_r2, t_ext)
cs_nirvr_r2 <- crop(cs_nirvr_r2, t_ext)

# Medians
cs_nirv_r2_median <- as.vector(cs_nirv_r2)
cs_nirv_r2_median <- cs_nirv_r2_median[!is.na(cs_nirv_r2_median)]
cs_nirv_r2_median <- sort(cs_nirv_r2_median)
cs_nirv_r2_median <- median(cs_nirv_r2_median)

cs_nirvr_r2_median <- as.vector(cs_nirvr_r2)
cs_nirvr_r2_median <- cs_nirvr_r2_median[!is.na(cs_nirvr_r2_median)]
cs_nirvr_r2_median <- sort(cs_nirvr_r2_median)
cs_nirvr_r2_median <- median(cs_nirvr_r2_median)

# Colors
r2.col   <- plasma(10)

# Labels
labs <- c(paste0("Monthly SIF vs NIRv Reflectance for Tropical Forest"),
          paste0("Monthly SIF vs NIRv Radiance for Tropical Forest"))

##### PLOTS ####

cairo_pdf(out_file, width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_nirv_r2, ext = c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_nirv_r2, col = r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[1], cex=0.85, col = "white")
mtext(3, text="a", cex= 0.85, adj=0, font=2, col = "white")

# Boxes
rect(-100, -23, -30, 23, col = NA, border = "white", lwd = 1)
rect(-18, -15, 45, 14, col = NA, border = "white", lwd = 1)
rect(72, -23, 175, 23, col = NA, border = "white", lwd = 1)
text(-35, 30, "c", col = "white")
text(-15, -22, "d", col = "white")
text(172, 30, "e", col = "white")

plot(cs_nirv_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0.01,0.5,0.97), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cs_nirv_r2, col = r2.col, breaks=10, ylim=c(0,250), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cs_nirv_r2_median, col="red")
axis(3, tck=F, labels= round2(cs_nirv_r2_median, 2), at=cs_nirv_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cs_nirv_r2, col = r2.col, breaks=10, ylim=c(0,250), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_nirvr_r2, ext = c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_nirvr_r2, col = r2.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = NA, border = "white")
mtext(3, text=labs[2], cex=0.85, col = "white")
mtext(3, text="b", cex= 0.85, adj=0, font=2, col = "white")

# Boxes
# Boxes
rect(-100, -23, -30, 23, col = NA, border = "white", lwd = 1)
rect(-18, -15, 45, 14, col = NA, border = "white", lwd = 1)
rect(72, -23, 175, 23, col = NA, border = "white", lwd = 1)
text(-35, 30, "f", col = "white")
text(-15, -22, "g", col = "white")
text(172, 30, "h", col = "white")

plot(cs_nirvr_r2, legend.only=TRUE, col=r2.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("R"^"2")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(0.01,0.5,0.97), labels=c("0","0.5","1.0"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

par(new=TRUE)
op <- par(mar = c(3.1,0.75,8,21)) # Set margins
hist(cs_nirvr_r2, col = r2.col, breaks=10, ylim=c(0,250), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = NA)
abline(v=cs_nirvr_r2_median, col="red")
axis(3, tck=F, labels= round2(cs_nirvr_r2_median, 2), at=cs_nirvr_r2_median, mgp=c(3, 0.1, 0), col.axis = "white")
hist(cs_nirvr_r2, col = r2.col, breaks=10, ylim=c(0,250), xlim=c(0,1), xaxs="i", yaxs="i", ann=FALSE, axes=FALSE, border = NA, add=T)

#axis
axis(side=1, tck=F, las=1, cex.axis=1, labels=c("0","0.5","1"),
     mgp=c(3,0.3,0), at=c(0, 0.5, 1), col.axis = "white")
box(col = "white")

dev.off()


