
library(raster)
library(terra)
library(viridis)
library(rgdal)
library(pals)

options(scipen=999)

out_file <- "G:/SIF_comps/figs/v2/SIF_vs_NIRv_clearsky_monthly_pvalue_black_v2.pdf"

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
      if (x[i] < 0.01) {
        l <- c(l, pval.col[1])
      } else if (x[i] < 0.05 && x[i] > 0.01) {
        l <- c(l, pval.col[2])
      } else if (x[i] >= 0.05) {
        l <- c(l, pval.col[3])
      } 
    } else {
      l <- c(l, NA)
    }
  }
  return(l)
}

### Data
cs_nirv_pval  <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_cold_n30_2018-2021_Pval.tif")
cs_nirvr_pval <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_cold_n30_2018-2021_Pval.tif")

cs_nirv_n   <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_cold_n30_2018-2021_Nobs.tif")
cs_nirvr_n  <- raster("G:/SIF_comps/figs/raster_regressions/clearsky_cold/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_cold_n30_2018-2021_Nobs.tif")

### Mask out by n
cs_nirv_n[cs_nirv_n < 40]   <- NA
cs_nirvr_n[cs_nirvr_n < 40] <- NA

cs_nirv_pval  <- mask(cs_nirv_pval, cs_nirv_n)
cs_nirvr_pval <- mask(cs_nirvr_pval, cs_nirvr_n)

### Crop to tropics
t_ext      <- extent(c(-180, 180, -23, 23))
cs_nirv_pval <- crop(cs_nirv_pval, t_ext)
cs_nirvr_pval <- crop(cs_nirvr_pval, t_ext)

# Medians
cs_nirv_pval_median <- as.vector(cs_nirv_pval)
cs_nirv_pval_median <- cs_nirv_pval_median[!is.na(cs_nirv_pval_median)]
cs_nirv_pval_median <- sort(cs_nirv_pval_median)
cs_nirv_pval_median <- median(cs_nirv_pval_median)

cs_nirvr_pval_median <- as.vector(cs_nirvr_pval)
cs_nirvr_pval_median <- cs_nirvr_pval_median[!is.na(cs_nirvr_pval_median)]
cs_nirvr_pval_median <- sort(cs_nirvr_pval_median)
cs_nirvr_pval_median <- median(cs_nirvr_pval_median)

# Classify for visual ploting
m      <- c(0, 0.01, 1,  0.01, 0.05, 2,  0.05, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
cs_nirv_pval <- reclassify(cs_nirv_pval, rclmat)
cs_nirvr_pval <- reclassify(cs_nirvr_pval, rclmat)

# Colors
pval.col   <- plasma(3)

# Labels
labs <- c(paste0("Monthly SIF vs NIRv Reflectance for Tropical Forest"),
          paste0("Monthly SIF vs NIRv Radiance for Tropical Forest"))

##### PLOTS ####

cairo_pdf(out_file, width=7.5, height=6)

par(mfrow=c(3,2),oma=c(0,0.25,1.25,0), bg = "black")

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_nirv_pval, ext = c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_nirv_pval, col = pval.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
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

plot(cs_nirv_pval, legend.only=TRUE, col=pval.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("p-value")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(1,2,3), labels=c("<0.01","<0.05",">0.05"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

### Clearsky R2 ###
op <- par(mar = c(0,0,0.25,0.25), bg = "black")
plot(cs_nirvr_pval, ext = c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA)
plot(coastlines, add = TRUE, border = NA, col = rgb(0.30,0.30,0.30))
plot(cs_nirvr_pval, col = pval.col, ext=c(-180,180,-80,80), axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
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

plot(cs_nirv_pval, legend.only=TRUE, col=pval.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("p-value")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(1,2,3), labels=c("<0.01","<0.05",">0.05"), col.axis = "white"),
     smallplot=c(0.40,0.90,0.2,0.25)); par(mar = par("mar"))

dev.off()


