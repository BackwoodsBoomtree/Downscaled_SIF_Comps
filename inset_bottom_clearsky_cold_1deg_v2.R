library(raster)
library(rgdal)
library(rgeos)
library(viridis)
library(pals)

options(scipen=999)

out_file <- "G:/SIF_comps/figs/v2/inset_bottom_clearsky_cold_1deg_v2.pdf"

#### Load Map ####

coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
# Change to full global coverage
coastlines@bbox <- as.matrix(extent(-180, 180, -90, 90))

# Round up
round2 <- function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}


### Data
cs_nirv_r2  <- raster("G:/SIF_comps/figs/raster_regressions/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_n30_2018-2021_Rsquare.tif")
cs_nirvr_r2 <- raster("G:/SIF_comps/figs/raster_regressions/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_n30_2018-2021_Rsquare.tif")

cs_nirv_n   <- raster("G:/SIF_comps/figs/raster_regressions/TROPOMI_SIF743_vs_NIRv_monthly_1deg_clearsky_n30_2018-2021_Nobs.tif")
cs_nirvr_n  <- raster("G:/SIF_comps/figs/raster_regressions/TROPOMI_SIF743_vs_NIRv_RAD_monthly_1deg_clearsky_n30_2018-2021_Nobs.tif")

### Mask out by n
cs_nirv_n[cs_nirv_n < 40]   <- NA
cs_nirvr_n[cs_nirvr_n < 40] <- NA

cs_nirv_r2  <- mask(cs_nirv_r2, cs_nirv_n)
cs_nirvr_r2 <- mask(cs_nirvr_r2, cs_nirvr_n)

### Crop to tropics
t_ext       <- extent(c(-180, 180, -23, 23))
cs_nirv_r2  <- crop(cs_nirv_r2, t_ext)
cs_nirvr_r2 <- crop(cs_nirvr_r2, t_ext)

# Crop to extent
samerica_ext <- extent(c(-110,-34,-90,90))
africa_ext   <- extent(c(-18,45,-23,14))
seasia_ext   <- extent(c(72,180,-23,23))

# Colors
r2.col   <- plasma(10)


##### PLOTS ####

cairo_pdf(out_file, width = 7.5, height = 5)

par(mfrow = c(2, 3), oma=c(0,0.1,1.35,0.1), bg = "black")

### S America ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = samerica_ext)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = samerica_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE, ext = samerica_ext)
box(col = "white")
mtext(3, text="c", col = "white", line = 0.25, adj = 0)

### Africa ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext, add = TRUE)
box(col = "white")
mtext(3, text="d", col = "white", line = 0.25, adj = 0)

### SE Asia ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = seasia_ext)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = seasia_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirv_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
box(col = "white")
mtext(3, text="e", col = "white", line = 0.25, adj = 0)

### S America ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = samerica_ext)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = samerica_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE, ext = samerica_ext)
box(col = "white")
mtext(3, text="c", col = "white", line = 0.25, adj = 0)

### Africa ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = africa_ext, add = TRUE)
box(col = "white")
mtext(3, text="d", col = "white", line = 0.25, adj = 0)

### SE Asia ###

op <- par(mar = c(2.0,0,0,0), bg = "black")
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = seasia_ext)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ext = seasia_ext, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE)
plot(cs_nirvr_r2, col=r2.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
box(col = "white")
mtext(3, text="e", col = "white", line = 0.25, adj = 0)

dev.off()

