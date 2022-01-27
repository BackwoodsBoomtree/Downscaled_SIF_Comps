library(raster)
library(rgdal)
library(rgeos)
library(viridis)
library(pals)

options(scipen=999)

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

# Data #
cf20_diff <- raster("G:/SIF_comps/min_max_diff/20km/cf20_vis/NIRv_Rad.CF20-SIF.max.CF20.monthly.20km.2020.tif")

# Mask rasters by veg
m         <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.0.2.tif") # Veg mask
cf20_diff <- mask(cf20_diff, m)

# Mean
cf20_diff_mean   <- cellStats(cf20_diff, stat = 'mean', na.rm = TRUE)

# Set thresholds for mapping
cf20_diff[cf20_diff < -3] <- -3
cf20_diff[cf20_diff > 3]  <- 3

# Crop to extent
samerica_ext <- extent(c(-82,-34,-15,13))
africa_ext   <- extent(c(7,45,-5,5))
seasia_ext   <- extent(c(95,155,-11,10))

samerica_diff <- crop(cf20_diff, samerica_ext)
africa_diff   <- crop(cf20_diff, africa_ext)
seasia_diff   <- crop(cf20_diff, seasia_ext)

# samerica_ext <- extent(c(-82,-34,-15,13))
# africa_ext   <- extent(c(7,45,-5,5))
# seasia_ext   <- extent(c(95,155,-11,10))
# 
# samerica_coast <- crop(coastlines, extent(c(-82,-60,-15,30)))
# africa_coast   <- crop(cf20_diff, extent(c(7,45,-5,5)))
# seasia_coast   <- crop(cf20_diff, extent(c(95,155,-11,10)))

# Colors
# diff.col <- coolwarm(7)
diff.col <- rev(viridis(7))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/inset_bottom_peak_CF20_black.pdf", width = 7.5, height = 2.5)

par(mfrow = c(1, 3), oma=c(0.1,0.1,1.25,0.1), bg = "black")

### S America ###

op <- par(mar = c(0,0,0,0), bg = "black")
plot(samerica_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F)
plot(samerica_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE, ylim = c(-60, 30))
plot(samerica_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
box(col = "white")
mtext(3, text="a", cex=0.85, col = "white", line = 0.25, adj = 0)

### Africa ###

op <- par(mar = c(0,0,0,0), bg = "black")
plot(africa_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F)
plot(africa_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE, ylim = c(-60, 30))
plot(africa_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
box(col = "white")
mtext(3, text="b", cex=0.85, col = "white", line = 0.25, adj = 0)

### SE Asia ###

op <- par(mar = c(0,0,0,0), bg = "black")
plot(seasia_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F)
plot(seasia_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", add = TRUE, ylim = c(-60, 30))
plot(seasia_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, add = TRUE)
box(col = "white")
mtext(3, text="c", cex=0.85, col = "white", line = 0.25, adj = 0)



# # Bar plot
# par(new=TRUE)
# op <- par(mar = c(1.5, 0.75, 8, 28)) # Set margins
# barplot(cf20_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
# abline(v = (cf20_diff_mean + 3.5), col = "red")
# axis(3, tck = F, labels = round2(cf20_diff_mean, 3), at = (cf20_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
# barplot(cf20_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)
# 
# axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
#      mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
# box(col = "white")

dev.off()

