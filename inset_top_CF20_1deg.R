library(raster)
library(rgdal)
library(viridis)
library(pals)

options(scipen=999)

#### Load Map ####

coastlines <- readOGR("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")

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
cf20_diff_max <- raster("G:/SIF_comps/min_max_diff/1deg/cf20/NIRv_Rad.cf20-SIF.max.cf20.monthly.1deg.2021.tif")
cf20_diff_min <- raster("G:/SIF_comps/min_max_diff/1deg/cf20/NIRv_Rad.cf20-SIF.min.cf20.monthly.1deg.2021.tif")


# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif") # Veg mask
cf20_diff_max <- mask(cf20_diff_max, m)
cf20_diff_min <- mask(cf20_diff_min, m)

# Crop to extent
my_ext    <- extent(c(-180,180,-56,80))
cf20_diff_max <- crop(cf20_diff_max, my_ext)
cf20_diff_min <- crop(cf20_diff_min, my_ext)

# Mean
# cf20_diff_max_mean   <- cellStats(cf20_diff_max, stat = 'mean', na.rm = TRUE)

# Set thresholds for mapping
# cf20_diff_max[cf20_diff_max < -3] <- -3
# cf20_diff_max[cf20_diff_max > 3]  <- 3

# Colors
# diff_max.col <- coolwarm(7)
diff_max.col <- rev(viridis(13))
diff_min.col <- rev(plasma(13))

# Labels
labs <- c(paste0("Difference in Peak NIRv Radiance and SIF"),
          paste0("Difference in Minimum NIRv Radiance and SIF"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/inset_top_CF20_1deg_black.pdf", width = 7.5, height = 2.25)

par(mfrow = c(1, 2), oma=c(0.1,0.1,0.1,0.1), bg = "black")

### Global Map ###

# Peak
op <- par(mar = c(0,0,0,0.5), bg = "black")
# plot(cf20_diff_max, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA, ylim = c(-56, 80))
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", ylim = c(-56, 80))
plot(cf20_diff_max, col=diff_max.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ylim = c(-56, 80), add = TRUE)
# box(col = "white")
mtext(3, text=labs[1], cex=0.85, col = "white", line = -1.25)

# Boxes
rect(-82, -15, -34, 13, col = NA, border = "white", lwd = 1)
rect(7, -5, 45, 5, col = NA, border = "white", lwd = 1)
rect(95, -11, 155, 10, col = NA, border = "white", lwd = 1)
text(-89, -13, "a", col = "white")
text(0, -3, "b", col = "white")
text(88, -9, "c", col = "white")

# Minimum
op <- par(mar = c(0,0,0,0.5), bg = "black")
# plot(cf20_diff_min, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA, ylim = c(-56, 80))
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", ylim = c(-56, 80))
plot(cf20_diff_min, col=diff_min.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ylim = c(-56, 80), add = TRUE)
# box(col = "white")
mtext(3, text=labs[2], cex=0.85, col = "white", line = -1.25)

# Boxes
rect(-82, -15, -34, 13, col = NA, border = "white", lwd = 1)
rect(7, -5, 45, 5, col = NA, border = "white", lwd = 1)
rect(95, -11, 155, 10, col = NA, border = "white", lwd = 1)
text(-89, -13, "d", col = "white")
text(0, -3, "e", col = "white")
text(88, -9, "f", col = "white")

# Legends
par(new=TRUE, mfrow = c(1,1))
op <- par(mar = c(1, 0.75, 1, 1)) # Set margins

plot(cf20_diff_min, legend.only=TRUE, col=diff_max.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-6, 0, 6), labels=c("-6", "0", "6"), col.axis = "white"),
     smallplot=c(0.10,0.40,0.060,0.110)); par(mar = par("mar"))

plot(cf20_diff_max, legend.only=TRUE, col=diff_min.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-6, 0, 6), labels=c("-6", "0", "6"), col.axis = "white"),
     smallplot=c(0.60,0.9,0.060,0.110)); par(mar = par("mar"))



# # Bar plot
# par(new=TRUE)
# op <- par(mar = c(1.5, 0.75, 8, 28)) # Set margins
# barplot(cf20_diff_max, col=diff_max.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
# abline(v = (cf20_diff_max_mean + 3.5), col = "red")
# axis(3, tck = F, labels = round2(cf20_diff_max_mean, 3), at = (cf20_diff_max_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
# barplot(cf20_diff_max, col=diff_max.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)
# 
# axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
#      mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
# box(col = "white")

dev.off()

