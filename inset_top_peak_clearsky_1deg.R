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
cs_diff <- raster("G:/SIF_comps/min_max_diff/1deg/clearsky/NIRv_Rad.clearsky-SIF.max.clearsky.monthly.1deg.2021.tif")

# Mask rasters by veg
m  <- raster("G:/SIF_comps/veg_mask/max.monthly.ndvi.1deg.tif") # Veg mask
cs_diff <- mask(cs_diff, m)

# Crop to extent
my_ext    <- extent(c(-180,180,-56,80))
cs_diff <- crop(cs_diff, my_ext)

# Mean
cs_diff_mean   <- cellStats(cs_diff, stat = 'mean', na.rm = TRUE)

# Set thresholds for mapping
# cs_diff[cs_diff < -3] <- -3
# cs_diff[cs_diff > 3]  <- 3

# Colors
# diff.col <- coolwarm(7)
diff.col <- rev(viridis(13))

# Labels
labs <- c(paste0("Difference in Peak NIRv and Peak SIF for Clear Sky"))

##### PLOTS ####

cairo_pdf("G:/SIF_comps/figs/inset_top_peak_clearsky_1deg_black.pdf", width = 7.5, height = 3.25)

par(oma=c(0.1,0.1,1.25,0.1), bg = "black")

### Global Map ###

# Main map
op <- par(mar = c(0,0,0,0), bg = "black")
# plot(cs_diff, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, col = NA, ylim = c(-56, 80))
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xaxs="i", yaxs="i", ylim = c(-56, 80))
plot(cs_diff, col=diff.col, axes=F, xaxs="i", yaxs="i", horizontal=T, legend=F, ylim = c(-56, 80), add = TRUE)
# box(col = "white")
mtext(3, text=labs[1], cex=0.85, col = "white", line = 0.25)

# Boxes
rect(-82, -15, -34, 13, col = NA, border = "white", lwd = 1.5)
rect(7, -5, 45, 5, col = NA, border = "white", lwd = 1.5)
rect(95, -11, 155, 10, col = NA, border = "white", lwd = 1.5)
text(-87, -13, "a", col = "white")
text(2, -3, "b", col = "white")
text(90, -9, "c", col = "white")

# Legend
plot(cs_diff, legend.only=TRUE, col=diff.col, horizontal=T, legend.width=2, legend.shrink=0.75,
     legend.args = list(text=expression(paste("Difference in Months")), side = 1, line = -1.65, cex=0.75, col = "white"),
     axis.args = list(line = -1.05, cex.axis=1,tick=F, at=c(-6, 0, 6), labels=c("-6","0","6"), col.axis = "white"),
     smallplot=c(0.45,0.85,0.060,0.110)); par(mar = par("mar"))

# # Bar plot
# par(new=TRUE)
# op <- par(mar = c(1.5, 0.75, 8, 28)) # Set margins
# barplot(cs_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE)
# rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "black", border = "white")
# abline(v = (cs_diff_mean + 3.5), col = "red")
# axis(3, tck = F, labels = round2(cs_diff_mean, 3), at = (cs_diff_mean + 3.5), mgp=c(3, 0.1, 0), col.axis = "white")
# barplot(cs_diff, col=diff.col, space = 0, border = NA, ylim = c(0,250000),
#         xaxs="i", yaxs="i", xaxt="n", yaxt="n", ann=FALSE, axes=FALSE, add = TRUE)
# 
# axis(side=1, tck=F, las=1, cex.axis=1, labels=c(paste0(intToUtf8(8804), "-3"),"0",paste0(intToUtf8(8805), "3")),
#      mgp=c(3,0.3,0), at=c(0.5, 3.5, 6.5), col.axis = "white")
# box(col = "white")

dev.off()

