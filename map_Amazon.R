library(terra)
library(raster)
library(RColorBrewer)

#### Output PDF name ####
out_name    <- "G:/SIF_comps/figs/v2/map_Amazon.pdf"

#### Get shapes and forest cover ####

coastlines     <- vect("C:/Russell/R_Scripts/TROPOMI_2/mapping/GSHHS_shp/c/GSHHS_c_L1.shp")
roi            <- vect("G:/Amazon_shp/Amazon_poly.shp") # Amazon
samerica_ext   <- extent(c(-82,-34,-20,13))

mcd12_majority <- rast("G:/MCD12C1/2020/reprocessed/percent/MCD12C1.A2020001.006.Percent_LC_03.tif")
mcd12_majority <- terra::crop(mcd12_majority, roi, mask = TRUE)
mcd12_majority[mcd12_majority < 90]  <- NA

#### Plot Settings ####
y_lab_map   <- "Forest Cover Percent (%)"
map.cols  <- colorRampPalette(c("#e5f5e0", "#006d2c"))
map.cols  <- (map.cols(11))

#### Plot ####
cairo_pdf(out_name, width = 6.5, height = 3.75)

par(oma=c(2.0,0.1,1.25,0.1))

# Map
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xlim = c(-85, -30), ylim = c(-22, 7), mar = c(0,6,0,0.5), axes = FALSE)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "black")
plot(coastlines, border = NA, col = rgb(0.30,0.30,0.30), xlim = c(-85, -30), ylim = c(-22, 7), mar = c(0,6,0,0.5), axes = FALSE, add = TRUE)
plot(mcd12_majority, col = map.cols, axes = FALSE, add = TRUE, legend = FALSE)
axis(1, labels = TRUE, tck = 0.02, mgp=c(3, 0.1, 0), col.axis = "black", col = "gray50", cex.axis = 0.85)
axis(2, labels = TRUE, tck = 0.02, mgp=c(3, 0.1, 0), col.axis = "black", col = "gray50", cex.axis = 0.85)
lines(roi, col = "gray20")
# mtext(3, text = "Amazon Tropical Forest")
box()
# Legend
plot(raster(mcd12_majority), legend.only=TRUE, col=map.cols, horizontal=F,
     legend.args = list(text = y_lab_map, side = 2, line = 1.25),
     axis.args = list(line = -2, cex.axis=1, tick=F, at=c(90, 100), labels=c("90","100"), hadj = 1),
     smallplot=c(0.085,0.110,0.10,0.90)); par(mar = par("mar"))

dev.off()

