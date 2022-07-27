library(terra)
library(ncdf4)

t_2018 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2018/TROPOMI.ESA.SIF.2018.EBF.monthly.1deg.CF80.nc"
t_2019 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2019/TROPOMI.ESA.SIF.2019.EBF.monthly.1deg.CF80.nc"
t_2020 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2020/TROPOMI.ESA.SIF.2020.EBF.monthly.1deg.CF80.nc"
t_2021 <- "G:/TROPOMI/esa/gridded/1deg/monthly/ebf/2021/TROPOMI.ESA.SIF.2021.EBF.monthly.1deg.CF80.nc"

k34_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K34_PC.csv", header = FALSE)[,2]
k67_pc <- read.csv("G:/SIF_comps/figs/Wu_2016/K67_PC.csv", header = FALSE)[,2]

k34_coords <- cbind(-60.2093, -2.6091)
k67_coords <- cbind(-54.959, -2.857)
k34        <- vect(k34_coords, crs="+proj=longlat +datum=WGS84")
k67        <- vect(k67_coords, crs="+proj=longlat +datum=WGS84")

t_sif_2019 <- rast(t_2018, subds = "SIF_Corr_743")
t_sif_2019 <- rast(t_2019, subds = "SIF_Corr_743")
t_sif_2020 <- rast(t_2020, subds = "SIF_Corr_743")
t_sif_2021 <- rast(t_2021, subds = "SIF_Corr_743")


for (i in 1:12){
  step_avg_sif <- app(c(t_sif_2019[[i]], t_sif_2020[[i]], t_sif_2021[[i]]), mean, na.rm = TRUE)
  
  if (i ==1) {
    avg_sif <- step_avg_sif
  } else {
    avg_sif <- c(avg_sif, step_avg_sif)
  }
}

# k34_sif <- extract(avg_sif, k34, ID = FALSE)
# k67_sif <- extract(avg_sif, k67, ID = FALSE)

k34_sif <- extract(t_sif_2021, k34, ID = FALSE)
k67_sif <- extract(t_sif_2021, k67, ID = FALSE)

k34_sif <- as.numeric(k34_sif[1,])
k67_sif <- as.numeric(k67_sif[1,])

plot(k34_sif, type = "l")
par(new = TRUE)
plot(k34_pc, type = "l", col = "red")

plot(k67_sif, type = "l")
par(new = TRUE)
plot(k67_pc, type = "l", col = "red")
