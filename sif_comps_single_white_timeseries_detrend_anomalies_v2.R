library(viridis)
library(corrplot)
library(tidyverse)
library(tibbletime)
library(anomalize)
library(timetk)


out_name    <- "G:/SIF_comps/figs/v2/sif_comps_single_white_timeseries_anomalies_v2.pdf"

# SIF data
tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean_timeseries.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2022_sifd_740_mean_qc_nadir_timeseries.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_740_mean_qc_nadir_timeseries.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2_timeseries.csv", header = TRUE)

tropo_sif_mean <- tropo_sif_data$Mean
oco2_sif_mean  <- oco2_sif_data$Mean
oco3_sif_mean  <- oco3_sif_data$Mean
gome2_sif_mean <- gome2_sif_data$Mean

tropo_sif_data  <- read.csv("G:/SIF_comps/csv/tropomi/Amazon_TROPOSIF_L2B_2019-2021_sifd_mean.csv", header = TRUE)
oco2_sif_data   <- read.csv("G:/SIF_comps/csv/oco2/Amazon_OCO2_L2B10_2015-2021_sifd_740_mean_qc_nadir.csv", header = TRUE)
oco3_sif_data   <- read.csv("G:/SIF_comps/csv/oco3/Amazon_OCO3_L2B10_2019-2022_sifd_740_mean_qc_nadir.csv", header = TRUE)
gome2_sif_data  <- read.csv("G:/SIF_comps/csv/gome2/Amazon_NSIFv2.6.2.GOME-2A_2011-2018_sifd_mean_qc2.csv", header = TRUE)

tropo_sif_mean_a <- tropo_sif_data$Mean
oco2_sif_mean_a  <- oco2_sif_data$Mean
oco3_sif_mean_a  <- oco3_sif_data$Mean
gome2_sif_mean_a <- gome2_sif_data$Mean

tropo_sif_mean_a <- rep(tropo_sif_mean_a, 3)
oco2_sif_mean_a  <- rep(oco2_sif_mean_a, 7)
oco3_sif_mean_a  <- rep(oco3_sif_mean_a, 4)
gome2_sif_mean_a <- rep(gome2_sif_mean_a, 8)


tropo_sif_mean_anom <- tropo_sif_mean - tropo_sif_mean_a
oco2_sif_mean_anom  <- oco2_sif_mean - oco2_sif_mean_a
oco3_sif_mean_anom  <- oco3_sif_mean - oco3_sif_mean_a
gome2_sif_mean_anom <- gome2_sif_mean - gome2_sif_mean_a

# 
# 
# tropo_sif_dates <- seq(as.Date("2019-01-01"), as.Date("2021-12-31"), "months")
# oco2_sif_dates  <- seq(as.Date("2015-01-01"), as.Date("2021-12-31"), "months")
# oco3_sif_dates  <- seq(as.Date("2019-01-01"), as.Date("2022-12-31"), "months")
# gome2_sif_dates <- seq(as.Date("2011-01-01"), as.Date("2018-12-31"), "months")
# 
# df_tropo <- data.frame(tropo_sif_dates, tropo_sif_mean)
# df_oco2  <- data.frame(oco2_sif_dates, oco2_sif_mean)
# df_oco3  <- data.frame(oco3_sif_dates, oco3_sif_mean)
# df_gome2 <- data.frame(gome2_sif_dates, gome2_sif_mean)
# 
# # Store as tibble
# df_tropo <- as_tibble(df_tropo)
# df_oco2  <- as_tibble(df_oco2)
# df_oco3  <- as_tibble(df_oco3)
# df_gome2 <- as_tibble(df_gome2)
# 
# # Anomalize
# 
# df_tropo_anomalized <- df_tropo %>%
#   time_decompose(tropo_sif_mean, frequency = "auto", trend = "36 months") %>%
#   anomalize(remainder) %>%
#   time_recompose()
# df_tropo_anomalized %>% glimpse()
# 
# df_oco2_anomalized <- df_oco2 %>%
#   time_decompose(oco2_sif_mean, merge = TRUE) %>%
#   anomalize(remainder) %>%
#   time_recompose()
# df_oco2_anomalized %>% glimpse()
# 
# df_oco3_anomalized <- df_oco3 %>%
#   time_decompose(oco3_sif_mean, merge = TRUE) %>%
#   anomalize(remainder) %>%
#   time_recompose()
# df_oco3_anomalized %>% glimpse()
# 
# df_gome2_anomalized <- df_gome2 %>%
#   time_decompose(gome2_sif_mean, merge = TRUE) %>%
#   anomalize(remainder) %>%
#   time_recompose()
# df_gome2_anomalized %>% glimpse()
# 
# 
# df_tropo_anomalized %>% plot_anomalies(ncol = 3, alpha_dots = 0.75)

# expand data for plotting for 2011 - 2022
tropo_sif_mean_anom <- c(rep(NA, 96), tropo_sif_mean_anom)
oco2_sif_mean_anom  <- c(rep(NA, 48), oco2_sif_mean_anom)
oco3_sif_mean_anom  <- c(rep(NA, 96), oco3_sif_mean_anom)

# # MCD43C4 data
# get_monthly_ts_means <- function(ts_data) {
#   df_m    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,1], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,1])/12)))
#   df_n    <- as.data.frame(split(read.csv(ts_data, header = TRUE)[,2], ceiling(seq_along(read.csv(ts_data, header = TRUE)[,2])/12)))
#   
#   for (i in 1:nrow(df_m)) {
#     mean_w <- weighted.mean(df_m[i,], df_n[i,])
#     if (i == 1) {
#       means_w <- mean_w
#     } else {
#       means_w <- c(means_w, mean_w)
#     }
#   }
#   
#   return(means_w)
# }
# mcd_nirv <- get_monthly_ts_means("G:/SIF_comps/csv/mcd43c4/Amazon_EBF90_2019-2021_monthly_NIRv.csv") / 10000


#### Plot ####
inf.cols   <- inferno(11)
vir.cols   <- viridis(11)
sifg.col   <- vir.cols[3]
sift.col   <- vir.cols[5]
sifo2.col  <- vir.cols[7]
sifo3.col  <- vir.cols[9]
nirv.col   <- "#00000025"
pa.col     <- inf.cols[3]
sza.col    <- inf.cols[7]
vza.col    <- inf.cols[5]
y_sif      <- c(-0.15, 0.15)
y_nirv     <- c(0.25, 0.33)
y_lab_sif  <- bquote("SIF740"[Daily]*" (mW/m"^"2"*"/sr/nm)")
y_lab_nirv <- bquote("NIRv")
x_len      <- c(1,144)
x_lab      <- seq(2011,2022)

### Plot
cairo_pdf(out_name, width = 8, height = 3)

par(mfrow = c(1, 1), oma=c(2.5,3.50,0.5,0.5))

op <- par(mar = c(0,0,0,0))

plot(NA, xlim = x_len, ylim = y_sif, col = nirv.col, axes = FALSE, xaxs="i", type = "o", lwd = 2, pch = 4, cex = 0.75, lty = 1)

abline(h = 0)
lines(gome2_sif_mean_anom, col = sifg.col, type = "o", lwd = 1, pch = 17, cex = 0.75, lty = 1)
lines(tropo_sif_mean_anom, col = sift.col, type = "o", lwd = 1, pch = 16, cex = 0.75, lty = 1)
lines(oco2_sif_mean_anom, col = sifo2.col, type = "o", lwd = 1, pch = 15, cex = 0.75, lty = 1)
lines(oco3_sif_mean_anom, col = sifo3.col, type = "o", lwd = 1, pch = 18, cex = 0.75, lty = 1)
axis(2, tck = 0.03, mgp=c(3, 0.2, 0), las = 2)

# 
# legend("topleft", legend = c("GOME-2", "TROPOMI", "OCO-2", "OCO-3", "NIRv"), ncol = 3, lwd = c(1,1,1,1,2),
#        col = c(sifg.col, sift.col, sifo2.col, sifo3.col, nirv.col), lty = c(2,1,2,1,1), pch = c(17,16,15,18,4),
#        box.col = "transparent", bg = "transparent")


legend("topright", legend = c("GOME-2", "TROPOMI", "OCO-2", "OCO-3"), ncol = 2, lwd = c(1,1,1,1),
       col = c(sifg.col, sift.col, sifo2.col, sifo3.col), lty = c(1,1,1,1), pch = c(17,16,15,18),
       box.col = "transparent", bg = "transparent")

axis(1, tck = 0.04, mgp=c(3, 0.2, 0), labels = x_lab, at = seq(1, 144, by = 12))
axis(1, tck = 0.03, labels = FALSE, at = seq(7, 144, 12))

mtext(1, text = "Month of Year", line = 1.5, outer = TRUE)
mtext(2, text = y_lab_sif, line = 1.8)
mtext(4, text = y_lab_nirv, line = 2, outer = FALSE)
box()

dev.off()


summary(lm(tropo_sif_mean~gome2_sif_mean))
summary(lm(tropo_sif_mean~oco2_sif_mean))
summary(lm(tropo_sif_mean~oco3_sif_mean))
summary(lm(oco2_sif_mean~oco3_sif_mean))
summary(lm(oco2_sif_mean~gome2_sif_mean))
summary(lm(oco3_sif_mean~gome2_sif_mean))


df <- cbind(tropo_sif_mean, gome2_sif_mean, oco2_sif_mean, oco3_sif_mean)
colnames(df) <- c("TROPO", "GOME-2", "OCO2", "OCO3")

df_cor <- round(cor(df),  2)
p.mat  <- cor.mtest(df)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(df_cor, method="color", col=col(40),  
         order="original", cl.cex = 1,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "pch",
         pch.cex = 6, pch.col = "#00000050",
         diag=TRUE)

