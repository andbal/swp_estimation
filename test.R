################################################################################
# DESCRIPTION:  TESTING SUITE for vanGenuchten_swc, with B1_new data
# AUTHOR(s):    Andrea Balotti - balotti.and at gmail.com
#
# Copyright (c) 2016 - Andrea Balotti
################################################################################

### B1 dataset
# SWC data [m^3/m^3] [0 < x < 1] -> non % value
# SWP data [kPa]

### vanGenuchten_swc input
# swc       [cm^3/cm^3]
# swp       [cm] = 0.0980665 [kPa]
# alpha     [cm^-1]
# n         [-]
# theta_res [cm^3/cm^3]
# theta_sat [cm^3/cm^3]

library(ggplot2)
library(gridExtra)
library(zoo)
source('~/GitHub/DataBaseAlpEnvEURAC/R/vanGenuchten_swc.R')

# B1 dataset
path <- "C:/Users/ABalotti/Documents/GitHub/swp_estimation"
file <- "B1_soil.csv"

# vanGenuchten B1 parameter
path_par <- "C:/Users/ABalotti/Documents/GitHub/swp_estimation"
file_par <- "B1_parameter.csv"
tmp_par <- read.csv(file = file.path(path_par,file_par), header = TRUE,
                    stringsAsFactors = FALSE)
para <- as.numeric(tmp_par)
names(para) <- names(tmp_par)

# read dataset
soil <- read.csv(file = file.path(path,file), header = FALSE, skip = 3,
                 stringsAsFactors = FALSE)
head <- read.csv(file = file.path(path,file), header = FALSE, nrows = 1,
                 stringsAsFactors = FALSE)
names(soil) <- head

# format date/time and separate it from data
soil$TIMESTAMP <- strptime(x = soil$TIMESTAMP,format = "%Y-%m-%d %H:%M")
soil_time <- soil$TIMESTAMP
soil_data <- soil[-1]

# zoo obj. ordered by date/time
# ts <- zoo(x = soil_data, order.by = soil_time)
# plot(ts)

# subset for 50cm depth
soil50 <- cbind.data.frame(SWC_50=soil$SWC_50,SWP_50=soil$SWP_50)

# plot original timeseries 50cm
p1 <- ggplot(mapping = aes(x=soil_time,y = soil50$SWC_50)) +
    geom_line() +
    ylab("SWC 50 cm [cm3/cm3]")
p2 <- ggplot(mapping = aes(x=soil_time,y = soil50$SWP_50)) +
    geom_line() +
    ylab("SWP 50 cm [cm or hPa]")
grid.arrange(p1,p2,ncol=1,nrow=2)

# convert SWP to [cm] from [kPa]
soil50$SWP_50 <- (soil50$SWP_50 / 0.0980665)

# compute effective SWC/SWP
out_swc <- vanGenuchten_swc(psi = as.numeric(soil50$SWP_50), alpha = para[8], 
                            n = para[9], theta_res = para[4], theta_sat = para[7] )
out_psi <- vanGenuchten_swc(swc = as.numeric(soil50$SWC_50), alpha = para[8], 
                            n = para[9], theta_res = para[4], theta_sat = para[7], inv = TRUE )

# convert out_psi from [cm] to [kPa]
out_psi <- (out_psi * 0.0980665)

# compute teoretical pedofunction
t_psi <- runif(length(soil50$SWP_50),min=0,max=1000)
t_swc <- vanGenuchten_swc(psi = t_psi, alpha = para[8], n = 2,
                          theta_res = para[4], theta_sat = para[7] )
# # create zoo obj
# zoo50 <- zoo(x = cbind(out_swc,out_psi),order.by = soil_time)
# plot(zoo50)

# plot effective pedofunction
pd1 <- ggplot(mapping = aes(x = sort(soil50$SWC_50),
                            y = sort(abs(out_psi)) ) ) +
    geom_line() +
    geom_line(aes(x = t_swc, y = t_psi) ) +
    scale_y_log10()
pd2 <- ggplot(mapping = aes(x = sort(out_swc),
                            y = sort(abs(soil50$SWP_50)) ) ) +
    geom_line() +
    geom_line(aes(x = t_swc, y = t_psi) ) +
    scale_y_log10()
grid.arrange(pd1,pd2,ncol=1,nrow=2)

# # plot pF between 0-7
# plot(soil50$SWC_50,log((abs(soil50$SWP_50)),base = 10),ylim=c(0,7))
# plot(out_swc,log((abs(out_psi)),base = 10),ylim=c(0,7))
