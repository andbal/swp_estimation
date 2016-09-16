# Cloned from Johannes Brenner script (github: JBrenn)
# for MONALISA


# Modified version of package can be found on:
# https://github.com/andbal/DataBaseAlpEnvEURAC

# Note:
# - vanGenuchten_swc() is a modified version of FUN implemented in "DataBaseAlpEnvEURAC"
#                      to support also model inversion (reconstruct SWP from SWC)

library(Helper4me)  # https://github.com/JBrenn/Helper4me
library(zoo)
library(chron)
library(dygraphs)   # facilities for charting time-series data

library(soilwater)  # formulas of soil water retention or conductivity curve (E.Cordano)
library(ggplot2)
library(gtable)     # make it easier to work with "tables" of "grobs" (-> grid object)
library(ggExtra)    # enhance 'ggplot2', add marginal histograms/boxplots/density plots
library(gridExtra)  # functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables.
library(grid)       # enhance base graphics

library(AnalyseGeotop)          # https://github.com/JBrenn/AnalyseGEOtop
library(DataBaseAlpEnvEURAC)    # https://github.com/JBrenn/DataBaseAlpEnvEURAC
source('~/GitHub/AnalyseGEOtop/R/Geotop_VisSoilWaterRet_gg.R')
source('~/GitHub/DataBaseAlpEnvEURAC/R/vanGenuchten_swc.R')

data_soil <- list()

### SMC / SWP read data  

# # read data Arduino
# wpath <- "H:/Projekte/MONALISA/04_Daten & Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/"
# dpath <- file.path(wpath,"data")
# files <- dir(dpath)
#
# for (i in files)
# {
#   df <- read.csv(file.path(dpath,i), header = T, na.strings = c("NA","NaN"))
#   
#   date_split <- matrix(unlist(strsplit(as.character(df$Date), " ")), nrow = dim(df)[1], byrow = TRUE)
#   datetime <- help_parseDateTime(x.date = date_split[,1], x.time = date_split[,2], 
#                                  format = list(date="%d/%m/%Y", time="%H:%M"))
#   # zoo object
#   data_zoo <- zoo(df[,-1], datetime)
#   
#   # hourly aggregation
#   data_zoo_h <- help_zooAggregation(data_zoo, na.rm = T)
#   
#   name <- substr(i,1,nchar(i)-4)
#   pdf(file = file.path(wpath, paste(name,"pdf",sep=".")), width = 14, height = 10)
#     plot(data_zoo_h, main = i)
#   dev.off()
#   
#   data_soil[[name]] <- data_zoo_h
# }
 
# read data BERATUNGSRING
path2data  <- "H:/Projekte/MONALISA/05_Arbeitsbereiche/BrJ/01_data/Beratungsring/"
prefix <- "BERAT"
# station_nr <- c(3,7,9,12,14,17,30,37,39,52,70,84,103,105,106,125,169,171,172,174,176)
station_nr <- c(3)
stations <- paste(prefix, formatC(station_nr, width = 4, flag = "0"), sep="")

for (i in stations)
{
  beratdat <- dB_getSWC(path2data = path2data, station = i, aggregation = "h", minVALUE = 0, maxVALUE = 100, write.csv = F)
  whichcol <- !apply(beratdat, 2, function(x) all(is.na(x)))
  if (any(whichcol)) {
    coredata(beratdat) <- coredata(beratdat)[,whichcol]
    data_soil[[i]] <- beratdat
    colnames(data_soil[[i]]) <- c("SWC_A_20", "SWC_A_40")
  } else {
    print(paste("no data for station", i))
  }
}
 
# # data MONALISA
# path2data <- "/media/alpenv/Projekte/MONALISA/05_Arbeitsbereiche/BrJ/01_data/Stations/"
# stations <- c("DOMEF1500", "DOMES1500", "DOPAS2000")
# 
# for (i in stations)
# {
#   data_soil[[i]] <- dB_getSWC(path2data = path2data, station = i, aggregation = "h", minVALUE = 0, maxVALUE = 100, write.csv = F)
#   colnames(data_soil[[i]]) <- c("SWC_A_02", "SWC_A_05", "SWC_A_20")
# }

save(list = "data_soil", file = "H:/Projekte/MONALISA/04_Daten_Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/data_soil_test.RData")

# load("H:/Projekte/MONALISA/04_Daten_Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/data_soil.RData")
load("H:/Projekte/MONALISA/04_Daten_Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/data_soil_test.RData")

# soil water retention curve (parameter from sample analysis)
# parameter_file <-  "/media/alpenv/Projekte/MONALISA/04_Daten_Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/soil sample.csv"
parameter_file <-  "H:/Projekte/MONALISA/04_Daten_Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/soil sample.csv"
para <- read.csv2(file = parameter_file, header = T)

for (i in unique(para$dataName))
{
    # %in% is matching operator
    # return true when left operator match one of right operator elements
    if (i %in% names(data_soil)) {
    
    print(paste("create figure for site", i))
    para_i <- para[para$dataName == i,]
    
    # Soil measured parameter (change for each station)
    alpha = para_i$alpha
    n = para_i$n
    theta_sat = para_i$thetaS
    theta_res = rep(.05,length(para_i$alpha))
    accurate = 5
    
    # # soil water pressure head in centimeter / hPa
    # psi <- seq(1,10000000,accurate)
    # 
    # # SWC in vol%
    # # SWP in hPa ???
    # swc <- list()
    # swp <- list()
    # 
    # # index of NAs values
    # ind_swc = which(is.na(obs$SWC))
    # ind_swp = which(is.na(obs$SWP))
    # 
    # # fill SWC (SWP) NAs where SWP (SWC) is recorded
    # for (i in 1:length(alpha)) {
    #     # swc[[i]] <- swc(psi = -psi, alpha = alpha[i], n = n[i], theta_sat = theta_sat[i], theta_res = theta_res[i]) *100
    #     if ( all(is.na(obs$SWC)) )
    #         swc[ind_swc] <- vanGenuchten_swc(psi = obs$SWP[ind_swc], alpha = alpha[i], n = n[i], 
    #                                          theta_sat = theta_sat[i], theta_res = theta_res[i]) *100
    #     if ( all(is.na(obs$SWP)) )
    #         swp[ind_swp] <- vanGenuchten_swc(swc = obs$SWC[ind_swp], alpha = alpha[i], n = n[i], 
    #                                          theta_sat = theta_sat[i], theta_res = theta_res[i], inv = T)
    # }
    # 
    
    # create total df
    obs_all <- matrix(NA, ncol = 3, dimnames = list(NULL,c("SWC","SWP","depth")))
    # obs_myall <- data.frame()
    
    for (k in 1:length(para_i$depth))
    {
        dep <- as.character(para_i$depth[k])
        print(paste("depth: ", dep, sep=""))
        
        if (!length(grep("BERAT", i))==0) {
        
        depth <- as.integer(strsplit(x = as.character(dep), split = "-")[[1]])[2]
        datanames <- names(data_soil[[i]])
        data2use <- as.integer(substr(datanames, nchar(datanames)-1, nchar(datanames)))
        data2use <- which(data2use == depth)
        
        } else if (!length(grep("Arduino", i))==0) {
        
        depth <- as.integer(strsplit(x = as.character(dep), split = "-")[[1]])[2]
        datanames <- names(data_soil[[i]])
        data2use <- as.integer(substr(datanames, nchar(datanames)-1, nchar(datanames)))
        data2use <- which(data2use == depth)
        
        } else {
        
        #depth <- as.integer(strsplit(x = as.character(dep), split = "-")[[1]])
        depths <- as.integer(strsplit(x = as.character(dep), split = "-")[[1]])
        depth  <- mean(depths)
        datanames <- names(data_soil[[i]])
        data2use <- as.integer(substr(datanames, nchar(datanames)-1, nchar(datanames)))
        #data2use <- c(which(data2use == depths[1]), which(data2use == depths[2]))
        if (min(abs(data2use-depth)) > 10) data2use <- NULL else data2use <- which.min(abs(data2use-depth))
        #data2use <- c(which(data2use == depth))
        
        }
        
        obs <- data_soil[[i]][,data2use]
        obs_names <- names(data_soil[[i]])[data2use]
        
        if (length(data2use)==0) {
            # obs <- data.frame(SWC = coredata(obs), SWP = as.numeric(NA)) # ???? why if data2use == 0, obs is not null????
            obs <- data.frame(SWC = as.numeric(NA), SWP = as.numeric(NA))
        } else {
            # return 1 if XXX is in obs_names, zero otherwise
            swc <- grep("SWC", obs_names) 
            swp <- grep("SWP", obs_names)
            # if zero set NA, otherwise set the proper data column
            if (length(swp)==0) SWP <- as.numeric(NA) else {
                if (is.null(dim(coredata(obs)))) SWP <- coredata(obs) else SWP <- c(coredata(obs)[,swp]) }
            if (length(swc)==0) SWC <- as.numeric(NA) else {
                if (is.null(dim(coredata(obs)))) SWC <- coredata(obs) else SWC <- c(coredata(obs)[,swc]) }

            # obs <- data.frame(SWC=SWC, SWP=SWP)
            # swcname <- paste("SWC_",depth,sep = "")
            # swpname <- paste("SWP_",depth,sep = "")
            obs <- data.frame(SWC, SWP)
            # colnames(obs) <- c(swcname,swpname)
        }
        
        # omogenize df: use NAs instead of NANs (if exist)
        obs <- as.data.frame(apply(obs, 2, function(x) ifelse(is.nan(x), NA, x)))
        
        # if not NAs rescale in proper range
        # if SWC [0-1], rescale in [0-100]
        if(!all(is.na(obs$SWC))){
          if (mean(obs$SWC, na.rm = TRUE) < 1) obs$SWC <- obs$SWC *100
        }
        # if SWP < 0, make it positive (as needed to plot it in log scale)
        if(!all(is.na(obs$SWP))){
          if (mean(obs$SWP, na.rm = TRUE) < 0) obs$SWP <- obs$SWP *(-1)
          obs$SWP <- ifelse(obs$SWP<=1, NA, obs$SWP)
        }
        
        # Reconstruct SWC/SWP series
        # fill SWC (SWP) NAs where SWP (SWC) is measured
        
        # index of NAs values
        ind_swc = which(is.na(obs$SWC))
        ind_swp = which(is.na(obs$SWP))
        
        # if NAs compute value with Van Genuchten model
        if (all(is.na(obs$SWC)))    
            obs$SWC[ind_swc] <- vanGenuchten_swc(psi = obs$SWP[ind_swc], alpha = alpha[k], n = n[k],
                                                 theta_sat = theta_sat[k], theta_res = theta_res[k])
        if (all(is.na(obs$SWP)))
            obs$SWP[ind_swp] <- -10^(vanGenuchten_swc(swc = obs$SWC[ind_swp], alpha = alpha[k], n = n[k],
                                                 theta_sat = theta_sat[k], theta_res = theta_res[k], inv = T))
        
        # swcname <- paste("SWC_",depth,sep = "")
        # swpname <- paste("SWP_",depth,sep = "")
        # colnames(obs) <- c(swcname,swpname)

        # add depth column and bind to total df
        obs$depth <- depth
        obs_all <- rbind(obs_all, obs)
        # obs_myall <- cbind(obs_myall, obs)
    }
    
    obs_all <- as.data.frame(obs_all[-1,])
    plot(zoo(obs_all))
    
    # colors <- as.character(para_i$depth)
    # colors <- gsub(pattern = c("0-5"), replacement = c("#999999"), x = colors)
    # colors <- gsub(pattern = c("0-20"), replacement = c("red"), x = colors)
    # colors <- gsub(pattern = c("20-40"), replacement = c("#E69F00"), x = colors)
    # colors <- gsub(pattern = c("40-60"), replacement = c("blue"), x = colors)
    # colors <- gsub(pattern = c("60-80"), replacement = c("darkgreen"), x = colors)
      
      
    # gg <- Geotop_VisSoilWaterRet_gg(alpha = para_dep$alpha, n = para_dep$n, theta_sat = para_dep$thetaS, theta_res = .05, 
    #                                 accurate = 1, 
    #                                 add_ref_curves = T, observed = obs)
    
    
    
    # gg <- Geotop_VisSoilWaterRet_gg(alpha = para_i$alpha, n = para_i$n, theta_sat = para_i$thetaS,
    #                                 theta_res = rep(.05,length(para_i$alpha)),
    #                                 accurate = 5,
    #                                 add_ref_curves = T, observed = obs_all, colors = colors)
    
    # ggsave(gg, filename = paste(i, ".png", sep=""))
 
    } else {
        print(paste("no SMC/SWP data for site", i))
    }# end IF
}# end FOR

# als <- zoo(obs_all, index(beratdat))
