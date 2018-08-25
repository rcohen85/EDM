
setwd("~/Home/School/EDM/class_proj/SoCal_Data")
library(rEDM)
library(lubridate)
# Site & bin length:
site = "N"
bn = "2 days"

# Load master dataframe, make into time series recognizable by later functions:
load(paste("Master_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))
ts_matrix = master
lib = c(1,length(ts_matrix$bin))
pred = lib

save(ts_matrix,lib,pred,file=paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))
load(paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))

# SIMPLEX & SMAP ----------------------------------------------------------

E_list = c(1:20)
theta_list = c(0,0.001,0.003,0.01,0.03,0.1,0.3,0.5,1,1.5,2,2.5,3,4,5,6,7,8)

## Change site & bin length in variables below: ##
for (i in 1:length(ts_matrix[1,])){
simplex_out_N3day <- simplex(ts_matrix[,i], lib = lib, pred = lib, E = E_list)
plot(E_list,simplex_out_N3day$rho, type = "l", main = colnames(ts_matrix[i]))
}

## Change site & bin length in variables below: ##
for (i in 1:length(ts_matrix[1,])){
  smap_N3day = s_map(ts_matrix[,i], lib = lib, pred = lib, E = 1, theta = theta_list)
  plot(smap_N3day$theta, smap_N3day$rho, type = "l",main = colnames(ts_matrix[i]), xlab = "theta", ylab = "rho")
}


# CORRELATION EXPLORATION -------------------------------------------------
# Determine correlation btwn clicks & all other individual covariates
corr_mat = data.frame(colnames(master[3:26]), Corr = 1:24)
for (i in 3:26){
  c = cor(master$Clicks, master[,i], use = "pairwise.complete.obs")
  corr_mat[i-2,2] = c
}

save(corr_mat,file = paste("Corr_Mat_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))
write.csv(corr_mat,file = paste("Corr_Mat_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".csv", sep = ""))


# TS MATRIX FUNCTIONS -----------------------------------------------------
## Stitch together longest continuous data chunks
# ts_matrix = ts_matrix[c(232:272,284:328,336:375,390:434,436:474,480:523, 525:567, 637:675,778:818),]
# rownames(ts_matrix) <- c(1:length(ts_matrix[,1]))
# lib = rbind(c(1,41),c(42,86),c(87,126),c(127,171),c(172,210),c(211,254),c(255,297),c(298,336),c(337,377))
# pred = lib
# 
# save(ts_matrix,lib,pred,file="3day_master_ts_ld.Rdata")
# load("3day_master_ts_ld.Rdata")

# source("rEDM additional functions/simplex_smap.R")
# simplex_smap_bestE(in_file = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""), 
#                    out_file = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,"_simp_smap.Rdata", sep = ""),
#                    Best_E_File = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,"_bestE.csv", sep = ""))

source("rEDM additional functions/ccm_network.R")
compute_ccm_web(in_file = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""),
                best_E_file = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,"_bestE.csv", sep = ""),
                out_file = paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,"_ccm.Rdata", sep = ""), 
                num_surr = 1000,progress_bar = TRUE, tp_select = 0)

dev.off() #reset figure parameters

load(paste("MasterTS_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,"_ccm.Rdata", sep = ""))

ccm_matrix[ccm_matrix < 0] = 0
ccm_matrix[ccm_significance > 0.05] = 0 ### ONLY IF USING SURROGATES (removes non-significant values)###

save(ccm_matrix,file = paste("CCM_Mat_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))
write.csv(ccm_matrix,file = paste("CCM_Mat_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".csv", sep = ""))
