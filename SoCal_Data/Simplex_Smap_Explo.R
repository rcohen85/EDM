setwd("~/Home/School/EDM/class_proj/SoCal_Data")
library(rEDM)

load("Master_3daybin_N.Rdata")

lib = c(1,length(master$bin))
pred = lib


# SIMPLEX & SMAP ----------------------------------------------------------

E_list = c(1:20)
theta_list = c(0,0.001,0.003,0.01,0.03,0.1,0.3,0.5,1,1.5,2,2.5,3,4,5,6,7,8)

for (i in 1:length(master[1,])){
simplex_out_N3day <- simplex(master[,i], lib = lib, pred = lib, E = E_list)
plot(E_list,simplex_out_N3day$rho, type = "l", main = colnames(master[i]))
}


for (i in 1:length(master[1,])){
  smap_N3day = s_map(master[,i], lib = lib, pred = lib, E = 1, theta = theta_list)
  plot(smap_N3day$theta, smap_N3day$rho, type = "l",main = colnames(master[i]), xlab = "theta", ylab = "rho")
}

#s_out <- simplex(time_series = hour_clicks$Count[1:10000], lib = c(1,10000), pred = c(1,10000),E = E_list)


# CORRELATION EXPLORATION -------------------------------------------------

cor_mat_1df = data.frame(colnames(day3_master[3:17]), Corr = 1:15)
for (i in 3:17){
  c = cor(day3_master$Clicks,day3_master[,i], use = "pairwise.complete.obs")
  cor_mat_1df [i-2,2] = c
}

day3_envir = day3_envir[1:830,]
cor_mat = data.frame(colnames(day3_master[3:17]), Corr = 1:15)
for (i in 3:17){
  c = cor(day3_master$Clicks,day3_envir[,i], use = "pairwise.complete.obs")
  cor_mat[i-2,2] = c
}

# TS MATRIX FUNCTIONS -----------------------------------------------------

setwd("~/Home/School/EDM/class_proj/SoCal_Data")
library(rEDM)

load("master_raw_3day.Rdata")
ts_matrix = day3_master
lib = c(1,length(ts_matrix$bin))
pred = lib

save(ts_matrix,lib,pred,file="3day_master_ts.Rdata")
load("3day_master_ts.Rdata")


ts_matrix = ts_matrix[c(232:272,284:328,336:375,390:434,436:474,480:523, 525:567, 637:675,778:818),]
rownames(ts_matrix) <- c(1:length(ts_matrix[,1]))
lib = rbind(c(1,41),c(42,86),c(87,126),c(127,171),c(172,210),c(211,254),c(255,297),c(298,336),c(337,377))
pred = lib

save(ts_matrix,lib,pred,file="3day_master_ts_ld.Rdata")
load("3day_master_ts_ld.Rdata")

source("rEDM additional functions/simplex_smap.R")
simplex_smap_bestE(in_file = "3day_master_ts.Rdata", 
                   out_file = "3day_master_ts_simplex_smap.Rdata",
                   Best_E_File = "3day_master_ts_best_E.csv")

source("rEDM additional functions/ccm_network.R")
compute_ccm_web(in_file = "3day_master_ts.Rdata",
                best_E_file = "3day_master_ts_best_E.csv",
                out_file = "3day_master_ts_ccm.Rdata", num_surr = 1000,progress_bar = TRUE, tp_select = -3)

load("3day_master_ts_ccm.Rdata")

ccm_matrix[ccm_matrix < 0] = 0
ccm_matrix[ccm_significance > 0.05] = 0 ### ONLY IF USING SURROGATES (removes non-significant values)###
