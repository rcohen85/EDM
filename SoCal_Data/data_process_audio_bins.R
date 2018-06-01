# Format and bin MFA, BW click, effort, and evironmental data into master time series matrix for
# use in Simplex, S-Maps, and CCM analysis. Assumes environmental data is 1-day resolution or higher,
# no need to insert empty bins between data points.

# TODO: add code to account for lower-res envir data/insert empty bins when bin length < 1 day


setwd("~/Home/School/EDM/class_proj/SoCal_Data")

library(dplyr)
library(tidyr)
library(lubridate)
library(DataCombine)
library(plyr)
library(readtext)

# Monitoring site and desired start/end dates of master matrix; site name will be used 
# to label saved files:
site = "N"
stdt = "2009-01-14 00:00:00"
enddt = "2016-11-10"

# Desired bin length:
bn = "3 days"

# Provide file names to be called for loading data: 
son_dat = paste("SOCAL_",site,"_00_anthro_mfa.csv",sep = "")
click_dat = paste("SOCAL_",site,"_00_Zc_clicks_01.csv",sep = "")
son_eff = paste("SOCAL_",site,"_00_anthro_mfa_effort.csv",sep = "")
click_eff = paste("SOCAL_",site,"_00_Zc_clicks_effort.csv",sep = "")
env_dat = paste("SOCAL",site,".csv",sep = "")


# LOAD SONAR DATA ---------------------------------------------------------

mfa <- read.csv(son_dat, stringsAsFactors = FALSE)

mfa$Start_UTC <- gsub("T", " ", mfa$Start_UTC)
mfa$Start_UTC <- gsub("Z", "", mfa$Start_UTC)

mfa$End_UTC <- gsub("T", " ", mfa$End_UTC)
mfa$End_UTC <- gsub("Z", "", mfa$End_UTC)

nr <- cbind(stdt, stdt, 0, 0, 0, 0) # add first row with desired start date/time
mfa <- InsertRow(mfa, NewRow = nr, RowNum = 1)
end = as.character(as.POSIXlt(enddt,format = "%Y-%m-%d",tz = "GMT") - (1)) # add last row with desired end date/time
nr <- cbind(end, end, 0, 0, 0, 0) 
colnames(nr) = colnames(mfa)
mfa <- InsertRow(mfa, NewRow = nr, RowNum = NULL)

options(digits.secs = 3)
mfa$Start_UTC <- as.POSIXlt(mfa$Start_UTC, format = "%Y-%m-%d %H:%M:%OS",
                            tz = "GMT")
mfa$End_UTC = as.POSIXlt(mfa$End_UTC, format = "%Y-%m-%d %H:%M:%OS",
                         tz = "GMT")

mfa$PPRL_dB = as.numeric(mfa$PPRL_dB)
mfa$SEL_dB = as.numeric(mfa$SEL_dB)
mfa$Start_Hz = as.numeric(mfa$Start_Hz)
mfa$End_Hz = as.numeric(mfa$End_Hz)

# Trim data set to desired date range:
mfa = subset(mfa, Start_UTC >= stdt)
mfa = subset(mfa, Start_UTC <= enddt)


save(mfa, file = paste("mfa_",site,".Rdata", sep = ""))


# LOAD CLICK DATA ---------------------------------------------------------

clicks <- read.csv(click_dat, stringsAsFactors = FALSE)

clicks$Counts <- 1

clicks$Start_UTC <- gsub("T", " ", clicks$Start_UTC)
clicks$Start_UTC <- gsub("Z", "", clicks$Start_UTC)
clicks$Start_UTC <- gsub("[.][0-9]+", "", clicks$Start_UTC)

nr <- cbind(stdt, 0) # add first row with desired start date/time
clicks <- InsertRow(clicks, NewRow = nr, RowNum = 1)
end = as.character(as.POSIXlt(enddt,format = "%Y-%m-%d",tz = "GMT") - (1)) # add last row with desired end date/time
nr <- cbind(end, 0) 
colnames(nr) = colnames(clicks)
clicks <- InsertRow(clicks, NewRow = nr, RowNum = NULL)

clicks$Start_UTC <- as.POSIXct(clicks$Start_UTC, format = "%Y-%m-%d %H:%M:%S",tz = "GMT")
clicks$Counts = as.numeric(clicks$Counts)

# Trim data set to desired date range:
clicks = subset(clicks, Start_UTC >= stdt)
clicks = subset(clicks, Start_UTC <= enddt)

save(clicks, file = paste("clicks_",site,".Rdata", sep = ""))


# LOAD EFFORT DATA --------------------------------------------------------

click_effort = read.csv(click_eff,header=TRUE)
mfa_effort = read.csv(son_eff,header=TRUE)

click_effort$Start_UTC <- gsub("T", " ", click_effort$Start_UTC)
click_effort$Start_UTC <- gsub("Z", "", click_effort$Start_UTC)
click_effort$Start_UTC <- gsub("[.][0-9]+", "", click_effort$Start_UTC)
click_effort$Start_UTC <- as.POSIXct(click_effort$Start_UTC, format = "%Y-%m-%d %H:%M:%S",
                                     tz = "GMT")
click_effort$End_UTC <- gsub("T", " ", click_effort$End_UTC)
click_effort$End_UTC <- gsub("Z", "", click_effort$End_UTC)
click_effort$End_UTC <- gsub("[.][0-9]+", "", click_effort$End_UTC)
click_effort$End_UTC <- as.POSIXct(click_effort$End_UTC, format = "%Y-%m-%d %H:%M:%S",
                                   tz = "GMT")

dur = as.data.frame(difftime(click_effort$End_UTC, click_effort$Start_UTC, tz = "GMT", units = c('days')))


mfa_effort$Start_UTC <- gsub("T", " ", mfa_effort$Start_UTC)
mfa_effort$Start_UTC <- gsub("Z", "", mfa_effort$Start_UTC)
mfa_effort$Start_UTC <- gsub("[.][0-9]+", "", mfa_effort$Start_UTC)
mfa_effort$Start_UTC <- as.POSIXct(mfa_effort$Start_UTC, format = "%Y-%m-%d %H:%M:%S",
                                   tz = "GMT")
mfa_effort$End_UTC <- gsub("T", " ", mfa_effort$End_UTC)
mfa_effort$End_UTC <- gsub("Z", "", mfa_effort$End_UTC)
mfa_effort$End_UTC <- gsub("[.][0-9]+", "", mfa_effort$End_UTC)
mfa_effort$End_UTC <- as.POSIXct(mfa_effort$End_UTC, format = "%Y-%m-%d %H:%M:%S",
                                 tz = "GMT")

save(click_effort,mfa_effort,file = paste("effort_",site,".Rdata", sep = ""))



# LOAD ENVIRONMENTAL DATA -------------------------------------------------
env = read.csv(env_dat, header=TRUE)

env$dates <- gsub("T", " ", env$dates)
env$dates <- gsub("Z", "", env$dates)
env$dates <- gsub("[.][0-9]+", "", env$dates)

env$dates <- as.POSIXct(env$dates, format = "%Y-%m-%d %H:%M:%S",
                            tz = "GMT")

# Extract date range and vars of interest

envir = env[c("dates","mean.chl","mean.npp","mean.sst","ssh","t5","t500","t1000","s5","s500","s1000",
              "curSp5","curSp500","curSp1000","curDir5","curDir500","curDir1000")]

strt = as.POSIXct(stdt) - (60*60*24)
stp = as.POSIXct(enddt)
envir = subset(envir, dates >= strt)
envir = subset(envir, dates <= stp)

envir[is.na(envir)==TRUE] = as.numeric("NA")
envir[,2:length(envir[1,])] = as.numeric(envir[,2:length(envir[1,])])

# First differences:
envir_1df = as.data.frame(matrix(as.numeric("Na"),length(envir$dates)-1,17))

for (i in 2:17){
  envir_1df[,i] = diff(envir[,i],lag = 1)
}

envir = envir[-(1),]

envir_1df[,1] = as.data.frame(envir$dates)
colnames(envir_1df) = colnames(envir)


save(envir,envir_1df,file = paste("envdat_",site,".Rdata", sep = ""))


# BIN DATA - N DAYS -------------------------------------------------------

load(paste("mfa_",site,".Rdata", sep = ""))
load(paste("clicks_",site,".Rdata", sep = ""))
load(paste("effort_",site,".Rdata", sep = ""))
load(paste("envdat_",site,".Rdata", sep = ""))

  # Assign bin numbers to data points:
  mfa$bin = cut(mfa$Start_UTC, breaks = bn, labels = FALSE)
  clicks$bin = cut(clicks$Start_UTC, breaks = bn, labels = FALSE)
  envir$bin = cut(envir$dates, breaks = bn, labels = FALSE)
  envir_1df$bin = cut(envir_1df$dates, breaks = bn, labels = FALSE)
  
  # Sum clicks:
  bin_clicks = as.data.frame(aggregate(x=clicks$Counts, by=list(bin=clicks$bin), FUN=sum))
  bin_clicks = rename(bin_clicks, c("x" = "Count"))

  # Insert empty bins in click data:
  sequence = as.data.frame(seq.int(1,max(bin_clicks$bin),1))
  colnames(sequence) <- "bin"
  sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt)-1,by=bn)
  bin_clicks = merge(sequence,bin_clicks, by = "bin", all = TRUE)
  
  # Create Sonar Variables --------------------------------------------------
      # Max PPRL:
      bin_mfa = as.data.frame(aggregate(x=mfa$PPRL_dB, by=list(bin=mfa$bin), FUN=max))
      bin_mfa = rename(bin_mfa, c("x" = "Max_PPRL_dB"))
      
      # Insert empty bins:
      sequence = as.data.frame(seq.int(1,max(bin_mfa$bin),1))
      colnames(sequence) <- "bin"
      sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt)-1,by=bn)
      bin_mfa = merge(sequence, bin_mfa, by.x = "bin", by.y = "bin", all.x = TRUE)
      
      # Cumulative SEL:
      mfa$bels = mfa$SEL_dB/10
      mfa$intensity = 10^(mfa$bels)
      cum_intensity = as.data.frame(aggregate(x=mfa$intensity, by=list(bin=mfa$bin), FUN=sum))
      cum_intensity = merge(sequence, cum_intensity, by.x = "bin", by.y = "bin", all.x = TRUE)
      bin_mfa$CumSEL_dB = 10*log10(cum_intensity$x)
      
      # Sonar proportion:
      mfa$dur = mfa$End_UTC - mfa$Start_UTC
      secs_per_bin = aggregate(x=mfa$dur, by=list(bin=mfa$bin), FUN=sum)
      secs_per_bin = merge(sequence, secs_per_bin, by.x = "bin", by.y = "bin", all.x = TRUE)
      bin_mfa$secs_per_bin = secs_per_bin$x
      son_prop = as.numeric(bin_mfa$secs_per_bin/(60*60*24*3))
      #colnames(son_prop) = ""
      bin_mfa$son_prop = son_prop
      
      # Standard deviation of SEL:
      sd_SEL = aggregate(x=mfa$SEL_dB, by=list(bin=mfa$bin), FUN=sd) 
      sd_SEL = merge(sequence, sd_SEL, by.x = "bin", by.y = "bin", all.x = TRUE)
      bin_mfa$StDvSEL = sd_SEL$x
      
      # Standard deviation of Sonar proportion: **Relevant time period over which to measure variability?**
       #Looking at variability btwn minutes results in too many data points, gets stuck in a later loop
        #Cut sonar data into smaller bins:
        #mfa$bin_min = cut(mfa$Start_UTC, breaks = "1 min", labels = FALSE)
        mfa$bin_hr = cut(mfa$Start_UTC, breaks = "1 hour", labels = FALSE)
      
        #son_secs_per_min = aggregate(x=mfa$dur, by=list(bin=mfa$bin_min), FUN=sum) # THIS MAY TAKE A WHILE
        son_secs_per_hour = aggregate(x=mfa$dur, by=list(bin=mfa$bin_hr), FUN=sum) # A SLOW-ISH STEP
      
        #names(son_secs_per_min)[2] = "secs"
        names(son_secs_per_hour)[2] = "secs"
      
        #son_secs_per_min$secs = as.numeric(son_secs_per_min$secs)
        son_secs_per_hour$secs = as.numeric(son_secs_per_hour$secs)
      
        # Add sonar-free bins:
        # sequence = as.data.frame(seq.int(1,max(son_secs_per_min$bin),1))
        # colnames(sequence) <- "bin"
        # sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt) - 1,by="1 min")
        # son_secs_per_min = merge(sequence,son_secs_per_min, by = "bin", all = TRUE)
      
        sequence = as.data.frame(seq.int(1,max(son_secs_per_hour$bin),1))
        colnames(sequence) <- "bin"
        sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt) - 1,by="1 hour")
        son_secs_per_hour = merge(sequence,son_secs_per_hour, by = "bin", all = TRUE)
      
       # Replace NAs with zeros for on-effort empty bins: (long loop method, but it works)
        
         # for (i in 1:length(son_secs_per_min$bin)){
          #   for(j in 1:length(mfa_effort$Start_UTC)){
           #     if (son_secs_per_min$Start_UTC[i] >= mfa_effort$Start_UTC[j] && son_secs_per_min$Start_UTC[i] <=
             #         mfa_effort$End_UTC[j] && isTRUE(is.na(son_secs_per_min$secs[i]))){
             #       son_secs_per_min$secs[i] = 0
           #     }
         #   }
         # }
  
          for (i in 1:length(son_secs_per_hour$bin)){ 
           for(j in 1:length(mfa_effort$Start_UTC)){
             if (son_secs_per_hour$Start_UTC[i] >= mfa_effort$Start_UTC[j] && son_secs_per_hour$Start_UTC[i] <=
                 mfa_effort$End_UTC[j] && isTRUE(is.na(son_secs_per_hour$secs[i]))){
                son_secs_per_hour$secs[i] = 0
              }
           }
          } # LOOP TAKES A FEW MINUTES
       
        #   # Unsuccessful attempts at more efficient loops to replace NAs w 0s for on-effort empty bins: 
        #   # Determine start and end bin of each on-effort period
        # on_off_eff_durs = as.data.frame(1:(length(mfa_effort$Start_UTC)+length(mfa_effort$Start_UTC)-1))
        # colnames(on_off_eff_durs) = "secs"
        # t = 2
        # w = 1
        # j = 1
        # for(i in 1:length(mfa_effort$Start_UTC)){
        #   on_off_eff_durs$secs[j] = as.duration(mfa_effort[w,2] - mfa_effort[w,1])
        #   if(w < 21){
        #     on_off_eff_durs$secs[j+1] = as.duration(mfa_effort[t,1] - mfa_effort[w,2])
        #     w = w+1
        #     t = t+1
        #     j = j+2
        #   }
        # }
        # 
        # on_off_eff_durs[on_off_eff_durs<0] = 0
        # on_off_eff_durs$mins = on_off_eff_durs$secs/60
        # on_off_eff_durs$hrs = on_off_eff_durs$secs/3600
        # 
        # strt_end_min = as.data.frame(1:(ceiling(length(on_off_eff_durs$mins))))
        # colnames(strt_end_min) = "Start_bin"
        # strt_end_min$End_bin[1] = ceiling(on_off_eff_durs$mins[1])
        # for(i in 2:length(on_off_eff_durs$mins)){
        #   strt_end_min$Start_bin[i]=strt_end_min$End_bin[i-1]+1
        #   strt_end_min$End_bin[i]=strt_end_min$End_bin[i-1]+ceiling(on_off_eff_durs$mins[i])
        # } #now have list of start/end bins of each on- and off-effort period; only need for on-effort
        # on_eff_ind = as.vector(seq(1,length(strt_end_min$Start_bin),2))
        # 
        #   # Replace NAs with zeros in on-effort empty bins:
        # # a = 1
        # # for(i in on_eff_ind){
        # #   # a = strt_end_min$Start_bin[i]
        # #   # b = strt_end_min$End_bin[i]
        # #   if(is.na(son_secs_per_min$secs[a:b])){
        # #     son_secs_per_min$secs[a:b]=0
        # #   }
        # #   a = a+2
        # # }
        # replace_na <- function(x,a,b) { mutate_all(x[a:b], funs(ifelse(is.na(.), 0, .))) }
        # 
        # for(i in on_eff_ind){
        #     a = strt_end_min$Start_bin[i]
        #     b = strt_end_min$End_bin[i]
        #     replace_na(son_secs_per_min$secs,a,b)
        #     # son_secs_per_min$secs[a:b] = mutate_at(c(5:10), funs(replace(., is.na(.), 0)))
        # }
        # 
        # for(i in on_eff_ind){
        #   a = strt_end_min$Start_bin[i]
        #   b = strt_end_min$End_bin[i]
        #   ind = as.vector(a:b)
        #   son_secs_per_min[ind] = replace(is.na(son_secs_per_min$secs[ind]),ind,0)
        # }
        # 
        # for(i in 1:length(on_eff_ind)){
        #   a = strt_end_min$Start_bin[i]
        #   b = strt_end_min$End_bin[i]
        #   for(j in a:b){
        #     if(isTRUE(is.na(son_secs_per_min$secs[j]))){
        #       son_secs_per_min$secs[j] = 0
        #     }
        #   }
        # }
        # 
      
        # Calculate proportion of time occupied by sonar in each small bin:
          #minbin_dur = as.duration("60 seconds")
          hrbin_dur = as.duration("3600 seconds")
          #son_secs_per_min$prop = son_secs_per_min$secs/as.numeric(minbin_dur)
          son_secs_per_hour$prop = son_secs_per_hour$secs/as.numeric(hrbin_dur)
          
        # Divide small bins among larger analysis bins:
          bigbin_dur = as.duration(bn)
          #min_brks = seq(0, (as.numeric(bigbin_dur,"minutes"))*(last(bin_mfa$bin)),(as.numeric(bigbin_dur,"minutes")))
          hr_brks = seq(0, (as.numeric(bigbin_dur,"hours"))*(last(bin_mfa$bin)),(as.numeric(bigbin_dur,"hours")))
          
          #son_secs_per_min$big_bin = cut(son_secs_per_min$bin, breaks=min_brks,labels=FALSE)
          son_secs_per_hour$big_bin = cut(son_secs_per_hour$bin, breaks=hr_brks,labels=FALSE)
          
        # Calculate standard dev of small bins within each analysis bin:
          #sd_sonprop_min = aggregate(x=son_secs_per_min$prop, by=list(bin=son_secs_per_min$big_bin), FUN=sd, na.rm=FALSE) 
          sd_sonprop_hr = aggregate(x=son_secs_per_hour$prop, by=list(bin=son_secs_per_hour$big_bin), FUN=sd, na.rm=FALSE) 
          # bin_mfa$StDv_sonprop_min = sd_sonprop_min$x
          bin_mfa$StDv_sonprop_hr = sd_sonprop_hr$x



  # Average Environmental Data: ---------------------------------------------
      bin_envir = as.data.frame(aggregate(x=envir[,2:length(envir[1,])], by=list(bin=envir$bin), FUN=mean))
      sd = as.vector(aggregate(x=envir$dates, by=list(bin=envir$bin), FUN=min))
      bin_envir$Start_UTC = as.POSIXct(sd[,2])
      
      bin_envir_1df = as.data.frame(aggregate(x=envir_1df[,2:length(envir[1,])], by=list(bin=envir_1df$bin), FUN=mean))
      sd = as.vector(aggregate(x=envir_1df$dates, by=list(bin=envir_1df$bin), FUN=min))
      bin_envir_1df$Start_UTC = as.POSIXct(sd[,2])
      
      
      # Add Julian Day variable to envir/envir_1df data:
      bin_envir$JD = as.numeric(strftime(bin_envir$Start_UTC, format = "%j"))
      bin_envir_1df$JD = as.numeric(strftime(bin_envir_1df$Start_UTC, format = "%j"))


  # Replace click & sonar NAs with zeros for on-effort empty bins: ------------------------
      # Click data:
      for (i in 1:length(bin_clicks$bin)){
        for(j in 1:length(click_effort$Start_UTC)){
          if (bin_clicks$Start_UTC[i] >= click_effort$Start_UTC[j] && bin_clicks$Start_UTC[i] <=
              click_effort$End_UTC[j] && isTRUE(is.na(bin_clicks$Count[i]))){
            bin_clicks$Count[i] = 0 
          }
          if (bin_clicks$Start_UTC[i] >= click_effort$End_UTC[j]  && 
              bin_clicks$Start_UTC[i] <=click_effort$Start_UTC[j+1]){      
            bin_clicks$Count[i] = as.numeric("NA")
          }
          if (bin_clicks$Start_UTC[i] >= click_effort$End_UTC[j] && j ==
              length(click_effort$End_UTC)){      
            bin_clicks$Count[i] = as.numeric("NA")
          }
        }
      } ## CHECK LAST DATA POINT TO MAKE SURE IT'S APPROPRIATE ZERO/NA
      
      # MFA data:
      for (i in 1:length(bin_mfa$bin)){
        for(j in 1:length(mfa_effort$Start_UTC)){
          if (bin_mfa$Start_UTC[i] >= mfa_effort$Start_UTC[j] && bin_mfa$Start_UTC[i] <=
              mfa_effort$End_UTC[j] && is.na(bin_mfa$Max_PPRL_dB[i])){
            bin_mfa[i,3:8] = 0
          }
          if (bin_mfa$Start_UTC[i] >= mfa_effort$End_UTC[j] && bin_mfa$Start_UTC[i] <=
              mfa_effort$Start_UTC[j+1]){      
            bin_mfa[i,3:8] = as.numeric("NA")
          }
        }
      } ## CHECK LAST DATA POINT TO MAKE SURE IT'S APPROPRIATE ZERO/NA
      
      # Add Sonar Presence variable to MFA data:
      bin_mfa$SPres = bin_mfa$Max_PPRL_dB
      bin_mfa$SPres[bin_mfa$SPres > 0] = 1

  
  # TODO: Normalize data in bins which were only partially on-effort:
  
  save(bin_clicks,bin_mfa, bin_envir, bin_envir_1df,file = paste("datvars_",
                        as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata",sep = ""))
  
  
  
# COMBINE DATA IN MASTER DATAFRAME --------------------------
load (paste("datvars_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata",sep = ""))

master = merge(bin_clicks,bin_mfa,by = "Start_UTC")
#master = merge(master,bin_envir,by = "Start_UTC")
master = merge(bin_envir_1df,master,by = "Start_UTC")

master = subset(master,select = -c(bin.x,bin.y,bin,Start_UTC))
master = rename(master,c("Count"="Clicks"))
master = master[,c(17,19:26,18,1:16)]
master$secs_per_bin = as.numeric(master$secs_per_bin)

save(master,file = paste("Master_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))

load (paste("Master_",as.numeric(duration(bn))/(60*60*24),"daybin_",site,".Rdata", sep = ""))



# # BIN DATA - 24 HRS --------------------------------------------------------------
# 
# # Assign bin numbers to data points:
# mfa$bin = cut(mfa$Start_UTC, breaks = '24 hour', labels = FALSE)
# clicks$bin = cut(clicks$Start_UTC, breaks = '24 hour', labels = FALSE)
# 
# # Sum clicks:
# day_clicks = aggregate(x=clicks$Counts, by=list(bin=clicks$bin), FUN=sum)
# day_clicks = rename(day_clicks, c("x" = "Count"))
# 
# # Max PPRL:
# day_mfa = aggregate(x=mfa$PPRL_dB, by=list(bin=mfa$bin), FUN=max)
# day_mfa = rename(day_mfa, c("x" = "Max_PPRL_dB"))
# 
# # Cumulative SEL:
# mfa$bels = mfa$SEL_dB/10
# mfa$intensity = 10^(mfa$bels)
# cum_intensity = aggregate(x=mfa$intensity, by=list(bin=mfa$bin), FUN=sum)
# day_mfa$CumSEL_dB = 10*log10(cum_intensity)[,2]
# 
# ### Insert empty bins: ###
# # To click data:
# sequence = as.data.frame(seq.int(1,max(day_clicks$bin),1)) # 24-hour bins
# colnames(sequence) <- "bin"
# sequence$Start_UTC = seq(as.POSIXlt("2009-01-14 00:00:00"),as.POSIXlt("2015-11-10 09:00:00.0"),by="24 hour")
# day_clicks = merge(sequence,day_clicks, by = "bin", all = TRUE)
# 
# # To MFA data:
# sequence = as.data.frame(seq.int(1,max(day_mfa$bin),1))
# colnames(sequence) <- "bin"
# sequence$Start_UTC = seq(as.POSIXlt("2009-01-14 00:00:00"),as.POSIXlt("2016-11-8 10:00:00.0"),by="24 hour")
# day_mfa = merge(sequence,day_mfa, by = "bin", all = TRUE)
# 
# # Replace NAs with zeros for on-effort empty bins:
# for (i in 1:length(day3_clicks$bin)){
#   for(j in 1:length(click_effort$Start_UTC)){
#     if (day_clicks$Start_UTC[i] >= click_effort$Start_UTC[j] && day_clicks$Start_UTC[i] <=
#         click_effort$End_UTC[j] && isTRUE(is.na(day_clicks$Count[i]))){
#       day_clicks$Count[i] = 0 
#     }
#     if (day_clicks$Start_UTC[i] >= click_effort$End_UTC[j] && day_clicks$Start_UTC[i] <=
#         click_effort$Start_UTC[j+1]){      
#       day_clicks$Count[i] = as.numeric("NA")
#     }
#   }
# }
# 
# for (i in 1:length(day_mfa$bin)){
#   for(j in 1:length(mfa_effort$Start_UTC)){
#     if (day_mfa$Start_UTC[i] >= click_effort$Start_UTC[j] && day_mfa$Start_UTC[i] <=
#         click_effort$End_UTC[j] && is.na(day_mfa$Max_PPRL_dB[i]) && is.na(day_mfa$CumSEL_dB[i])){
#       day_mfa$Max_PPRL_dB[i] = 0
#       day_mfa$CumSEL_dB[i] = 0
#     }
#     if (day_mfa$Start_UTC[i] >= click_effort$End_UTC[j] && day_mfa$Start_UTC[i] <=
#         click_effort$Start_UTC[j+1]){      
#       day_mfa$Max_PPRL_dB[i] = as.numeric("NA")
#       day_mfa$CumSEL_dB[i] = as.numeric("NA")
#     }
#   }
# }
# save(day_clicks,day_mfa,file = "bw_mfa_raw_day.Rdata")
# 
# load ("bw_mfa_raw_day.Rdata")

# # BIN DATA - 1 WEEK -------------------------------------------------------
# 
# # Assign bin numbers to data points:
# mfa$bin = cut(mfa$Start_UTC, breaks = '1 week', labels = FALSE)
# clicks$bin = cut(clicks$Start_UTC, breaks = '1 week', labels = FALSE)
# envir$bin = cut(envir$dates, breaks = '1 week', labels = FALSE)
# 
# # Sum clicks:
# week_clicks = aggregate(x=clicks$Counts, by=list(bin=clicks$bin), FUN=sum)
# week_clicks = rename(week_clicks, c("x" = "Count"))
# 
# # Max PPRL:
# week_mfa = aggregate(x=mfa$PPRL_dB, by=list(bin=mfa$bin), FUN=max)
# week_mfa = rename(week_mfa, c("x" = "Max_PPRL_dB"))
# 
# # Cumulative SEL:
# mfa$bels = mfa$SEL_dB/10
# mfa$intensity = 10^(mfa$bels)
# cum_intensity = aggregate(x=mfa$intensity, by=list(bin=mfa$bin), FUN=sum)
# week_mfa$CumSEL_dB = 10*log10(cum_intensity)[,2]
# 
# # Average environmental data:
# week_envir = aggregate(x=envir, by=list(bin=envir$bin), FUN=mean)
# week_envir = rename(week_envir, c("dates" = "Start_date"))
# 
# ### Insert empty bins: ###
# # To click data:
# sequence = as.data.frame(seq.int(1,max(week_clicks$bin)-1,1))
# colnames(sequence) <- "bin"
# sequence$Start_UTC = seq(as.POSIXlt("2009-01-14 00:00:00"),as.POSIXlt("2015-11-10 09:00:00.0"),by="1 week")
# week_clicks = merge(sequence,week_clicks, by = "bin", all = TRUE)
# 
# # To MFA data:
# sequence = as.data.frame(seq.int(1,max(week_mfa$bin)-1,1))
# colnames(sequence) <- "bin"
# sequence$Start_UTC = seq(as.POSIXlt("2009-01-14 00:00:00"),as.POSIXlt("2016-11-8 10:00:00.0"),by="1 week")
# week_mfa = merge(sequence,week_mfa, by = "bin", all = TRUE)
# 
# # Replace NAs with zeros for on-effort empty bins:
# for (i in 1:length(week_clicks$bin)){
#   for(j in 1:length(click_effort$Start_UTC)){
#     if (week_clicks$Start_UTC[i] >= click_effort$Start_UTC[j] && week_clicks$Start_UTC[i] <=
#         click_effort$End_UTC[j] && isTRUE(is.na(week_clicks$Count[i]))){
#       week_clicks$Count[i] = 0 
#     }
#   }
# }
# 
# for (i in 1:length(week_mfa$bin)){
#   for(j in 1:length(mfa_effort$Start_UTC)){
#     if (week_mfa$Start_UTC[i] >= mfa_effort$Start_UTC[j] && week_mfa$Start_UTC[i] <=
#         mfa_effort$End_UTC[j] && is.na(week_mfa$Max_PPRL_dB[i]) && is.na(week_mfa$CumSEL_dB[i])){
#       week_mfa$Max_PPRL_dB[i] = 0
#       week_mfa$CumSEL_dB[i] = 0
#     }
#   }
# }
# 
# save(week_clicks,week_mfa,file = "bw_mfa_raw_week.Rdata")
# 
# load ("bw_mfa_raw_week.Rdata")
# 
# 
# 
# 

# # CHASE’S BINNING FUNCTION ------------------------------------------------
# # 
# # hour_sonar <- binning_func(in_data = mfa, bin_size = "1 hour", vars = c(3:4))
# # hour_clicks <- binning_func(in_data = clicks, bin_size = "1 hour", vars = 2)
# # 
# # 
# # # Click binning function:
# # binning_click <- function(in_data = clicks, bin_size = "1 hour", vars = c(1:4)) {
# # 
# #   data <- in_data
# #   
# # # binning data:
# # 
# # data$bin <- cut(data$Start_UTC, breaks = bin_size, labels = FALSE)
# # 
# # VARS <- names(data)[vars]
# # if((length(vars)>1) == TRUE){
# # data[,vars] <- lapply(data[,vars], function(x) as.numeric(as.character(x)))
# # }
# # if((length(vars)>1) == FALSE){data[,vars] <- as.numeric(as.character(data[,vars]))}
# # 
# # bin_avg <- data %>%
# #   group_by(bin) %>%
# #   summarise(.vars = VARS, .funs = c(max ="max"))
# #   #summarise(.vars = VARS, .funs = c(sum ="cum"))
# #   #summarise(max_PPRL_dB = max(PPRL_dB),SEL_dB = sum(SEL_dB))
# # 
# # # fill in zeros:
# # 
# # sequence <- as.data.frame(seq(1:max(bin_avg$bin)))
# # colnames(sequence) <- "bin"
# # 
# # bin_avg_full <- merge(sequence,bin_avg, by = "bin", all = TRUE)
# # 
# # bin_avg_full[is.na(bin_avg_full)] = 0
# # 
# # return(bin_avg_full)
# # 
# # }
# 
# 
# 
# 
# 
# 
# 
# 
