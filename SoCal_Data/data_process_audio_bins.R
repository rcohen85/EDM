# Format and bin MFA, BW click, effort, and evironmental data into master time series matrix for
# use in Simplex, S-Maps, and CCM analysis. Assumes environmental data is 1-day resolution.

setwd("~/Home/School/EDM/class_proj/SoCal_Data")

library(dplyr)
library(tidyr)
library(lubridate)
library(DataCombine)
library(plyr)
library(readtext)

# Monitoring site and desired start/end dates of master matrix; site name will be used 
# to label saved files:
site = "E"
stdt = "2006-09-03"
enddt = "2009-09-17"

# Desired bin length:
bn = "1 hour" ### USE APPROPRIATE BINNING SCRIPT BELOW FOR BINS OF DAYS VS HOURS ###


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
stp = as.POSIXct(enddt) - (60*60*24)
envir = subset(envir, dates >= strt)
envir = subset(envir, dates <= stp)

envir[is.na(envir)==TRUE] = as.numeric("NA")
envir[,2:ncol(envir)] = as.numeric(envir[,2:ncol(envir)])

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
      bin_mfa$secs_per_bin = as.numeric(secs_per_bin$x)
      son_prop = as.numeric(bin_mfa$secs_per_bin/as.numeric(duration(bn)))
      bin_mfa$son_prop = son_prop
      
      # Standard deviation of SEL:
      sd_SEL = aggregate(x=mfa$SEL_dB, by=list(bin=mfa$bin), FUN=sd) 
      sd_SEL = merge(sequence, sd_SEL, by.x = "bin", by.y = "bin", all.x = TRUE)
      bin_mfa$StDvSEL = sd_SEL$x
      
      # Standard deviation of Sonar proportion: variability of sonar prop between smaller bins within each analysis bin
        # **Relevant time period over which to look at variability? e.g. compare minutes vs compare hours**
      
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
        sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt)-1,by="1 hour")
        son_secs_per_hour = merge(sequence,son_secs_per_hour, by = "bin", all = TRUE)
      
       # Replace NAs with zeros for on-effort empty bins:
        # for (i in 1:length(mfa_effort$Start_UTC)){
        #   a = which(son_secs_per_min$Start_UTC>= mfa_effort$Start_UTC[i])
        #   b = which(son_secs_per_min$Start_UTC<= mfa_effort$End_UTC[i])
        #   ind = intersect(a,b)
        #   need_zero = ind[which(is.na(son_secs_per_min$secs[ind]))]
        #   son_secs_per_min[need_zero,3] = 0
        # }
        
        for (i in 1:length(mfa_effort$Start_UTC)){
          a = which(son_secs_per_hour$Start_UTC>= mfa_effort$Start_UTC[i])
          b = which(son_secs_per_hour$Start_UTC<= mfa_effort$End_UTC[i])
          ind = intersect(a,b)
          need_zero = ind[which(is.na(son_secs_per_hour$secs[ind]))]
          son_secs_per_hour[need_zero,3] = 0
        }
      
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
      for (i in 1:length(click_effort$Start_UTC)){
        a = which(bin_clicks$Start_UTC>= click_effort$Start_UTC[i])
        b = which(bin_clicks$Start_UTC<= click_effort$End_UTC[i])
        ind_a = intersect(a,b)
        need_zero = ind_a[which(is.na(bin_clicks$Count[ind_a]))]
        bin_clicks[need_zero,3] = 0
        
        c = which(bin_clicks$Start_UTC >= click_effort$End_UTC[i])
        d = which(bin_clicks$Start_UTC <= click_effort$Start_UTC[i+1])
        ind_b = intersect(c,d)
        need_na = ind_b[which(bin_clicks$Count[ind_b]==0)]
        bin_clicks[need_na,3] = as.numeric("NA")
      }
      
      # MFA data:
      for (i in 1:length(mfa_effort$Start_UTC)){
        a = which(bin_mfa$Start_UTC>= mfa_effort$Start_UTC[i])
        b = which(bin_mfa$Start_UTC<= mfa_effort$End_UTC[i])
        ind_a = intersect(a,b)
        need_zero = ind_a[which(is.na(bin_mfa$Max_PPRL_dB[ind_a]))]
        bin_mfa[need_zero,3:7] = 0
        
        c = which(bin_mfa$Start_UTC >= mfa_effort$End_UTC[i])
        d = which(bin_mfa$Start_UTC <= mfa_effort$Start_UTC[i+1])
        ind_b = intersect(c,d)
        need_na = ind_b[which(bin_mfa$Max_PPRL_dB[ind_b]==0)]
        bin_mfa[need_na,3:7] = as.numeric("NA")
      }
      # Add Sonar Presence variable to MFA data:
      bin_mfa$SPres = bin_mfa$Max_PPRL_dB
      bin_mfa$SPres[bin_mfa$SPres > 0] = 1
  
  
# BIN DATA - N HOURS --------------------------------
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
bin_mfa$secs_per_bin = as.numeric(secs_per_bin$x)
son_prop = as.numeric(bin_mfa$secs_per_bin/as.numeric(duration(bn)))#colnames(son_prop) = ""
bin_mfa$son_prop = son_prop

# Standard deviation of SEL:
sd_SEL = aggregate(x=mfa$SEL_dB, by=list(bin=mfa$bin), FUN=sd) 
sd_SEL = merge(sequence, sd_SEL, by.x = "bin", by.y = "bin", all.x = TRUE)
bin_mfa$StDvSEL = sd_SEL$x

# Standard deviation of Sonar proportion: variability of sonar prop between smaller bins within each analysis bin
  #Cut sonar data into smaller bins:
  mfa$bin_min = cut(mfa$Start_UTC, breaks = "1 min", labels = FALSE)

  son_secs_per_min = aggregate(x=mfa$dur, by=list(mfa$bin_min), FUN=sum) # THIS MAY TAKE A WHILE

  names(son_secs_per_min)[1] = "bin"
  names(son_secs_per_min)[2] = "secs"

  son_secs_per_min$secs = as.numeric(son_secs_per_min$secs)

  # Add sonar-free bins:
  sequence = as.data.frame(seq.int(1,max(son_secs_per_min$bin),1))
  colnames(sequence) <- "bin"
  sequence$Start_UTC = seq(as.POSIXlt(stdt),as.POSIXlt(enddt) - 1,by="1 min")
  son_secs_per_min = merge(sequence,son_secs_per_min, by = "bin", all = TRUE)

  # Replace NAs with zeros for on-effort empty bins:

  for (i in 1:length(mfa_effort$Start_UTC)){
    a = which(son_secs_per_min$Start_UTC>= mfa_effort$Start_UTC[i])
    b = which(son_secs_per_min$Start_UTC<= mfa_effort$End_UTC[i])
    ind = intersect(a,b)
    need_zero = ind[which(is.na(son_secs_per_min$secs[ind]))]
    son_secs_per_min[need_zero,3] = 0
  }
   
  # Calculate proportion of time occupied by sonar in each small bin:
  minbin_dur = as.duration("60 seconds")
  son_secs_per_min$prop = son_secs_per_min$secs/as.numeric(minbin_dur)
 
  # Divide small bins among larger analysis bins:
  bigbin_dur = as.duration(bn)
  min_brks = seq(0, (as.numeric(bigbin_dur,"minutes"))*(last(bin_mfa$bin)),(as.numeric(bigbin_dur,"minutes")))
 
  son_secs_per_min$big_bin = cut(son_secs_per_min$bin, breaks=min_brks,labels=FALSE)
 
  # Calculate standard dev of small bins within each analysis bin:
  sd_sonprop_min = aggregate(x=son_secs_per_min$prop, by=list(bin=son_secs_per_min$big_bin), FUN=sd, na.rm=FALSE)
  bin_mfa$StDv_sonprop_min = sd_sonprop_min$x
 
  # Repeat Environmental Data: ---------------------------------------------
  rep_num = 24/(as.numeric(duration(bn))/(60*60))
  bin_envir = data.table(envir)[rep(c(1:(length(envir[,1]))-1),each=rep_num)]
  bin_envir = rbind(bin_envir,last(envir))
  bin_envir_1df = data.table(envir_1df)[rep(c(1:(length(envir_1df[,1]))-1),each=rep_num)]
  bin_envir_1df = rbind(bin_envir_1df,last(envir_1df))
  
  # Add higher-resolution time/date data:
  bin_envir$dates = seq(as.POSIXlt(stdt),as.POSIXlt(enddt)-(24*60*60),by=bn)
  bin_envir_1df$dates = seq(as.POSIXlt(stdt),as.POSIXlt(enddt)-(24*60*60),by=bn)
  
  # Fix bin #s:
  bin_envir$bin = seq(1,nrow(bin_envir),by=1)
  bin_envir_1df$bin = seq(1,nrow(bin_envir_1df),by=1)
 
  # Add Julian Day variable to envir/envir_1df data:
  dt = data.table(seq(as.Date(stdt),as.Date(enddt),by="1 day"),format="%Y-%m-%d")
  dt = dt[rep(c(1:(nrow(dt)-1)),each=rep_num)]
  dt = dt[c(1:(nrow(dt)-23)),1]
  dt = as.data.frame(dt)
  bin_envir$JD = yday(dt[,1])
  bin_envir_1df$JD = yday(dt[,1])
 
  # Replace click & sonar NAs with zeros for on-effort empty bins: ------------------------
# Click data:
 for (i in 1:length(click_effort$Start_UTC)){
   a = which(bin_clicks$Start_UTC>= click_effort$Start_UTC[i])
   b = which(bin_clicks$Start_UTC<= click_effort$End_UTC[i])
   ind_a = intersect(a,b)
   need_zero = ind_a[which(is.na(bin_clicks$Count[ind_a]))]
   bin_clicks[need_zero,3] = 0
   
   c = which(bin_clicks$Start_UTC >= click_effort$End_UTC[i])
   d = which(bin_clicks$Start_UTC <= click_effort$Start_UTC[i+1])
   ind_b = intersect(c,d)
   need_na = ind_b[which(bin_clicks$Count[ind_b]==0)]
   bin_clicks[need_na,3] = as.numeric("NA")
 }
 
# MFA data:
for (i in 1:length(mfa_effort$Start_UTC)){
  a = which(bin_mfa$Start_UTC>= mfa_effort$Start_UTC[i])
  b = which(bin_mfa$Start_UTC<= mfa_effort$End_UTC[i])
  ind_a = intersect(a,b)
  need_zero = ind_a[which(is.na(bin_mfa$Max_PPRL_dB[ind_a]))]
  bin_mfa[need_zero,3:7] = 0
  
  c = which(bin_mfa$Start_UTC >= mfa_effort$End_UTC[i])
  d = which(bin_mfa$Start_UTC <= mfa_effort$Start_UTC[i+1])
  ind_b = intersect(c,d)
  need_na = ind_b[which(bin_mfa$Max_PPRL_dB[ind_b]==0)]
  bin_mfa[need_na,3:7] = as.numeric("NA")
}

# Add Sonar Presence variable to MFA data:
bin_mfa$SPres = bin_mfa$Max_PPRL_dB
bin_mfa$SPres[bin_mfa$SPres > 0] = 1


# TODO: Normalize data in bins which were only partially on-effort:

# SAVE INDIVIDUAL VARIABLES ----------------
save(bin_clicks,bin_mfa, bin_envir, bin_envir_1df,file = gsub(" ","_", paste("datvars_",
                                              bn,"_bin_",site,".Rdata",sep = ""),fixed=TRUE))


# COMBINE DATA IN MASTER DATAFRAME --------------------------
load (gsub(" ","_", paste("datvars_",bn,"_bin_",site,".Rdata",sep = ""),fixed=TRUE))

master = merge(bin_clicks,bin_mfa,by = "Start_UTC")
#master = merge(master,bin_envir,by = "Start_UTC")
master = merge(bin_envir_1df,master,by = "Start_UTC")

master = subset(master,select = -c(bin.x,bin.y,bin,Start_UTC))
master = rename(master,c("Count"="Clicks"))
master = master[,c(17,19:26,18,1:16)]
master$secs_per_bin = as.numeric(master$secs_per_bin)

save(master,file = gsub(" ","_",paste("Master_",bn,"_bin_",site,".Rdata", sep = "")))

load (gsub(" ","_",paste("Master_",bn,"_bin_",site,".Rdata", sep = "")))












