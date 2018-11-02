##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)
library(zoo)
library(scales)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
##source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)

read.gcm.data <- function(gcm.list,scenario) {

  read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/',scenario,'/annual/')
  clip.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles','bc', stringsAsFactors=F)

  data <- matrix(NA,nrow=150,ncol=length(gcm.list))
       
  for (g in seq_along(gcm.list)) {
    gcm <- gcm.list[g]
    tasmax.files <- list.files(path=paste0(read.dir,gcm),pattern=paste0('tasmax_ann_'),full.name=TRUE)
    tasmax.past <- tasmax.files[grep('1951-2000',tasmax.files)]
    tasmax.proj <- tasmax.files[grep('2001-2100',tasmax.files)]

    tasmin.files <- list.files(path=paste0(read.dir,gcm),pattern=paste0('tasmin_ann_'),full.name=TRUE)
    tasmin.past <- tasmin.files[grep('1951-2000',tasmin.files)]
    tasmin.proj <- tasmin.files[grep('2001-2100',tasmin.files)]      

    tx.past <- gcm.netcdf.climatologies(tasmax.past,'tasmax',gcm,NULL,clip.shp)
    tx.proj <- gcm.netcdf.climatologies(tasmax.proj,'tasmax',gcm,NULL,clip.shp)

    tn.past <- gcm.netcdf.climatologies(tasmin.past,'tasmin',gcm,NULL,clip.shp)
    tn.proj <- gcm.netcdf.climatologies(tasmin.proj,'tasmin',gcm,NULL,clip.shp)

    past.series <- apply((tx.past+tn.past)/2,2,mean,na.rm=T)
    proj.series <- apply((tx.proj+tn.proj)/2,2,mean,na.rm=T)
    full.series <- c(past.series,proj.series)
    full.anoms <- full.series - mean(full.series[21:50])                
    if (length(full.anoms)==149) {
       full.anoms <- c(full.anoms,NA)
    }
    data[,g] <- full.anoms
    print(gcm)

  }

return(data)
}

##---------------------------------------------------------------------
##PCDS Data

obs.dir <- '/storage/data/projects/rci/data/assessments/bc/pcds/'

obs.tx <- read.csv(paste0(obs.dir,'annual_tx_anoms_tseries_0016_BRITISH COLUMBIA.csv'),header=TRUE,as.is=TRUE)
tx.anoms <- obs.tx[,3]
obs.tn <- read.csv(paste0(obs.dir,'annual_tn_anoms_tseries_0016_BRITISH COLUMBIA.csv'),header=TRUE,as.is=TRUE)
tn.anoms <- obs.tn[,3]
obs.anoms <- (tx.anoms + tn.anoms)/2
obs.years <- obs.tx[,1]
obs.anoms <- obs.anoms - mean(obs.anoms[72:101])

rcp26.list <- c('CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-ES','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp45.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp85.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

##rcp26.data <- read.gcm.data(rcp26.list,'rcp26')
##rcp45.data <- read.gcm.data(rcp45.list,'rcp45')
##rcp85.data <- read.gcm.data(rcp85.list,'rcp85')

##save(rcp26.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp26.gcm.RData')
##save(rcp45.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp45.gcm.RData')
##save(rcp85.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp85.gcm.RData')

load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp26.gcm.RData')
load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp45.gcm.RData')
load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp85.gcm.RData')
##browser()
rcp26.series <- apply(rcp26.data,1,mean,na.rm=T)
rcp45.series <- apply(rcp45.data,1,mean,na.rm=T)
rcp85.series <- apply(rcp85.data,1,mean,na.rm=T)

ix <- 52:140
px <- 1:52
rx <- 11
yrs <- 2007:2095
hys <- 1956:2007

rcp26.mean <- rollmean(rcp26.series,rx)
rcp45.mean <- rollmean(rcp45.series,rx)
rcp85.mean <- rollmean(rcp85.series,rx)

hist.90 <- rollmean(apply(rcp85.data,1,quantile,0.9,na.rm=T),rx)[px]
hist.10 <- rollmean(apply(rcp85.data,1,quantile,0.1,na.rm=T),rx)[px]

rcp26.90 <- rollmean(apply(rcp26.data,1,quantile,0.9),rx)[ix]
rcp26.10 <- rollmean(apply(rcp26.data,1,quantile,0.1),rx)[ix]

rcp45.90 <- rollmean(apply(rcp45.data,1,quantile,0.9,na.rm=T),rx)[ix]
rcp45.10 <- rollmean(apply(rcp45.data,1,quantile,0.1,na.rm=T),rx)[ix]
rcp85.90 <- rollmean(apply(rcp85.data,1,quantile,0.9,na.rm=T),rx)[ix]
rcp85.10 <- rollmean(apply(rcp85.data,1,quantile,0.1,na.rm=T),rx)[ix]

plot.dir <- '/storage/data/projects/rci/data/assessments/bc/'
plot.file <- paste0(plot.dir,'bc.annual.tas.lines.rcp85.png')

png(plot.file,width=1200,height=900)
par(mar=c(5,5,5,3))
plot(1951:2100,rcp26.series,type='l',lwd=4,col='white',ylim=c(-2.4,9),
main='Average Temperature Anomalies in BC',xlab='Year',ylab='Temperature Change (\u00B0C)',
cex.axis=2,cex.lab=2,cex.main=2.5)
##polygon(c(yrs,rev(yrs)),c(rcp85.10,rev(rcp85.90)),col=alpha('red',0.3),border=alpha('red',0.2))
##polygon(c(yrs,rev(yrs)),c(rcp45.10,rev(rcp45.90)),col=alpha('orange',0.3),border=alpha('orange',0.2))
##polygon(c(yrs,rev(yrs)),c(rcp26.10,rev(rcp26.90)),col=alpha('blue',0.3),border=alpha('blue',0.2))
##polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

apply(rcp85.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('red',0.5))},1951:2100)
##apply(rcp45.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('orange',0.3))},1951:2100)
##apply(rcp26.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('blue',0.3))},1951:2100)
##lines(1956:2095,rcp85.mean,lwd=4,col='red')
##lines(hys,rcp85.mean[px],lwd=4,col='darkgray')


##lines(c(1985,2010),rep(mean(rcp26.series[35:60]),2),col='black',lwd=6)

abline(h=seq(-2,8,2),col='gray',lty=3,lwd=2)

lines(obs.years,obs.anoms,lwd=4,col='black')

box(which='plot')
##legend('topleft',legend=c('PCDS','RCP8.5','RCP4.5','RCP2.6'),col=c('black','red','orange','blue'),cex=2,pch=15)
dev.off()

browser()

rcp26.mean <- rollmean(rcp26.series,11)
rcp45.mean <- rollmean(rcp45.series,11)
rcp85.mean <- rollmean(rcp85.series,11)


plot.file <- paste0(plot.dir,'nfld.annual.tas.smoothed.png')
##png(plot.file,width=900,height=900)
par(mar=c(5,5,5,3))
plot(1955:2095,rcp26.mean,type='l',lwd=4,col='green',ylim=c(0,15),
     main='NFLD Smoothed Annual Average Temperatures',xlab='Year',ylab='TAS (degC)',
     cex.axis=2,cex.lab=2,cex.main=2.5)
lines(1955:2095,rcp45.mean,lwd=4,col='orange')
lines(1955:2094,rcp85.mean,lwd=4,col='red')
abline(h=seq(0,20,5),col='gray',lty=3,lwd=3)
legend('topleft',legend=c('RCP8.5','RCP4.5','RCP2.6'),col=c('red','orange','green'),cex=2,pch=15)
box(which='plot')
##dev.off()
