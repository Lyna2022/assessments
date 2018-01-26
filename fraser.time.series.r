##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)
library(zoo)
library(scales)


source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)

read.gcm.data <- function(gcm.list,scenario) {

  read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/rcp85/climdex/')
  clip.shp <- readOGR('/storage/data/projects/rci/data/assessments/fraser_district/shapefiles','fraser_district', stringsAsFactors=F)

  data <- matrix(NA,nrow=1800,ncol=length(gcm.list))
  ##data <- vector(mode='list',length=length(gcm.list))     
  dates <- vector(mode='list',length(rcp85.list))

  for (g in seq_along(gcm.list)) {
    gcm <- gcm.list[g]
##    tasmax.files <- list.files(path=paste0(read.dir,gcm),pattern=paste0('tasmax_ann_'),full.name=TRUE)
##    tasmax.past <- tasmax.files[grep('1951-2000',tasmax.files)]
##    tasmax.proj <- tasmax.files[grep('2001-2100',tasmax.files)]

##    tasmin.files <- list.files(path=paste0(read.dir,gcm),pattern=paste0('tasmin_ann_'),full.name=TRUE)
##    tasmin.past <- tasmin.files[grep('1951-2000',tasmin.files)]
##    tasmin.proj <- tasmin.files[grep('2001-2100',tasmin.files)]      

    pr.files <- list.files(path=paste0(read.dir,gcm),pattern=paste0('rx5dayETCCDI'),full.name=TRUE)
    pr.past <- pr.files[grep('1951-2100',pr.files)]
    print(pr.past)
    ##pr.proj <- pr.files[grep('2001-2100',pr.files)]      

##    pnc <- nc_open(pr.past)
##    past.dates <- netcdf.calendar(pnc)
    ##fnc <- nc_open(pr.proj)
    ##proj.dates <- netcdf.calendar(fnc)
##    time.series <- c(past.dates) ##,proj.dates)
##    dates[[g]] <- time.series
##    nc_close(pnc)
    ##nc_close(fnc)

    if (1==1) {
      pr.past.data <- gcm.netcdf.climatologies(pr.past,'rx5dayETCCDI',gcm,NULL,clip.shp)
##      pr.proj.data <- gcm.netcdf.climatologies(pr.proj,'pr',gcm,NULL,clip.shp)

      past.series <- apply(pr.past.data,2,mean,na.rm=T)
##      proj.series <- apply(pr.proj.data,2,mean,na.rm=T)
      full.series <- c(past.series) ##,proj.series)
      ##full.anoms <- full.series - mean(full.series[21:50])                
      if (length(full.series)==1788) {
         ##full.anoms <- c(full.anoms,NA)
         full.series <- c(full.series,rep(NA,12))
      }
      data[,g] <- full.series
    }  
    print(gcm)

  }
  rv <- data ##list(data=data,dates=dates)
  return(rv)
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
rcp85.data <- read.gcm.data(rcp85.list,'rcp85')
browser()


##save(rcp26.data,file='/storage/data/projects/rci/data/assessments/fraser_district/pcds/data_files/rcp26.gcm.RData')
##save(rcp45.data,file='/storage/data/projects/rci/data/assessments/fraser_district/pcds/data_files/rcp45.gcm.RData')
##save(rcp85.data,file='/storage/data/projects/rci/data/assessments/fraser_district/pcds/data_files/rcp85.gcm.RData')

##load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp26.gcm.RData')
##load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/rcp45.gcm.RData')
##load('/storage/data/projects/rci/data/assessments/fraser_district/pcds/data_files/rcp85.gcm.RData')

##rcp26.series <- apply(rcp26.data,1,mean,na.rm=T)
##rcp45.series <- apply(rcp45.data,1,mean,na.rm=T)
rcp85.series <- apply(rcp85.data,1,mean,na.rm=T)

yrs <- 1:1800
months <- seq(from=as.Date('1951-01-01'),by='month',to=as.Date('2100-12-31'))

hist.90 <- apply(rcp85.data,1,quantile,0.9,na.rm=T)
hist.10 <- apply(rcp85.data,1,quantile,0.1,na.rm=T)

plot.dir <- '/storage/data/projects/rci/data/assessments/fraser_district/'
plot.file <- paste0(plot.dir,'fraser_district.rx5day.rcp85.195-2070.png')

png(plot.file,width=1200,height=700)
par(mar=c(5,5,5,3))
sb <- 1:1800 ##c(241:1440)
plot(months[sb],rcp85.series[sb],type='l',lwd=4,col='white',ylim=c(0,200),
main='Monthly RX5Day for Fraser District',xlab='Year',ylab='5-Day Precipitation (mm)',
cex.axis=2,cex.lab=2,cex.main=2.5)
polygon(c(months[sb],rev(months[sb])),c(hist.10[sb],rev(hist.90[sb])),col=alpha('blue',0.3),border=alpha('blue',0.2))
points(months[sb],rcp85.series[sb],cex=3,col='blue',pch='-')
abline(h=c(0,50,100,200),col='gray',lty=3,lwd=2)
box(which='plot')
dev.off()

