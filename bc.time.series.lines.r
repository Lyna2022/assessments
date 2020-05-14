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

obs.dir <- '/storage/data/projects/rci/data/assessments/bc/pcds/annual/'

obs.tx <- read.csv(paste0(obs.dir,'annual_tx_anoms_tseries_0017_BRITISH COLUMBIA.csv'),header=TRUE,as.is=TRUE)
tx.anoms <- obs.tx[,3]
obs.tn <- read.csv(paste0(obs.dir,'annual_tn_anoms_tseries_0017_BRITISH COLUMBIA.csv'),header=TRUE,as.is=TRUE)
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

if (1==0) {
plot.file <- paste0(plot.dir,'bc.annual.tas.agu.rcp85.png')

png(plot.file,width=1000,height=600)
par(mar=c(5,5,5,3))
plot(1951:2100,rcp26.series,type='l',lwd=4,col='white',xlim=c(1950,2100),ylim=c(-2.4,9),xaxs='i',
main='Average Temperature Anomalies in BC',xlab='Year',ylab='Temperature Change (\u00B0C)',
cex.axis=2,cex.lab=2,cex.main=2.5)
polygon(c(yrs,rev(yrs)),c(rcp85.10,rev(rcp85.90)),col=alpha('red',0.3),border=alpha('red',0.2))
polygon(c(yrs,rev(yrs)),c(rcp45.10,rev(rcp45.90)),col=alpha('orange',0.3),border=alpha('orange',0.2))
polygon(c(yrs,rev(yrs)),c(rcp26.10,rev(rcp26.90)),col=alpha('blue',0.3),border=alpha('blue',0.2))
polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

##apply(rcp85.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('red',0.5))},1951:2100)
##apply(rcp45.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('orange',0.3))},1951:2100)
##apply(rcp26.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('blue',0.3))},1951:2100)

lines(yrs,rcp85.mean[ix],lwd=4,col='red')
lines(yrs,rcp45.mean[ix],lwd=4,col='orange')
lines(yrs,rcp26.mean[ix],lwd=4,col='blue')
lines(hys,rcp85.mean[px],lwd=4,col='darkgray')

##lines(c(1985,2010),rep(mean(rcp26.series[35:60]),2),col='black',lwd=6)

abline(h=seq(-2,8,2),col='gray',lty=3,lwd=2)

lines(obs.years,obs.anoms,lwd=4,col='black')

lines(c(1961,2000),rep(quantile(rcp26.data[11:50,],0.9),2),col='darkgreen',lty=2,lwd=6)
lines(c(1961,2000),rep(quantile(rcp85.data[11:50,],0.1),2),col='darkgreen',lty=2,lwd=6)
lines(c(1961,1961),c(quantile(rcp85.data[11:50,],0.1),quantile(rcp26.data[11:50,],0.9)),col='darkgreen',lty=2,lwd=6)
lines(c(2000,2000),c(quantile(rcp85.data[11:50,],0.1),quantile(rcp26.data[11:50,],0.9)),col='darkgreen',lty=2,lwd=6)
text(1965,1.75,'Building Code\nParameters',col='darkgreen',cex=2)

lines(c(2001,2020),rep(quantile(rcp85.data[51:70,],0.9),2),col='darkgreen',lty=2,lwd=6)
lines(c(2001,2020),rep(quantile(rcp85.data[51:70,],0.1),2),col='darkgreen',lty=2,lwd=6)
lines(c(2001,2001),c(quantile(rcp85.data[51:70,],0.1),quantile(rcp85.data[51:70,],0.9)),col='darkgreen',lty=2,lwd=6)
lines(c(2020,2020),c(quantile(rcp85.data[51:70,],0.1),quantile(rcp85.data[51:70,],0.9)),col='darkgreen',lty=2,lwd=6)
text(2029,-0.9,'Current Design Parameters',col='darkgreen',cex=2)

lines(c(2021,2070),rep(quantile(rcp85.data[71:120,],0.9),2),col='darkgreen',lty=2,lwd=6)
lines(c(2021,2070),rep(quantile(rcp45.data[71:120,],0.1),2),col='darkgreen',lty=2,lwd=6)
lines(c(2021,2021),c(quantile(rcp45.data[71:120,],0.1),quantile(rcp85.data[71:120,],0.9)),col='darkgreen',lty=2,lwd=6)
lines(c(2070,2070),c(quantile(rcp45.data[71:120,],0.1),quantile(rcp85.data[71:120,],0.9)),col='darkgreen',lty=2,lwd=6)
text(2030,4.9,'Expanded Design Parameters',col='darkgreen',cex=2)



box(which='plot')
legend('topleft',legend=c('Historial Observations (PCDS)',
                          '3.5\u00B0C Business as usual (RCP8.5)',
                          '2.0\u00B0C Paris Global Limit (RCP4.5)',
                          '1.5\u00B0C Aspirational Global Limit (RCP2.6)'),
                          col=c('black','red','orange','blue'),cex=2,pch=15)
dev.off()

}

##plot.file <- paste0(plot.dir,'nfld.annual.tas.smoothed.png')
##png(plot.file,width=900,height=900)
par(mar=c(5,5,5,3))
plot(1956:2095,rcp26.mean,type='l',lwd=4,col='green',ylim=c(0,10),
     main='Annual Average Temperatures',xlab='Year',ylab='TAS (degC)',
     cex.axis=2,cex.lab=2,cex.main=2.5,xlim=c(1950,2110))
apply(rcp85.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('red',0.5))},1951:2100)
apply(rcp45.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('orange',0.3))},1951:2100)
apply(rcp26.data,2,function(y,x){lines(x,y,lwd=1,col=alpha('blue',0.3))},1951:2100)

lines(1951:2100,rcp26.series,lwd=4,col='blue')
lines(1951:2100,rcp45.series,lwd=4,col='orange')
lines(1951:2100,rcp85.series,lwd=4,col='red')
abline(h=seq(0,20,5),col='gray',lty=3,lwd=3)
legend('topleft',legend=c('RCP8.5','RCP4.5','RCP2.6'),col=c('red','orange','green'),cex=2,pch=15)
box(which='plot')
##dev.off()

years <- 2081:2100
tas <- rcp26.series[131:150]
rcp.ext <- data.frame(years=years,tas=tas)
rcp.fit <- lm(tas~years,rcp.ext)
x <- 2081:2110
y.26 <- rcp.fit$coefficients[2]*x + rcp.fit$coefficients[1]
lines(x,y.26,col='blue')

years <- 2081:2100
tas <- rcp45.series[131:150]
rcp.ext <- data.frame(years=years,tas=tas)
rcp.fit <- lm(tas~years,rcp.ext)
x <- 2081:2110
y.45 <- rcp.fit$coefficients[2]*x + rcp.fit$coefficients[1]
lines(x,y.45,col='orange')

years <- 2081:2100
tas <- rcp85.series[131:150]
rcp.ext <- data.frame(years=years,tas=tas)
rcp.fit <- lm(tas~years,rcp.ext)
x <- 2081:2110
y.85 <- rcp.fit$coefficients[2]*x + rcp.fit$coefficients[1]
lines(x,y.85,col='red')

ts <- 1951:2100

st.10s <- grep(1986,ts)
en.10s <- grep(2016,ts)

st.20s <- grep(2011,ts)
en.20s <- grep(2040,ts)

st.50s <- grep(2041,ts)
en.50s <- grep(2070,ts)

st.80s <- grep(2071,ts)
en.80s <- grep(2100,ts)

col.names <- c('RCPs','2010s','2020s','2050s','2080s','2100')
row.names <- c('RCP2.6','RCP4.5','RCP8.5')

anoms.26 <- c(mean(rcp26.series[st.10s:en.10s]),
              mean(rcp26.series[st.20s:en.20s]),
              mean(rcp26.series[st.50s:en.50s]),
              mean(rcp26.series[st.80s:en.80s]),
              mean(y.26[11:30]))

anoms.45 <- c(mean(rcp45.series[st.10s:en.10s]),
              mean(rcp45.series[st.20s:en.20s]),
              mean(rcp45.series[st.50s:en.50s]),
              mean(rcp45.series[st.80s:en.80s]),
              mean(y.45[11:30]))
              
anoms.85 <- c(mean(rcp85.series[st.10s:en.10s]),
              mean(rcp85.series[st.20s:en.20s]),
              mean(rcp85.series[st.50s:en.50s]),
              mean(rcp85.series[st.80s:en.80s]),
              mean(y.85[11:30]))


bc.anoms <- cbind(row.names,round(rbind(anoms.26,anoms.45,anoms.85),1))
colnames(bc.anoms) <- col.names

write.table(bc.anoms,file=paste0(plot.dir,'BC.gcm.bccaq2.tas.anomalies.csv'),quote=F,sep=',',col.names=T,row.names=F)
