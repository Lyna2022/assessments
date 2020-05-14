##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)
library(zoo)
library(scales)
library(raster)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)

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

regions <- c('ALBERNI-CLAYOQUOT','CAPITAL','COMOX VALLEY','COWICHAN VALLEY',
             'MOUNT WADDINGTON','NANAIMO','STRATHCONA')

regions <- 'BRITISH COLUMBIA'

if (1==0) {

rd.file <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/RD_2011.shp'
rd.shp <- shapefile(rd.file)
rd.names <- rd.shp[[2]]
tx.dir <- '/storage/data/projects/crmp/bc_trends_indicators/annual_tx_anoms/'
tn.dir <- '/storage/data/projects/crmp/bc_trends_indicators/annual_tn_anoms/'

areas <- rep(NA,length(regions))

for (i in seq_along(regions)) {
   rd.ix <- which(toupper(rd.names) %in% regions[i])
   district <- rd.shp[rd.ix,]
   areas[i] <- area(district)/10E6
}
total.area <- sum(areas)
weights <- areas/total.area
obs.anoms <- c()
for (i in seq_along(regions)) {
   obs.tx <- read.csv(paste0(tx.dir,'annual_tx_anoms_tseries_0018_',regions[i],'.csv'),header=TRUE,as.is=TRUE)
   obs.years <- obs.tx[,1]
   tx.anoms <- obs.tx[,3]
   obs.tn <- read.csv(paste0(tn.dir,'annual_tn_anoms_tseries_0018_',regions[i],'.csv'),header=TRUE,as.is=TRUE)
   tn.anoms <- obs.tn[,3]
   rd.anoms <- (tx.anoms + tn.anoms)/2
   rd.anoms <- rd.anoms - mean(rd.anoms[72:101])
   obs.anoms <- rbind(obs.anoms,weights[i]*rd.anoms)
}
obs.anoms <- apply(obs.anoms,2,sum)
}
##

if (1==1) {
 obs.dir <- '/storage/data/projects/rci/data/assessments/bc/pcds/annual/'
 obs.tx <- read.csv(paste0(obs.dir,'annual_tx_anoms_tseries_0018_BRITISH\ COLUMBIA.csv'),header=TRUE,as.is=TRUE)
 tx.anoms <- obs.tx[,3]
 obs.tn <- read.csv(paste0(obs.dir,'annual_tn_anoms_tseries_0018_BRITISH\ COLUMBIA.csv'),header=TRUE,as.is=TRUE)
 tn.anoms <- obs.tn[,3]
 obs.anoms <- (tx.anoms + tn.anoms)/2
 obs.years <- obs.tx[,1]
 obs.anoms <- obs.anoms - mean(obs.anoms[72:101])
}

if (1==0) {
rcp26.list <- c('CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-ES','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp45.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp85.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

rcp26.data <- read.gcm.data(rcp26.list,'rcp26')
rcp45.data <- read.gcm.data(rcp45.list,'rcp45')
rcp85.data <- read.gcm.data(rcp85.list,'rcp85')

save(rcp26.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp26.gcm.RData')
save(rcp45.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp45.gcm.RData')
save(rcp85.data,file='/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp85.gcm.RData')
}

load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp26.gcm.RData')
load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp45.gcm.RData')
load('/storage/data/projects/rci/data/assessments/bc/pcds/data_files/bc.rcp85.gcm.RData')

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
plot.file <- paste0(plot.dir,'bc.infographic.pdf') ##annual.tas.2018.png')
##plot.file <- paste0(plot.dir,'capital.region.annual.tas.2018.png')

##png(plot.file,width=1200,height=900)
##png(file=plot.file,width=10,height=6,units='in',res=600,pointsize=6,bg='white')
pdf(file=plot.file,width=10,height=6,bg='white')
##pdf(file=plot.file,width=8,height=6,bg='white')

par(mar=c(5,5,5,3))
plot(1951:2100,rcp26.series,type='l',lwd=4,col='white',ylim=c(-2.2,7.1),xlim=c(1950,2170),
main='Average Temperature Anomalies in the British Columbia',xlab='Year',ylab='Temperature Change (\u00B0C)',
cex.axis=1.0,cex.lab=1.0,cex.main=1.25,xaxs='i')
###cex.axis=1.75,cex.lab=1.75,cex.main=2.5,xaxs='i')
##polygon(c(yrs,rev(yrs)),c(rcp85.10,rev(rcp85.90)),col=alpha('red',0.3),border=alpha('red',0.2))
##polygon(c(yrs,rev(yrs)),c(rcp45.10,rev(rcp45.90)),col=alpha('orange',0.3),border=alpha('orange',0.2))
##polygon(c(yrs,rev(yrs)),c(rcp26.10,rev(rcp26.90)),col=alpha('blue',0.3),border=alpha('blue',0.2))
##polygon(c(hys,rev(hys)),c(hist.10,rev(hist.90)),col=alpha('gray',0.3),border=alpha('gray',0.5))

yrs <- 2021:2095
ix <- 66:140
lines(yrs,rcp85.mean[ix],lwd=2,col='red')
lines(yrs,rcp45.mean[ix],lwd=2,col='orange')
lines(yrs,rcp26.mean[ix],lwd=2,col='blue')
lines(1956:2020,apply(rbind(rcp85.mean,rcp45.mean,rcp26.mean),2,mean)[1:65],lwd=2,col='darkgray')
lines(c(1971,2000),rep(mean(rcp26.series[21:50]),2),col='black',lwd=3)

###lines(hys,rcp85.mean[px],lwd=2,col='darkgray')
px <- 1.25

points(x=2020,y=1.2,col='black',pch=16,cex=px)
#points(x=2025,y=mean(rcp26.series[61:90]),col='blue',pch=16,cex=px)
points(x=2055,y=mean(rcp26.series[91:120]),col='blue',pch=16,cex=px)
points(x=2085,y=mean(rcp26.series[121:150]),col='blue',pch=16,cex=px)
points(x=2100,y=1.9,col='blue',pch=16,cex=px)

#points(x=2025,y=mean(rcp45.series[61:90]),col='orange',pch=16,cex=px)
points(x=2055,y=mean(rcp45.series[91:120]),col='orange',pch=16,cex=px)
points(x=2085,y=mean(rcp45.series[121:150]),col='orange',pch=16,cex=px)
points(x=2100,y=3.2,col='orange',pch=16,cex=px)

#points(x=2025,y=mean(rcp85.series[61:90]),col='red',pch=16,cex=px)
points(x=2055,y=mean(rcp85.series[91:120]),col='red',pch=16,cex=px)
points(x=2085,y=mean(rcp85.series[121:150]),col='red',pch=16,cex=px)
points(x=2100,y=6.1,col='red',pch=16,cex=px)

abline(v=seq(1950,2100,length.out=11),lty=3,col='gray')
abline(h=seq(par('usr')[3],par('usr')[4],length.out=11),lty=3,col='gray')

##abline(h=seq(-2,8,2),col='gray',lty=3,lwd=1)
##abline(v=seq(1950,2100,20),col='gray',lty=3,lwd=1)

rect(2110,par('usr')[3],2190,par('usr')[4],col='white')
abline(v=seq(2129,2149,20))
abline(v=seq(2121,2181,20))

tx <- 0.9
text(2115.5,1.2,'2020',col='black',cex=tx)
text(2125,1.2,'1.2',col='black',cex=tx)

text(2115.5,2.4,'2100',col='blue',cex=tx)
text(2125,2.4,'1.9',col='blue',cex=tx)
text(2115.5,2.1,'2080',col='blue',cex=tx)
text(2125,2.1,round(mean(rcp26.series[121:150]),1),col='blue',cex=tx)
text(2115.5,mean(rcp26.series[91:120]),'2050',col='blue',cex=tx)
text(2125,mean(rcp26.series[91:120]),round(mean(rcp26.series[91:120]),1),col='blue',cex=tx)
#text(2115.5,mean(rcp26.series[61:90]),'2020',col='blue',cex=tx)
#text(2125,mean(rcp26.series[61:90]),round(mean(rcp26.series[61:90]),1),col='blue',cex=tx)

text(2135,3.4,'2100',col='orange',cex=tx)
text(2145,3.4,'3.2',col='orange',cex=tx)
text(2135,mean(rcp45.series[121:150]),'2080',col='orange',cex=tx)
text(2145,mean(rcp45.series[121:150]),round(mean(rcp45.series[121:150]),1),col='orange',cex=tx)
text(2135,mean(rcp45.series[91:120]),'2050',col='orange',cex=tx)
text(2145,mean(rcp45.series[91:120]),round(mean(rcp45.series[91:120]),1),col='orange',cex=tx)
#text(2135,mean(rcp45.series[61:90]),'2020',col='orange',cex=tx)
#text(2145,mean(rcp45.series[61:90]),round(mean(rcp45.series[61:90]),1),col='orange',cex=tx)


text(2155,6.1,'2100',col='red',cex=tx)
text(2165,6.1,'6.1',col='red',cex=tx)
text(2155,mean(rcp85.series[121:150]),'2080',col='red',cex=tx)
text(2165,mean(rcp85.series[121:150]),round(mean(rcp85.series[121:150]),1),col='red',cex=tx)
text(2155,mean(rcp85.series[91:120]),'2050',col='red',cex=tx)
text(2165,mean(rcp85.series[91:120]),round(mean(rcp85.series[91:120]),1),col='red',cex=tx)
#text(2155,mean(rcp85.series[61:90]),'2020',col='red',cex=tx)
#text(2165,mean(rcp85.series[61:90]),round(mean(rcp85.series[61:90]),1),col='red',cex=tx)


 
lines(obs.years,obs.anoms,lwd=2,col='black')

box(which='plot')
legend('topleft',legend=c('PCDS','RCP8.5','RCP4.5','RCP2.6'),col=c('black','red','orange','blue'),pt.cex=1,cex=1.0,pch=15)
##legend('topleft',legend=c('PCDS','RCP8.5','RCP4.5'),col=c('black','red','orange'),cex=1.75,pch=15)

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
