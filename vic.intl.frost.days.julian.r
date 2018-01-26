##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(climdex.pcic)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)


##Station Data

stn.file <- '/storage/data/projects/rci/data/assessments/crd/1018620_MIN_TEMP_EC.csv'
stn.data <- read.csv(stn.file,header=TRUE,as.is=TRUE)
stn.leap <- stn.data[,2] == 366
stn.yrs <- stn.data[!stn.leap,1]
stn.jday.fac <- as.factor(stn.data[!stn.leap,2])
stn.tasmin <- stn.data[!stn.leap,3]
stn.fd <- stn.tasmin < 0
stn.1980s <- seq(head(grep('1971',stn.yrs),1),
                 tail(grep('2000',stn.yrs),1))
stn.1990s <- seq(head(grep('1981',stn.yrs),1),
                 tail(grep('2010',stn.yrs),1))
stn.2000s <- seq(head(grep('2000',stn.yrs),1),
                 tail(grep('2017',stn.yrs),1))

stn.base <- seq(head(grep('1951',stn.yrs),1),
                 tail(grep('2005',stn.yrs),1))

stn.fd.1980s <- tapply(stn.fd[stn.1980s],stn.jday.fac[stn.1980s],function(y){sum(y,na.rm=T)})
stn.fd.1990s <- tapply(stn.fd[stn.1990s],stn.jday.fac[stn.1990s],function(y){sum(y,na.rm=T)})
stn.fd.2000s <- tapply(stn.fd[stn.2000s],stn.jday.fac[stn.2000s],function(y){sum(y,na.rm=T)})

stn.tn.all <- tapply(stn.tasmin,stn.jday.fac,function(y){mean(y,na.rm=T)})

stn.fd.all <- tapply(stn.fd,stn.jday.fac,function(y){sum(y,na.rm=T)})/length(unique(stn.yrs))*100
stn.tn.base <- mean(stn.tasmin[stn.base],na.rm=T)

read.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/'
plot.dir <- '/storage/data/projects/rci/data/assessments/crd/'

coords <- c(-123.4291,48.6417)
var.name <- 'tasmin'

gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CCSM4',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'HadGEM2-ES',
              'inmcm4',
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

tn.matrix <- matrix(NA,nrow=12,ncol=54750)


for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    all.names <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_day'),full.name=TRUE)
    file.names <- all.names[grep('rcp85',all.names)]
    past.file <- file.names[grep('1951',file.names)]
    proj.file <- file.names[grep('2001',file.names)]
    past.nc <- nc_open(past.file)
    proj.nc <- nc_open(proj.file)
    past.dates <- netcdf.calendar(past.nc)
    proj.dates <- netcdf.calendar(proj.nc)
    dates <- c(past.dates,proj.dates)

    lon <- ncvar_get(past.nc,'lon')
    lon.ix <- which.min(abs(lon-coords[1]))
    lat <- ncvar_get(past.nc,'lat')
    lat.ix <- which.min(abs(lat-coords[2]))
    past.sub <- ncvar_get(past.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    proj.sub <- ncvar_get(proj.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.sub <- c(past.sub,proj.sub)

    ##Leap Dates
    full.dates <- seq(from=as.Date('1951-01-01'),by='day',to=as.Date('2100-12-31'))
    fd.leap.flag <- grep('02-29',full.dates)
    noleap.dates <- full.dates[-fd.leap.flag]

    if (grepl('HadGEM2',gcm)) {
       print('Hadley 360 day garbage')
       ##150 years by 365 days - standard vector length
       had.len <- 150*365
       sv <- rep(NA,had.len)
       data.tmp <- sv
       hadley.ix <- seq(73,54750,73)
       hadley.flag <- (1:54750) %in% hadley.ix
       hadley.fill <- rep(NA,360) ##To fill in 2100
       data.tmp[!hadley.flag] <- c(data.sub,hadley.fill)
       rv <- data.tmp
    } else {
       leap.flag <- grep('02-29',dates)
       if (length(leap.flag)!=0) {
           print(paste0(gcm,' is Gregorian'))
           rv <- data.sub[-leap.flag]
       } else {
           print('365 day Calendar')
           rv <- data.sub
       }
    }

    dates.base <- seq(head(grep('1951',noleap.dates),1),
                      tail(grep('2005',noleap.dates),1))
    data.avg <- mean(rv[dates.base],na.rm=T)
    model.bias <- data.avg - stn.tn.base
    print(paste0(gcm,' bias:'))
    print(model.bias)
    tn.matrix[i,] <- rv - model.bias
}


##Compute Frost Days
fd.matrix <- tn.matrix < 0
jday.factor <- as.factor(rep(1:365,150))


yrs.1980s <- seq(grep('1971-01-01',noleap.dates),grep('2000-12-31',noleap.dates),1)
yrs.2020s <- seq(grep('2011-01-01',noleap.dates),grep('2040-12-31',noleap.dates),1)
yrs.2050s <- seq(grep('2041-01-01',noleap.dates),grep('2070-12-31',noleap.dates),1)
yrs.2080s <- seq(grep('2071-01-01',noleap.dates),grep('2100-12-31',noleap.dates),1)


fd.prct.1980s <- apply(fd.matrix[,yrs.1980s],1,function(x){tapply(x,jday.factor[yrs.1980s],function(y){sum(y,na.rm=T)})})
fd.prct.2020s <- apply(fd.matrix[,yrs.2020s],1,function(x){tapply(x,jday.factor[yrs.2020s],function(y){sum(y,na.rm=T)})})
fd.prct.2050s <- apply(fd.matrix[,yrs.2050s],1,function(x){tapply(x,jday.factor[yrs.2050s],function(y){sum(y,na.rm=T)})})
fd.prct.2080s <- apply(fd.matrix[,yrs.2080s],1,function(x){tapply(x,jday.factor[yrs.2080s],function(y){sum(y,na.rm=T)})})

##fd.2020s.clim <- apply((fd.prct.2020s-fd.prct.1980s),2,function(x,y){x+y},stn.fd.1980s)
##fd.2050s.clim <- apply((fd.prct.2050s-fd.prct.1980s),2,function(x,y){x+y},stn.fd.1980s)
##fd.2080s.clim <- apply((fd.prct.2080s-fd.prct.1980s),2,function(x,y){x+y},stn.fd.1980s)


yix <- 1:365 ##c(182:365,1:181)
clim <- 30
mean.1980s <- apply(fd.prct.1980s[yix,],1,mean)/clim*100
mean.2020s <- apply(fd.prct.2020s[yix,],1,mean)/clim*100
mean.2050s <- apply(fd.prct.2050s[yix,],1,mean)/clim*100
mean.2080s <- apply(fd.prct.2080s[yix,],1,mean)/clim*100

##png('/storage/data/projects/rci/data/assessments/crd/frost_days_vic_intl2.png',width=1200,height=600)
plot(c(),xlim=c(0,365),ylim=c(0,60),axes=FALSE,
        main='Victoria International Airport\nFrost Days',
        xlab='Day of the year',ylab='Percentage of Days with Frost (%)',cex.main=2,cex.lab=1.5,xaxs='i',yaxs='i')
##lines(1:365,stn.fd.1980s[yix]/30*100,col='black',lwd=3,lty=2)
##lines(1:365,stn.fd.1990s[yix]/30*100,col='black',lwd=3,lty=2)
lines(1:365,stn.fd.2000s[yix]/18*100,col='black',lwd=3,lty=2)
lines(1:365,stn.fd.all[yix],col='blue',lwd=5)
##lines(1:365,mean.1980s,col='black',lwd=5)
lines(1:365,mean.2020s,col='green',lwd=5)
lines(1:365,mean.2050s,col='orange',lwd=5)
lines(1:365,mean.2080s,col='red',lwd=5)
##axis(1,c('Jul','Aug','Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr','May','Jun'),
##      at=c(1,32,63,93,124,154,185,216,244,275,305,336),cex=2,cex.lab=2,cex.axis=1.5)
axis(1,seq(0,365,30),at=seq(0,365,30),cex=2,cex.lab=2,cex.axis=1.5)
axis(2,seq(0,60,15),at=seq(0,60,15),cex=2,cex.lab=2,cex.axis=1.5)
abline(h=seq(0,60,15),lty=2,col='gray',lwd=0.5)
legend('top',leg=c('Station (1940-2017)','2020s','2050s','2080s'),
                 col=c('blue','green','orange','red'),pch=15,cex=1.5)
box(which='plot')
##dev.off()