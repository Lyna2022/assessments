##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)
library(zoo)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)


extract.series <- function(read.dir,gcm,scenario,var.name,lon.ix,lat.ix) {

    file.names <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_gcm_prism'),full.name=TRUE) 
    ##file.names <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_day'),full.name=TRUE) 
    scen.files <- file.names[grep(scenario,file.names)]
    past.file <- scen.files[grep('1951-2000',scen.files)]
    print(past.file)   
    proj.file <- scen.files[grep('2001-2100',scen.files)]
    print(proj.file)

    past.nc <- nc_open(past.file)
    proj.nc <- nc_open(proj.file)
    
    data.past <- ncvar_get(past.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.proj <- ncvar_get(proj.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.sub <- round(c(data.past,data.proj),1)

    past.time <- netcdf.calendar(past.nc)
    proj.time <- netcdf.calendar(proj.nc)
    full.time <- as.character(c(past.time,proj.time))
    nc_close(past.nc)
    nc_close(proj.nc)
    rv <- list(time=full.time,
                 data=data.sub)

    return(rv)    
}


bccaq.correct.for.dates <- function(tx,tn,pr,series.dates,gcm) {

   full.dates <- seq(from=as.Date('1951-01-01'),by='day',to=as.Date('2100-12-31'))                      
   fd.leap.flag <- grep('02-29',full.dates)           
   noleap.dates <- full.dates[-fd.leap.flag]
 
   ##If Hadley, fill in the 5 missing days per year
   if (grepl('HadGEM2',gcm)) {
      print('Hadley 360 day garbage')
      ##150 years by 365 days - standard vector length
      had.len <- 150*365
      sv <- rep(NaN,had.len)               
      mat.template <- cbind(as.character(noleap.dates),sv,sv,sv)
      hadley.ix <- seq(73,54750,73)   
      hadley.flag <- (1:54750) %in% hadley.ix
      hadley.fill <- rep(NaN,360) ##To fill in 2100
      mat.template[!hadley.flag,2] <- c(tx,hadley.fill)
      mat.template[!hadley.flag,3] <- c(tn,hadley.fill)
      mat.template[!hadley.flag,4] <- c(pr,hadley.fill)
      rv <- rbind(c('DATE','TASMAX','TASMIN','PR'),
                   mat.template)
   } else {
          
      data.matrix <- rbind(c('DATE','TASMAX','TASMIN','PR'), 
                        cbind(as.character(series.dates),tx,tn,pr))
      print(dim(data.matrix))                        
      ##Strip out the leap days if they exist
      data.noleap <- data.matrix   
      leap.flag <- grep('02-29',series.dates)           
      if (length(leap.flag)!=0) {
         print(paste0(gcm,' is Gregorian'))
         rv <- data.matrix[-leap.flag,]      
         } else {
      rv <- data.matrix
      }                       
   }   

   print(dim(rv))
   return(rv)
}

gcm.correct.for.dates <- function(input.matrix,gcm) {

   series.dates <- input.matrix[-1,1]                      
   full.dates <- seq(from=as.Date('1951-01-01'),by='day',to=as.Date('2100-12-31'))                      
   fd.leap.flag <- grep('02-29',full.dates)           
   noleap.dates <- full.dates[-fd.leap.flag]
 
   ##If Hadley, fill in the 5 missing days per year
   if (grepl('HadGEM2',gcm)) {
                          
      print('Hadley 360 day garbage')
      ##150 years by 365 days - standard vector length
      had.len <- 150*365
      sv <- rep(NaN,had.len)               
##Change the column length to match the number of variables     
      mat.template <- cbind(as.character(noleap.dates),sv,sv,sv,sv) ##4 GCM variables
      hadley.ix <- seq(73,54750,73)   
      hadley.flag <- (1:54750) %in% hadley.ix
      ##Add in the missing December 2005 points
      dec.05 <- grep('2005-12-*',noleap.dates)
      hadley.flag[dec.05] <- TRUE

##Change the column length to match the number of variables     
      mat.template[!hadley.flag,2:5] <- c(input.matrix[-1,2:5])

##
      rv <- rbind(input.matrix[1,],
                   mat.template)

   } else {
          
      print(dim(input.matrix))                        
      ##Strip out the leap days if they exist
      data.noleap <- input.matrix   
      leap.flag <- grep('02-29',series.dates)           

      if (length(leap.flag)!=0) {
         print(paste0(gcm,' is Gregorian'))
         rv <- input.matrix[-leap.flag,]      
         } else {
      rv <- input.matrix
      }                       
   }   

   print(dim(rv))

   return(rv)
}

read.dir <- '/storage/data/climate/downscale/CMIP5/BCCAQ/'
file.rcp26 <- paste0(read.dir,'hadgem2-es_rcp26_annual.nc')
file.rcp45 <- paste0(read.dir,'hadgem2-es_rcp45_annual.nc')
file.rcp85 <- paste0(read.dir,'hadgem2-es_rcp85_annual.nc')


clip.shp <- readOGR('/storage/data/projects/rci/data/assessments/nfld/shapefiles','nfld', stringsAsFactors=F)

tx.rcp26 <- gcm.netcdf.climatologies(file.rcp26,'tasmax','HadGEM2-ES',NULL,clip.shp)
tx.rcp45 <- gcm.netcdf.climatologies(file.rcp45,'tasmax','HadGEM2-ES',NULL,clip.shp)
tx.rcp85 <- gcm.netcdf.climatologies(file.rcp85,'tasmax','HadGEM2-ES',NULL,clip.shp)

tn.rcp26 <- gcm.netcdf.climatologies(file.rcp26,'tasmin','HadGEM2-ES',NULL,clip.shp)
tn.rcp45 <- gcm.netcdf.climatologies(file.rcp45,'tasmin','HadGEM2-ES',NULL,clip.shp)
tn.rcp85 <- gcm.netcdf.climatologies(file.rcp85,'tasmin','HadGEM2-ES',NULL,clip.shp)

rcp26.series <- apply((tx.rcp26+tn.rcp26)/2,2,mean,na.rm=T)
rcp45.series <- apply((tx.rcp45+tn.rcp45)/2,2,mean,na.rm=T)
rcp85.series <- apply((tx.rcp85+tn.rcp85)/2,2,mean,na.rm=T)


plot.dir <- '/storage/data/projects/rci/data/assessments/nfld/'
plot.file <- paste0(plot.dir,'nfld.annual.tas.png')

png(plot.file,width=900,height=900)
par(mar=c(5,5,5,3))
plot(1950:2100,rcp26.series,type='l',lwd=4,col='green',ylim=c(0,15),
     main='NFLD Annual Average Temperatures',xlab='Year',ylab='TAS (degC)',
     cex.axis=2,cex.lab=2,cex.main=2.5)
lines(1950:2100,rcp45.series,lwd=4,col='orange')
lines(1950:2099,rcp85.series,lwd=4,col='red')
abline(h=seq(0,20,5),col='gray',lty=2,lwd=3)
box(which='plot')
legend('topleft',legend=c('RCP8.5','RCP4.5','RCP2.6'),col=c('red','orange','green'),cex=2,pch=15)
dev.off()

rcp26.mean <- rollmean(rcp26.series,11)
rcp45.mean <- rollmean(rcp45.series,11)
rcp85.mean <- rollmean(rcp85.series,11)

plot.file <- paste0(plot.dir,'nfld.annual.tas.smoothed.png')
png(plot.file,width=900,height=900)
par(mar=c(5,5,5,3))
plot(1955:2095,rcp26.mean,type='l',lwd=4,col='green',ylim=c(0,15),
     main='NFLD Smoothed Annual Average Temperatures',xlab='Year',ylab='TAS (degC)',
     cex.axis=2,cex.lab=2,cex.main=2.5)
lines(1955:2095,rcp45.mean,lwd=4,col='orange')
lines(1955:2094,rcp85.mean,lwd=4,col='red')
abline(h=seq(0,20,5),col='gray',lty=2,lwd=3)
legend('topleft',legend=c('RCP8.5','RCP4.5','RCP2.6'),col=c('red','orange','green'),cex=2,pch=15)
box(which='plot')
dev.off()
