##Generates a time series of meteo data from BCCAQ2 downscaled GCMS to 
##PNWNAmet, bias-corrected by PRISM for a single locations


library(ncdf4)
library(PCICt)


##******************************************************************************
################################################################################
##Calculate BCCAQ anomalies

create_anoms <- function(var.name,file,lonc,latc,var.mon,interval) {

  nc <- nc_open(file)
  lon <- ncvar_get(nc,'lon')
  lat <- ncvar_get(nc,'lat')
  lon.ix <- which.min(abs(lonc-lon))
  lat.ix <- which.min(abs(latc-lat))

  var.data <- ncvar_get(nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  time.values <- ncvar_get(nc,'time')
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)  
  var.dates <- origin.pcict + time.values*86400
  monthly.fac <- as.factor(format(var.dates,'%m'))
  
  ##Load full time series and take anomalies from this
  var.anoms <- var.data*0
  for(mn in 1:12) {
    print(mn)
    var.mean <- var.mon[mn]
    var.ix <- which(monthly.fac==sprintf('%02d',mn))
    mlen <- length(var.ix)
    for (i in 1:mlen) {
      ix <- var.ix[i]
      if (var.name=='pr')
        var.anoms[ix] <- var.data[ix]/var.mean
      if (grepl('tas',var.name))
        var.anoms[ix] <- var.data[ix] - var.mean
    }
  }

  nc_close(nc)
  rv <- list(anoms=var.anoms,fac=monthly.fac,time=var.dates)
  return(rv)

}

bccaq_anomalies <- function(var.name,scenario,lonc,latc,gcm,interval,base.dir) {
  print(paste('BCCAQ Anomalies: ',gcm,', ',var.name,sep=''))

  base.files <- list.files(path=paste(base.dir,'baseline/',gcm,sep=''),pattern=paste(var.name,'_day_',sep=''),full.name=TRUE)
  base.file <- base.files[grep('rcp45',base.files)]

  gcm.dir <- paste(base.dir,gcm,sep='')
  print(base.file)
  gnc <- nc_open(base.file)
  time.atts <- ncatt_get(gnc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  time.values <- ncvar_get(gnc,'time')
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)  
  var.dates <- origin.pcict + time.values*86400
  monthly.fac <- as.factor(format(var.dates,'%m'))
  monthly.ts.fac <- as.factor(format(var.dates,'%Y-%m'))
  mon.facs <- as.factor(format(as.Date(paste(levels(monthly.ts.fac),'-01',sep='')),'%m'))

  lon <- ncvar_get(gnc,'lon')
  lat <- ncvar_get(gnc,'lat')
  lon.ix <- which.min(abs(lonc-lon))
  lat.ix <- which.min(abs(latc-lat))

  var.data <- ncvar_get(gnc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))

  var.mon <- tapply(var.data,monthly.fac,mean,na.rm=TRUE)
  if (var.name=='pr') {
    var.data[var.data <=0] <- NA    
    var.test <- tapply(var.data,monthly.ts.fac,sum,na.rm=T)
    var.mon <-  tapply(var.test,mon.facs,mean,na.rm=T)
  }
  nc_close(gnc)
  ##------------------------------------  
  var.files <- list.files(path=gcm.dir,pattern=paste(var.name,'_day_',sep=''),full.name=TRUE)
  scen.files <- var.files[grep(scenario,var.files)]
  slct.file <- scen.files[grep(interval,scen.files)]

  ts.anomalies <- create_anoms(var.name=var.name,file=slct.file,
                               lonc=lonc,latc=latc,
                               var.mon=var.mon,interval=interval)
  rv <- ts.anomalies
  return(rv)

}

daily.prism.scale <- function(var.name,lonc,latc,gcm,interval,coarse.anoms,var.dates,base.dir) {

  print(paste('Daily PRISM: ',gcm,', ',var.name,', ',interval,sep=''))

  gcm.dir <- paste(base.dir,gcm,'/',sep='')

  prism.var <- switch(var.name,
                      pr='pr',
                      tasmax='tmax',
                      tasmin='tmin')
  prism.file <- paste(base.dir,'PRISM/',prism.var,'_day_PRISM_observation_1981-2010.nc',sep='')

  ##PRISM climatologies
  pnc <- nc_open(prism.file)
  lon <- ncvar_get(pnc,'lon')
  lat <- ncvar_get(pnc,'lat')
  lon.ix <- which.min(abs(lonc-lon))
  lat.ix <- which.min(abs(latc-lat))
  
  prism.clim <- ncvar_get(pnc,prism.var,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
  nc_close(pnc)

  ##Monthly factor from the BCCAQ dates  
  monthly.fac <- coarse.anoms$fac
  anoms <- coarse.anoms$anoms

  var.adjust <- anoms*0  
  for(mn in 1:12) {
     print(mn)
     prism.mean <- prism.clim[mn] ##cbind(addon,prism.clim[,,mn])
     var.ix <- which(monthly.fac==sprintf('%02d',mn))
     mlen <- length(var.ix)
     for (i in 1:mlen) {
        ix <- var.ix[i]
        var.sub <- var.adjust[ix]
        if (var.name=='pr') {
          var.sub <- anoms[ix]*prism.mean
        }

        if (grepl('tas',var.name)) {
          var.sub <- anoms[ix] + prism.mean
        }
        var.adjust[ix] <- var.sub
     }##Loop over indices    
  }##Loop over Months

  rv <- list(data=var.adjust,time=coarse.anoms$time)
  return(rv)
}
##--------------------------------------------------------------------------

retrieve_bccaq2_prism_series <- function(var.name,scenario,gcm.list,lonc,latc,base.dir) {

  ##Requires 1981-2010 (or equivalent base period) in the /baseline directory to create anomalies from the full period
  ## (usually 1950-2000 and 2001-2100). Both of these are created using extract.bccaq.gcm.r
  ##Also need the PRISM climatologies (also using extract.bccaq.gcm.r).
  full.series <- vector(mode='list',length=length(gcm.list))
 
  for (g in seq_along(gcm.list)) {
      gcm <- gcm.list[g]
      print(gcm)
      past.anoms <- bccaq_anomalies(var.name=var.name,scenario=scenario,
                                      gcm=gcm,interval='1951-2000',
                                      lonc=lonc,latc=latc,
                                      base.dir=base.dir)                                           
      past.series <- daily.prism.scale(var.name=var.name,
                                       gcm=gcm,interval='1951-2000',
                                       lonc=lonc,latc=latc,
                                       coarse.anoms=past.anoms,
                                       base.dir=base.dir)
      proj.anoms <- bccaq_anomalies(var.name=var.name,scenario=scenario,
                                      gcm=gcm,interval='2001-2100',
                                      lonc=lonc,latc=latc,
                                      base.dir=base.dir)                                           
      proj.series <- daily.prism.scale(var.name=var.name,
                                       gcm=gcm,interval='2001-2100',
                                       lonc=lonc,latc=latc,
                                       coarse.anoms=proj.anoms,
                                       base.dir=base.dir)

      full.series[[g]] <- list(data=c(past.series$data,proj.series$data),
                               dates=c(past.series$time,proj.series$time))
  }  
  return(full.series)
}

################################################################################
##******************************************************************************

##base.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/'
##base.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/bccaq2_tps/BCCAQ2/'

##gcm.list <- c('ACCESS1-0','CCSM4','CanESM2',
##              'CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
##              'HadGEM2-CC','HadGEM2-ES','inmcm4',
##              'MIROC5','MPI-ESM-LR','MRI-CGCM3')
##gcm.list <- c('ACCESS1-0','CanESM2')

##bccaq2.series <- retrieve_bccaq2_prism_series(
##                             var.name='pr',scenario='rcp85',
##                             gcm.list=gcm.list,
##                             lonc=-120.7,latc=56.2,
##                             base.dir=base.dir)



