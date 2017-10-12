##Script to extract a variable from the BCCAQ GCM driven downscaling data
##and write the smaller output to a new netcdf file.
##This is necessary as the current files are too large use with ncks.

library(ncdf4)
library(PCICt)

ptm <- proc.time()

extract.gcm <- function(gcm,rcm=NULL,var.name,scenario,type,
	   		lon.bnds,lat.bnds,past.int,proj.int,
                        data.dir,write.dir) {
   
    ##BCCAQ GCM Files
    if (type=='BCCAQ') {
      
      file.names <- list.files(path=paste0(data.dir,gcm),pattern=paste('*BCCAQ_',gcm,'_',scenario,'_*',sep=''),full.name=TRUE) 
      past.file <- file.names[grep(past.int,file.names)]
      proj.file <- file.names[grep(proj.int,file.names)]
      
      file.split <- strsplit(past.file,'_')[[1]]
      run <- file.split[grep('r*i1p1',file.split)]

      lon.offset <- 0

      pys <- strsplit(past.int,'-')[[1]]
      fys <- strsplit(proj.int,'-')[[1]]

      write.hist.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_',pys[1],'-',pys[2],'.nc',sep='') ###
      write.proj.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_',fys[1],'-',fys[2],'.nc',sep='')

      hist.dir <- paste(write.dir,gcm,'/',sep='')
      if (!file.exists(hist.dir))
        dir.create(hist.dir,recursive=TRUE)      
    }
    
##--------------------------------------------------------------

  nc <- nc_open(past.file,write=FALSE)

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  past.time.values <- ncvar_get(nc,'time') 
  past.len <- length(past.time.values)

  ##Attributes to retain
  lon <- ncvar_get(nc,'lon') - lon.offset
  lat <- ncvar_get(nc,'lat')  
  
  lon.atts <- ncatt_get(nc,'lon')
  lat.atts <- ncatt_get(nc,'lat')
  global.atts <- ncatt_get(nc,varid=0)

  data.atts <- ncatt_get(nc,var.name)

  data.att.sub <- !(names(data.atts) %in% c('add_offset','scale_factor'))
  data.atts <- data.atts[data.att.sub]

  lon.st <- which.min(abs(lon-lon.bnds[1]))  
  lon.en <- which.min(abs(lon-lon.bnds[2]))
  lon.sub <- lon[lon.st:lon.en]
  n.lon <- length(lon.sub)

  lat.st <- which.min(abs(lat-lat.bnds[1]))
  lat.en <- which.min(abs(lat-lat.bnds[2]))
  lat.sub <- lat[lat.st:lat.en]
  n.lat <- length(lat.sub)
 
  ##--------------------------------------------------------------
  ##Create new past netcdf file
if (1==1) {
  x.geog <- ncdim_def('lon', 'degrees_east', lon.sub)
  y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
  t.geog <- ncdim_def('time', time.units, past.time.values,
                      unlim=TRUE, calendar=time.calendar)
  
  var.geog <- ncvar_def(var.name, units=data.atts$units, dim=list(x.geog, y.geog, t.geog),
                        missval=data.atts[['_FillValue']])

  hist.nc <- nc_create(paste(hist.dir,write.hist.name,sep=''), var.geog)
  
  ##Loop over subsets of the time series
  ##Past file first
  global.names <- names(global.atts)
  for (g in 1:length(global.atts)) 
    ncatt_put(hist.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
  
  time.names <- names(time.atts)
  for (j in 1:length(time.atts)) 
    ncatt_put(hist.nc,varid='time',attname=time.names[j],attval=time.atts[[j]])
  
  lon.names <- names(lon.atts)
  for (j in 1:length(lon.atts))  
    ncatt_put(hist.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])
  
  lat.names <- names(lat.atts)
  for (j in 1:length(lat.atts))  
    ncatt_put(hist.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
  
  data.names <- names(data.atts)
  for (j in 1:length(data.atts))
    ncatt_put(hist.nc,varid=var.name,attname=data.names[j],attval=data.atts[[j]])

  for (i in 1:past.len) {    
    data.subset <- ncvar_get(nc,var.name,start=c(lon.st,lat.st,i),count=c(n.lon,n.lat,1))
    data.adjust <- data.subset ##(data.subset - data.atts$add_offset)/data.atts$scale_factor
    if (var.name=='pr')
      data.adjust[data.adjust < -10] <- NA
    if (var.name=='tasmax' | var.name=='tasmin')
      data.adjust[data.adjust < -90] <- NA
    ncvar_put(hist.nc,varid=var.name,vals=data.adjust,
              start=c(1,1,i),count=c(n.lon,n.lat,1))
    
  }

  nc_close(hist.nc)
}
  ##--------------------------------------------------------------
  ##--------------------------------------------------------------
  ##Create new netcdf file for the future

if (1==1) {

  fnc <- nc_open(proj.file,write=FALSE)
  proj.time.values <- ncvar_get(fnc,'time') 
  proj.len <- length(proj.time.values)

  x.geog <- ncdim_def('lon', 'degrees_east', lon.sub)
  y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
  t.geog <- ncdim_def('time', time.units, proj.time.values,
                      unlim=TRUE, calendar=time.calendar)
  
  var.geog <- ncvar_def(var.name, units=data.atts$units, dim=list(x.geog, y.geog, t.geog),
                      missval=data.atts[['_FillValue']])
  proj.nc <- nc_create(paste(hist.dir,write.proj.name,sep=''), var.geog)
  
  ##Loop over subsets of the time series
  
  global.names <- names(global.atts)
  for (g in 1:length(global.atts)) 
    ncatt_put(proj.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
  
  time.names <- names(time.atts)
  for (j in 1:length(time.atts)) 
    ncatt_put(proj.nc,varid='time',attname=time.names[j],attval=time.atts[[j]])
  
  lon.names <- names(lon.atts)
  for (j in 1:length(lon.atts))  
    ncatt_put(proj.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])
  
  lat.names <- names(lat.atts)
  for (j in 1:length(lat.atts))  
    ncatt_put(proj.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
  
  data.names <- names(data.atts)
  for (j in 1:length(data.atts))
    ncatt_put(proj.nc,varid=var.name,attname=data.names[j],attval=data.atts[[j]])
  
  for (i in 1:proj.len) {
    data.subset <- ncvar_get(fnc,var.name,start=c(lon.st,lat.st,i),count=c(n.lon,n.lat,1))
    data.adjust <- data.subset  ##    data.adjust <- (data.subset - data.atts$add_offset)/data.atts$scale_factor
    if (var.name=='pr')
      data.adjust[data.adjust < -10] <- NA
    if (var.name=='tasmax' | var.name=='tasmin')
      data.adjust[data.adjust < -90] <- NA
    
    ncvar_put(proj.nc,varid=var.name,vals=data.adjust,
              start=c(1,1,i),count=c(n.lon,n.lat,1))
    
  }
  
  nc_close(proj.nc)
}
  nc_close(nc)
  nc_close(fnc)

}



fix.precipitation <-  function(gcm,rcm) {

  base.dir <- '/storage/data/projects/rci/data/stat.downscaling/scaling_comparison/rcm/'
  if (is.null(rcm))
    data.dir <- paste(base.dir,gcm,'/',sep='')
  if (!is.null(rcm))
    data.dir <- paste(base.dir,gcm,'.',rcm,'/',sep='')
                      
  pr.names <- list.files(path=data.dir,pattern='pr',full.name=TRUE)
  pr.file <- pr.names[grep('2041-2070',pr.names)]
  print(pr.file)

  system(paste('ncatted -a units,pr,o,c,"kg m-2 s-1" ', pr.names,sep=''))

 
}



##**************************************************************************************

      ##BC Boundaries
        ##lon.bnds <- c(-140,-114)
        ##lat.bnds <- c(48,60)
      ##MOTI Boundaries
      ##lon.bnds <- c(-127.0,-126.0)
      ##lat.bnds <- c(52,53)
      ##Metro Vancouver Boundaries
      ##lon.bnds <- c(-123.5,-122.4)
      ##lat.bnds <- c(49,49.6)
      ##PNW Boundaries for US
      ##lon.bnds <- c(-125,-114)
      ##lat.bnds <- c(41.5,49)
      ##Metro Van/Whistler Region Boundaries
      ##lon.bnds <- c(-123.51,-120.69)
      ##lat.bnds <- c(48.99,50.41)
      ##Okanagan Region Boundaries
      ##lon.bnds <- c(-121.2,-118.025)
      ##lat.bnds <- c(48.99,51.1)
      ##Health impacts boundaries ##BC Boundaries
      ##lon.bnds <- c(-123.303,-121.236)
      ##lat.bnds <- c(48.989,50.5197)
      ##Nanaimo Hospital Boundaries
      lon.bnds <- c(-124.30,-123.70)
      lat.bnds <- c(48.86,49.33)
      ##South Vancouver Island
      ##lon.bnds <- c(-124.90,-122.95)
      ##lat.bnds <- c(48.25,49.2)
      ##Upper Adams River
      ##lon.bnds <- c(-119.89,-118.4)
      ##lat.bnds <- c(50.90,52.26)
      ##Revelstoke Region
      ##lon.bnds <- c(-118.50,-117.88)
      ##lat.bnds <- c(50.67,51.3)
      ##Block Test
      ##lon.bnds <- c(-130.0,-120.0)
      ##lat.bnds <- c(49.0,56.0)


##**************************************************************************************

gcm.list <- c('ACCESS1-0',
              'CCSM4',
              'CanESM2',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',              
              'HadGEM2-CC',              
              'HadGEM2-ES',
              'inmcm4',              
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

##gcm.list <- 'ACCESS1-0'

extract.bccaq.gcms <- function() {
  data.dir <- '/storage/data/climate/downscale/BCCAQ2/high_res_downscaling/bccaq_gcm_bc_subset/'

  write.dir <- '/storage/data/climate/downscale/BCCAQ2/high_res_downscaling/bccaq_gcm_nanaimo_subset/baseline/'

  var.list <- c('tasmax','tasmin','pr')
  scenario <- 'rcp85'

  past.int <- '1971-2000'
  proj.int <- '2001-2100'

  for (var.name in var.list) {
    print(var.name)
    for (model in gcm.list) {      
      gcm <- model[1]
      print(gcm)
      rcm <- NULL 
      test <- extract.gcm(gcm,rcm,var.name,scenario,type='BCCAQ',
			lon.bnds,lat.bnds,past.int,proj.int,
                        data.dir,write.dir)
    }
  }
}

##--------------------------------------------------------------

extract.bccaq.gcms()  

print('Elapsed time')
print(proc.time() - ptm)
