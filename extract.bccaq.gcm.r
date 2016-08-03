##Script to extract a variable from the BCCAQ GCM driven downscaling data
##and write the smaller output to a new netcdf file.
##This is necessary as the current files are too large use with ncks.

library(ncdf4)
library(PCICt)

extract.gcm <- function(gcm,rcm=NULL,var.name,scenario,
                        data.dir,write.dir) {
bccaq <- TRUE
run.gcm <- FALSE
anusplin <- FALSE
prism <- FALSE

  if (is.null(rcm)) {
    
    ##BCCAQ GCM Files
    if (bccaq==TRUE) {
      ##file.match <- list.files(path=paste(data.dir,gcm,sep=''),pattern=var.name)[1]
      file.name <- list.files(path=data.dir,pattern=paste(gcm,'_historical\\+',scenario,'_*',sep=''),full.name=TRUE)[1]
      ##file.name <- list.files(path=data.dir,pattern=paste('*bccaq\\+',gcm,'_historical\\+',scenario,'_*',sep=''),full.name=TRUE)[1]

      file.split <- strsplit(file.name,'_')[[1]]
      run <- file.split[grep('r*i1p1',file.split)]

      ##file.name <- paste(data.dir,gcm,'/',var.name,'_day_BCCAQ_',gcm,'_rcp85_',run,'_1971-2000.nc',sep='')
      lon.offset <- 0

      ##Reanalysis
      ##
      ##run <- strsplit(file.name,'_')[[1]][5]      
      ##file.name <- paste(data.dir,'pr+tasmax+tasmin_day_BCCAQ+ANUSPLIN300+NCEP1_1948-1990+1991-2011_evaluation_19480101-20111231.nc',sep='')
      ##paste(data.dir,'pr+tasmax+tasmin_day_ANUSPLIN300_observation_v20130130_19500101-20101231.nc',sep='')
        
      ##run <- 'base'
      ##write.hist.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_1951-2000.nc',sep='')
      ##write.proj.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_2001-2100.nc',sep='')
      
      #lon.offset <- 0
      write.hist.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_1951-2000.nc',sep='') ###
      write.proj.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',scenario,'_',run,'_2001-2100.nc',sep='')

      ##BC Boundaries
      ##      lon.bnds <- c(-140,-114)
      ##      lat.bnds <- c(48,60)
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
      ##Nanaimo Hospital Boundaries
      ##lon.bnds <- c(-124.30,-123.70)
      ##lat.bnds <- c(48.86,49.33)
      ##South Vancouver Island
      lon.bnds <- c(-124.60,-122.95)
      lat.bnds <- c(48.25,49.2)
      hist.dir <- paste(write.dir,gcm,'/',sep='')
      if (!file.exists(hist.dir))
        dir.create(hist.dir,recursive=TRUE)      
    }
    
    ##-----------------------------------
    if (run.gcm==TRUE) {
      ##GCM files - search through the main CMIP5 climate directory for files
      file.name <- list.files(path=paste(data.dir,gcm,'/',sep=''),pattern=paste('*',var.name,'_day_',gcm,'_historical\\+',scenario,'_*',sep=''),full.name=TRUE)[1]    

      file.split <- strsplit(file.name,'_')[[1]]
      run <- file.split[grep('r*i1p1',file.split)]

      write.hist.name <- paste(var.name,'_day_',gcm,'_',scenario,'_',run,'_1971-2000.nc',sep='')
      write.proj.name <- paste(var.name,'_day_',gcm,'_',scenario,'_',run,'_2041-2070.nc',sep='')

      lon.offset <- 360
      lon.bnds <- c(-145,-110)
      lat.bnds <- c(45,65)
      hist.dir <- paste(write.dir,gcm,'/',sep='')
      if (!file.exists(hist.dir))
        dir.create(hist.dir,recursive=TRUE)
      browser()
    }
    if (anusplin==TRUE) {
      file.name <- paste(data.dir,'pr+tasmax+tasmin_day_ANUSPLIN300_observation_v20141027_19500101-20121231.nc',sep='')
      ##paste(data.dir,'pr+tasmax+tasmin_day_ANUSPLIN300_observation_v20130130_19500101-20101231.nc',sep='')
      run <- 'v2013'
      lon.offset <- 0
      write.hist.name <- paste(var.name,'_day_ANUSPLIN_observation_',run,'_1950-1992.nc',sep='')
      write.proj.name <- NULL
      ##Health impacts boundaries ##BC Boundaries
      lon.bnds <- c(-123.303,-121.236) ##c(-140,-114)
      lat.bnds <- c(48.989,50.5197) ##c(48,60)
      hist.dir <- write.dir
      if (!file.exists(hist.dir))
        dir.create(hist.dir,recursive=TRUE)

    }
    if (prism==TRUE) {
      file.name <- paste(data.dir,var.name,'_monClim_PRISM_historical_run1_197101-200012.nc',sep='')
      lon.offset <- 0
      write.hist.name <- paste(var.name,'_day_PRISM_observation_1971-2000.nc',sep='')
      write.proj.name <- NULL
      ##Health impacts boundaries ##BC Boundaries
      ##lon.bnds <- c(-123.303,-121.236) ##c(-140,-114)
      ##lat.bnds <- c(48.989,50.5197) ##c(48,60)
      ##Metro Van/Whistler Region Boundaries
      lon.bnds <- c(-123.51,-120.69)
      lat.bnds <- c(48.99,50.41)
      ##Okanagan Region Boundaries
      ##lon.bnds <- c(-121.2,-118.1)
      ##lat.bnds <- c(48.99,51.1)
      ##Nanaimo Hospital Boundaries
      ##lon.bnds <- c(-124.24,-123.70)
      ##lat.bnds <- c(48.95,49.33)
      ##South Vancouver Island
      ##lon.bnds <- c(-124.60,-122.95)
      ##lat.bnds <- c(48.25,49.2)
      
      hist.dir <- write.dir
      if (!file.exists(hist.dir))
        dir.create(hist.dir,recursive=TRUE)
    }    
  } else {
    file.name <- list.files(path=data.dir,pattern=paste('*',gcm,'\\+',rcm,'_historical\\+',scenario,'_*',sep=''),full.name=TRUE)[1]

    file.split <- strsplit(file.name,'_')[[1]]
    run <- file.split[grep('r*i1p1',file.split)]

    lon.offset <- 0
    write.hist.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',rcm,'_1971-2000.nc',sep='')
    write.proj.name <- paste(var.name,'_day_BCCAQ_',gcm,'_',rcm,'_2041-2070.nc',sep='')
    lon.bnds <- c(-140,-114)
    lat.bnds <- c(48,60)
    hist.dir <- paste(write.dir,gcm,'_',rcm,'/',sep='')
    if (!file.exists(hist.dir))
      dir.create(hist.dir,recursive=TRUE)    
  }

  print(file.name)

##--------------------------------------------------------------

  nc <- nc_open(file.name,write=FALSE)

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  time.values <- ncvar_get(nc,'time') 
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
  time.series <- origin.pcict + time.values*86400

  if (is.null(rcm)) {
    if (bccaq) {
      past.st <- head(grep('1951-*',time.series),1) ###
      past.en <- tail(grep('2000-*',time.series),1)

      proj.st <- head(grep('2001-*',time.series),1)
      ##proj.en <- tail(grep('2100-*',time.series),1)
      ##proj.st <- 1 ##head(grep('1991-*',time.series),1)
      proj.en <- length(time.series) ##tail(grep('2100-*',time.series),1)      
    } else if (prism) {
      past.st <- 1
      past.en <- 12
      proj.st <- 1
      proj.en <- 12
    }
    
  } else {
    past.st <- head(grep('1971-*',time.series),1)
    past.en <- tail(grep('2000-*',time.series),1)
    
    proj.st <- head(grep('2041-*',time.series),1)
    proj.en <- tail(grep('2070-*',time.series),1)
  }
  
if (length(past.en)==0)
  past.en <- proj.st-1
  past.ix <- past.st:past.en
  past.len <- length(past.ix)
  
  proj.ix <- proj.st:proj.en
  proj.len <- length(proj.ix)
  
  ##Attributes to retain
  lon <- ncvar_get(nc,'lon') - lon.offset
  lat <- ncvar_get(nc,'lat')  
  
  lon.atts <- ncatt_get(nc,'lon')
  lat.atts <- ncatt_get(nc,'lat')
  global.atts <- ncatt_get(nc,varid=0)

  data.atts <- ncatt_get(nc,var.name)

  data.att.sub <- !(names(data.atts) %in% c('add_offset','scale_factor'))
  data.atts <- data.atts[data.att.sub]

  past.time.sub <- time.values[past.st:past.en]
  proj.time.sub <- time.values[proj.st:proj.en]

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
  t.geog <- ncdim_def('time', time.units, past.time.sub,
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
    x <- past.ix[i]
    data.subset <- ncvar_get(nc,var.name,start=c(lon.st,lat.st,x),count=c(n.lon,n.lat,1))
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
  x.geog <- ncdim_def('lon', 'degrees_east', lon.sub)
  y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
  t.geog <- ncdim_def('time', time.units, proj.time.sub,
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
    x <- proj.ix[i]
    data.subset <- ncvar_get(nc,var.name,start=c(lon.st,lat.st,x),count=c(n.lon,n.lat,1))
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

}


fix.temperatures <-  function(gcm,rcm) {

  base.dir <- '/storage/data/projects/rci/data/stat.downscaling/scaling_comparison/bccaq_rcm_bc_subset/'
  if (is.null(rcm))
    data.dir <- paste(base.dir,gcm,'/',sep='')
  if (!is.null(rcm))
    data.dir <- paste(base.dir,gcm,'_',rcm,'/',sep='')
                      
  tasmax.name <- list.files(path=data.dir,pattern='tasmax',full.name=TRUE)
  tasmin.name <- list.files(path=data.dir,pattern='tasmin',full.name=TRUE)

  system(paste('ncatted -a units,tasmax,o,c,degC ', tasmax.name,sep=''))
  system(paste('ncatted -a long_name,tasmax,o,c,"Maximum Temperature" ', tasmax.name,sep=''))
  system(paste('ncatted -a standard_name,tasmax,o,c,"Maximum Temperature" ', tasmax.name,sep=''))

  system(paste('ncatted -a units,tasmin,o,c,degC ', tasmin.name,sep=''))
  system(paste('ncatted -a long_name,tasmin,o,c,"Minimum Temperature" ', tasmin.name,sep=''))
  system(paste('ncatted -a standard_name,tasmin,o,c,"Minimum Temperature" ', tasmin.name,sep=''))
 
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


  rcm.list <- list(c('CCSM','CRCM'),
                   c('CCSM','MM5I'),
                   c('CCSM','WRFG'),
                   c('CGCM3','CRCM'),
                   c('CGCM3','RCM3'),
                   c('CGCM3','WRFG'),
                   c('GFDL','ECP2'),
                   c('GFDL','HRM3'),
                   c('GFDL','RCM3'),
                   c('HADCM3','HRM3'),
                   c('HADCM3','MM5I'))

extract.bccaq.gcms <- function() {
  data.dir <- '/storage/data/climate/downscale/CMIP5/BCCAQ/'
    ##'/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm_bc_subset/rcp85/'
  ##'/storage/data/climate/ANUSPLIN/ANUSPLIN300_DAILY_STANDARD_GRIDS/'
  ##'/storage/data/climate/downscale/rean/BCCAQ/'    ## '/storage/data/climate/downscale/CMIP5/nobackup/CONUS/' ##
  write.dir <- '/storage/data/scratch/ssobie/bccaq_gcm_south_island_subset/'
    ##'/storage/data/scratch/ssobie/bccaq_gcm_metro_van_subset/'##
  ##gcm.list <- 'ACCESS1-0'
  var.list <- c('pr','tasmax','tasmin')
  scenario <- 'rcp85'

##  if (var.name != 'pr')
##    stop('Check adjustment for temperatures')
for (var.name in var.list) {
  for (model in gcm.list) {
    gcm <- model[1]
    rcm <- NULL 
    test <- extract.gcm(gcm,rcm,var.name,scenario,
                        data.dir,write.dir)
    
  }
}
}

extract.gcms <- function() {
  ##data.dir <- '/storage/data/climate/downscale/CMIP5/anusplin_downscaling_cmip5/preprocess/CMIP5/'
  data.dir <- '/storage/data/climate/CMIP5/output/'
  write.dir <- '/storage/data/projects/rci/data/stat.downscaling/scaling_comparison/gcm_bc_subset/'

  var.name <- 'tasmax'
  scenario <- 'rcp45'
  if (var.name != 'pr')
    stop('Check adjustment for temperatures')

  for (model in gcm.list) {
    gcm <- model[1]
    rcm <- NULL 
    test <- extract.gcm(gcm,rcm,var.name,scenario,
                        data.dir,write.dir)
  }
}

extract.bccaq.rcms <- function() {
  data.dir <- '/storage/data/climate/downscale/NARCCAP/BCCAQ/'
##  write.dir <- '/storage/data/projects/rci/data/stat.downscaling/scaling_comparison/bccaq_rcm_bc_subset/'
  write.dir <- '/storage/data/scratch/ssobie/bccaq_rcm_bc_subset/'

  var.name <- 'tasmin'
  scenario <- 'sresa2'
  
  for (model in rcm.list) {
    gcm <- model[1]
    rcm <- model[2]
    test <- extract.gcm(gcm,rcm,var.name,scenario,
                        data.dir,write.dir)

  }
}

extract.anusplin <- function() {
  data.dir <- '/storage/data/climate/ANUSPLIN/ANUSPLIN300_DAILY_STANDARD_GRIDS/'
  write.dir <- '/storage/data/projects/rci/data/stat.downscaling/BCCAQ/anusplin/'

  var.name <- 'tasmin'
  scenario <- 'observation'  

  test <- extract.gcm(gcm='ANUSPLIN',rcm=NULL,var.name,scenario,
                      data.dir,write.dir)
}

extract.prism <- function() {
  data.dir <- '/storage/data/climate/PRISM/dataportal/'
  write.dir <- '/storage/data/scratch/ssobie/bccaq_gcm_van_whistler_subset/PRISM/'
  var.name <- 'tmin'
  scenario <- 'observation'  

  test <- extract.gcm(gcm='PRISM',rcm=NULL,var.name,scenario,
                      data.dir,write.dir)
}


extract.bccaq.gcms()  
##extract.prism()
