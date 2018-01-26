##Script to extract a variable from the BCCAQ GCM driven downscaling data
##and write the smaller output to a new netcdf file.
##This is necessary as the current files are too large use with ncks.

library(ncdf4)
library(PCICt)

ptm <- proc.time()

extract.gcm <- function(gcm,var.name,scenario,type,
	   		lon.bnds,lat.bnds,past.int,proj.int,
                        data.dir,hist.dir,tmp.dir) {
   
      file.names <- list.files(path=data.dir,pattern=paste(gcm,'_historical\\+',scenario,'_r1i1p1*',sep=''))
      file.read <- file.names[grep(var.name,file.names)]
      print(file.read)

      move.to <- paste("rsync -av ",data.dir,file.read,' ',tmp.dir,file.read,sep='')
      print(move.to)
      system(move.to)

      file.name <- paste0(tmp.dir,file.read)

      file.split <- strsplit(file.name,'_')[[1]]
      run <- file.split[grep('r*i1p1',file.split)]

      lon.offset <- 0
      pys <- strsplit(past.int,'-')[[1]]
      fys <- strsplit(proj.int,'-')[[1]]

      write.hist.name <- paste(var.name,'_day_BCCAQ2_',gcm,'_',scenario,'_',run,'_',pys[1],'-',pys[2],'.nc',sep='') ###
      write.proj.name <- paste(var.name,'_day_BCCAQ2_',gcm,'_',scenario,'_',run,'_',fys[1],'-',fys[2],'.nc',sep='')

##--------------------------------------------------------------

  nc <- nc_open(file.name,write=FALSE)

  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  time.values <- ncvar_get(nc,'time') 
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
  time.series <- origin.pcict + time.values*86400
  pys <- strsplit(past.int,'-')[[1]]
  fys <- strsplit(proj.int,'-')[[1]]

    past.st <- head(grep(paste(pys[1],'-*',sep=''),time.series),1) ###
    past.en <- tail(grep(paste(pys[2],'-*',sep=''),time.series),1)

    proj.st <- head(grep(paste(fys[1],'-*',sep=''),time.series),1)
    proj.en <- length(time.series) ##tail(grep('2100-*',time.series),1)      

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

  hist.nc <- nc_create(paste(tmp.dir,write.hist.name,sep=''), var.geog)
  
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
  for (j in 1:length(data.atts)) {
    ncatt_put(hist.nc,varid=var.name,attname=data.names[j],attval=data.atts[[j]])
    print(data.atts[[j]])
  }

  for (i in 1:past.len) { 
    x <- past.ix[i]
    data.subset <- ncvar_get(nc,var.name,start=c(lon.st,lat.st,x),count=c(n.lon,n.lat,1))
    if (var.name=='pr')
      data.subset[data.subset < -10] <- NA
    if (var.name=='tasmax' | var.name=='tasmin')
      data.subset[data.subset < -90] <- NA

    ##data.adjust <- (data.subset - data.atts$add_offset)/data.atts$scale_factor
    ncvar_put(hist.nc,varid=var.name,vals=data.subset,
              start=c(1,1,i),count=c(n.lon,n.lat,1))   
  }

  nc_close(hist.nc)

  move.back <- paste('rsync -av ',tmp.dir,write.hist.name,' ',hist.dir,sep='')
  print(move.back)
  system(move.back)

  clean.up <- paste0('rm ',tmp.dir,write.hist.name)
  print(clean.up)
  system(clean.up)

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
    if (var.name=='pr')
      data.subset[data.subset < -10] <- NA
    if (var.name=='tasmax' | var.name=='tasmin')
      data.subset[data.subset < -90] <- NA
    ##data.adjust <- (data.subset - data.atts$add_offset)/data.atts$scale_factor    
    ncvar_put(proj.nc,varid=var.name,vals=data.subset,
              start=c(1,1,i),count=c(n.lon,n.lat,1))   
  }
  
  nc_close(proj.nc)
  move.back <- paste('rsync -av ',tmp.dir,write.proj.name,' ',hist.dir,sep='')
  print(move.back)
  system(move.back)

  clean.up <- paste0('rm ',tmp.dir,write.proj.name)
  print(clean.up)
  system(clean.up)

}
  nc_close(nc)



  clean.up <- paste0("rm ",tmp.dir,file.read)  
  print(clean.up)
  system(clean.up)

  
}

##**************************************************************************************

      ##BC Boundaries
        lon.bnds <- c(-140,-114)
        lat.bnds <- c(48,60)

##**************************************************************************************
##

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}


data.dir <- '/storage/data/climate/downscale/CMIP5_delivery/bccaqv2_with_metadata/'
write.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/' ##baseline/'
tmp.dir <- '/local_temp/ssobie/extract/'
if (!file.exists(tmp.dir))
   dir.create(tmp.dir,recursive=TRUE)      
 
var.list <- c('tasmax','tasmin','pr')  ##c('pr','tasmax','tasmin') ##c('tasmax','tasmin','pr')
scenario <- 'rcp45'
past.int <- '1951-2000'
proj.int <- '2001-2100'

for (var.name in var.list) {
  print(var.name)
  print(gcm)
  hist.dir <- paste(write.dir,gcm,'/',sep='')
  if (!file.exists(hist.dir)) {
    dir.create(hist.dir,recursive=TRUE)      
  }
  test <- extract.gcm(gcm,var.name,scenario,type='BCCAQ',
                      lon.bnds,lat.bnds,past.int,proj.int,
                      data.dir,hist.dir,tmp.dir)
}
 

##--------------------------------------------------------------


print('Elapsed time')
print(proc.time() - ptm)
