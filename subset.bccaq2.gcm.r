##Script to extract a variable from the BCCAQ GCM driven downscaling data
##and write the smaller output to a new netcdf file.
##This is necessary as the current files are too large use with ncks.

library(ncdf4)
library(PCICt)

ptm <- proc.time()

extract.gcm <- function(gcm,var.name,scenario,run,type,
	   		lon.bnds,lat.bnds,interval,
                        meta.dir,data.file.read,hist.dir,tmp.dir) {
   
      meta.file.names <- list.files(path=meta.dir,pattern=paste(gcm,'_historical\\+',scenario,'_',run,'*',sep=''))
      meta.file.read <- meta.file.names[grep(var.name,meta.file.names)]
      print(meta.file.read)

      ##Get Metadata
      nc <- nc_open(paste0(meta.dir,meta.file.read),write=FALSE)

      ##Time Atts
      time.atts <- ncatt_get(nc,'time')
      time.calendar <- time.atts$calendar
      time.units <- time.atts$units
      time.values <- ncvar_get(nc,'time') 
      origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
      time.series <- origin.pcict + time.values*86400
      pys <- strsplit(interval,'-')[[1]]

      write.new.name <- paste(var.name,'_day_BCCAQ2_',gcm,'_',scenario,'_',run,'_',pys[1],'-',pys[2],'.nc',sep='') ###  

      time.st <- head(grep(paste(pys[1],'-*',sep=''),time.series),1)
      time.en <- tail(grep(paste(pys[2],'-*',sep=''),time.series),1)
      if (length(time.en)==0)
        time.en <- length(time.values)

      time.ix <- time.st:time.en
      time.len <- length(time.ix)

      ##Attributes to retain
      lon <- ncvar_get(nc,'lon') ##- lon.offset
      lat <- ncvar_get(nc,'lat')  
  
      lon.atts <- ncatt_get(nc,'lon')
      lat.atts <- ncatt_get(nc,'lat')
      global.atts <- ncatt_get(nc,varid=0)

      data.atts <- ncatt_get(nc,var.name)

      time.sub <- time.values[time.st:time.en]  

      lon.st <- which.min(abs(lon-lon.bnds[1]))  
      lon.en <- which.min(abs(lon-lon.bnds[2]))
      lon.sub <- lon[lon.st:lon.en]
      n.lon <- length(lon.sub)

      lat.st <- which.min(abs(lat-lat.bnds[1]))
      lat.en <- which.min(abs(lat-lat.bnds[2]))
      lat.sub <- lat[lat.st:lat.en]
      n.lat <- length(lat.sub)

      ##--------------------------------------------------------------
      ##Create new netcdf file
      x.geog <- ncdim_def('lon', 'degrees_east', lon.sub)
      y.geog <- ncdim_def('lat', 'degrees_north', lat.sub)
      t.geog <- ncdim_def('time', time.units, time.sub,
                           unlim=TRUE, calendar=time.calendar)
  
      var.geog <- ncvar_def(var.name, units=data.atts$units, dim=list(x.geog, y.geog, t.geog),
                            missval=data.atts[['_FillValue']])

      new.nc <- nc_create(paste(tmp.dir,write.new.name,sep=''), var.geog)
  
      ##Loop over subsets of the time series
      global.names <- names(global.atts)
      for (g in 1:length(global.atts)) {
         ncatt_put(new.nc,varid=0,attname=global.names[g],attval=global.atts[[g]])
      }   
      time.names <- names(time.atts)
      for (j in 1:length(time.atts)) {
          ncatt_put(new.nc,varid='time',attname=time.names[j],attval=time.atts[[j]])
      }
      lon.names <- names(lon.atts)
      for (j in 1:length(lon.atts))  {
          ncatt_put(new.nc,varid='lon',attname=lon.names[j],attval=lon.atts[[j]])
      }
      lat.names <- names(lat.atts)
      for (j in 1:length(lat.atts))  {
          ncatt_put(new.nc,varid='lat',attname=lat.names[j],attval=lat.atts[[j]])
      }
      data.names <- names(data.atts)
      data.skip <- data.names %in% c('add_offset','scale_factor')
      print(data.names)
      data.atts <- data.atts[!data.skip]
      data.names <- data.names[!data.skip]

      for (j in 1:length(data.atts)) {
        ncatt_put(new.nc,varid=var.name,attname=data.names[j],attval=data.atts[[j]])
        print(data.atts[[j]])
     }

     data.nc <- nc_open(paste0(tmp.dir,data.file.read),write=FALSE)

     ##Subset the data
     for (i in 1:time.len) { 
       x <- time.ix[i]
       data.subset <- ncvar_get(data.nc,var.name,start=c(lon.st,lat.st,x),count=c(n.lon,n.lat,1))
       if (var.name=='pr')
         data.subset[data.subset < -10] <- NA
       if (var.name=='pr')
         data.subset[data.subset < 0] <- 0
       if (var.name=='tasmax' | var.name=='tasmin')
         data.subset[data.subset < -90] <- NA

       ncvar_put(new.nc,varid=var.name,vals=data.subset,
                 start=c(1,1,i),count=c(n.lon,n.lat,1))   
     }
  nc_close(nc)
  nc_close(new.nc)
  nc_close(data.nc)  

  file.copy(from=paste0(tmp.dir,write.new.name),to=hist.dir,overwrite=TRUE)
  print('Copying')
  print(paste0(tmp.dir,write.new.name))
  print(hist.dir)

  clean.up <- paste0('rm ',tmp.dir,write.new.name)
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

###gcm <- 'MIROC5'
###run <- 'r3i1p1'
###tmpdir <- '/local_temp/ssobie/extract/'

##data.dir <- '/storage/data/climate/downscale/CMIP5_delivery/qdm_files/'
meta.dir <- '/storage/data/climate/downscale/BCCAQ2/bccaqv2_with_metadata/'
data.dir <- '/storage/data/climate/downscale/BCCAQ2/qdm_files/'

write.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/'

tmp.dir <- '/local_temp/ssobie/extract/'
if (!file.exists(tmp.dir))
   dir.create(tmp.dir,recursive=TRUE)      
 
var.list <- c('pr','tasmax','tasmin') ##c('tasmax','tasmin','pr')
scenario <- 'rcp85'

for (var.name in var.list) {
  print(var.name)
  print(gcm)

  data.file.names <- list.files(path=data.dir,pattern=paste(gcm,'_historical\\+',scenario,'_',run,'*',sep=''))
  data.file.read <- data.file.names[grep(var.name,data.file.names)]
  print(paste0(data.dir,data.file.read))

  file.copy(from=paste0(data.dir,data.file.read),to=paste0(tmp.dir,data.file.read))
  print(paste0(tmp.dir,data.file.read))

  ##Full series
  hist.dir <- paste(write.dir,gcm,'/',sep='')
  if (!file.exists(hist.dir)) {
    dir.create(hist.dir,recursive=TRUE)      
  }
  past <- extract.gcm(gcm,var.name,scenario,run,type='BCCAQ2',
                      lon.bnds,lat.bnds,interval='1951-2000',
                      meta.dir,data.file.read,hist.dir,tmp.dir)
  proj <- extract.gcm(gcm,var.name,scenario,run,type='BCCAQ2',
                      lon.bnds,lat.bnds,interval='2001-2100',
                      meta.dir,data.file.read,hist.dir,tmp.dir)

  ##Baseline                      
  base.dir <- paste(write.dir,'baseline/',gcm,'/',sep='')
  if (!file.exists(base.dir)) {
    dir.create(base.dir,recursive=TRUE)      
  }
  base <- extract.gcm(gcm,var.name,scenario='rcp85',run,type='BCCAQ2',
                      lon.bnds,lat.bnds,interval='1981-2010',
                      meta.dir,data.file.read,base.dir,tmp.dir)


  ##Clean up
  clean.up <- paste0('rm ',tmp.dir,data.file.read)
  print(clean.up)
  system(clean.up)
  
}
 

##--------------------------------------------------------------


print('Elapsed time')
print(proc.time() - ptm)
