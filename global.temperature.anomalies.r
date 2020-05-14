##Script to calculate climatologies from the derived variable files
##for CMIP6

library(raster)
library(ncdf4)

library(scales)

##---------------------------------------------------------
##Get the areas for the weighted average

get_areas <- function(lon,lat,dm) {
  lond <- diff(lon)[1]/2
  latd <- diff(lat)[1]/2

  lon.up <- lon + lond
  lon.dn <- lon - lond
  lat.up <- lat + latd
  lat.dn <- lat - latd

  areas <- matrix(NA,nrow=dm[2],ncol=dm[1])

  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
     areas[j,i] <- 2*(pi/180)*6371^2 * abs(sin(lat.up[j]/180*pi)-sin(lat.dn[j]/180*pi)) *abs(lon.up[i]-lon.dn[i])
    }
  }

  return(areas)
}


##---------------------------------------------------------
##Subset the derived file by the given time interval

subset_by_time <- function(input.file,interval,freq,read.dir,write.dir) {
   yrs <- strsplit(interval,'-')[[1]]
   subset.file <- gsub(pattern='[0-9]{8}-[0-9]{8}',replacement=interval,input.file)
   work <- paste0('cdo seldate,',yrs[1],'-01-01T00:00:00,',yrs[2],'-12-31T23:59:59 ',
                  read.dir,input.file,' ',write.dir,subset.file)
   system(work)
   Sys.sleep(1)                     
   return(subset.file)
}

##---------------------------------------------------------
##Subset the derived file by the given time interval

make_climatology <- function(input.file,clim.file,clim.fx,read.dir,write.dir) {

   work <- paste0('cdo timmean ',
                  read.dir,input.file,' ',write.dir,clim.file)
   system(work)
   Sys.sleep(1)                     
   ##return(clim.file)
}

##---------------------------------------------------------

##*********************************************************

tmp.dir <- '/local_temp/ssobie/global_tas/'
if (!file.exists(tmp.dir)) {
    dir.create(tmp.dir,recursive=T)
}

base.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/CMIP5/global/'

scenario <- 'rcp85'
intervals <- c('1971-2000','1986-2016','2011-2040','2041-2070','2071-2100','2091-2100')
##-------------------
##
gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
              'GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4',
              'MIROC5','MPI-ESM-LR','MRI-CGCM3')

gcm.clims <- matrix(NA,nrow=length(gcm.list),ncol=length(intervals))

for (g in seq_along(gcm.list)) {
   gcm <- gcm.list[g]
   print(gcm)
   read.dir <- paste0(base.dir,gcm,'/')
   files <- list.files(path=read.dir,pattern='tas_day')
   scen.files <- files[grep(scenario,files)]
   tas.file <- scen.files[grep('19000101-21001231',scen.files)]
   print(tas.file)
   ##Copy to temp
   file.copy(from=paste0(read.dir,tas.file),to=tmp.dir,overwrite=TRUE)
 
   ##Subset by time
   for (i in seq_along(intervals)) {
      interval <- intervals[i]
      print(interval)
      time.file <- subset_by_time(tas.file,interval,var.freq,
                                  tmp.dir,tmp.dir)
      clim.file <- gsub(pattern='tas_day',replacement='tas_day_climatology',time.file)
      make_climatology(time.file,clim.file,'timmean',tmp.dir,tmp.dir)

      nc <- nc_open(paste0(tmp.dir,clim.file))
      tas <- ncvar_get(nc,'tas')
      lon <- ncvar_get(nc,'lon')
      lat <- ncvar_get(nc,'lat')
      ar <- get_areas(lon,lat,dim(tas))
      global.wts <- ar[,1]/sum(ar[,1])
      global.avg <- mean(apply(tas,1,weighted.mean,global.wts,na.rm=T)) - 273
      old.global.avg <- mean(tas,na.rm=T) - 273
      nc_close(nc)

      gcm.clims[g,i] <- global.avg 
      ##file.copy(from=paste0(tmp.dir,clim.file),to=write.dir,overwrite=TRUE)
      ##Sys.sleep(1)
      ##file.remove(paste0(tmp.dir,clim.file))
      ##file.remove(paste0(tmp.dir,time.file))
   }
}

all.files <- list.files(path=tmp.dir,recursive=TRUE,full.name=TRUE)
##file.remove(all.files)