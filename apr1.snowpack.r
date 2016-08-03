##Script to add attributes to the aggregated downscaled output.

library(ncdf4)
library(PCICt)

##RCP26
## 
## 
## 
##

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

avg.sub.time <- function(day.file,seas.file,var.name,interval,read.dir,write.dir) {

  nc <- nc_open(paste(read.dir,day.file,sep=''))
  anc <- nc_open(paste(write.dir,seas.file,sep=''),write=TRUE)
  time.atts <- ncatt_get(nc,'time')
  time.calendar <- time.atts$calendar
  time.units <- time.atts$units
  time.values <- ncvar_get(nc,'time')
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
  time.series <- origin.pcict + time.values*86400
  years <- format(time.series,'%Y')
  yrs <- strsplit(interval,'-')[[1]]
  st <- head(grep(yrs[1],format(time.series,'%Y')),1)
  en <- tail(grep(yrs[2],format(time.series,'%Y')),1)

  ##apr1.dates <- grep('-04-01',time.series[st:en])
  may1.dates <- grep('-05-01',time.series[st:en])

  subset.data <- ncvar_get(nc,var.name,start=c(1,1,st),count=c(-1,-1,en-st+1))
  ##april.data <- subset.data[,,apr1.dates]
  may.data <-  subset.data[,,may1.dates]
  rm(subset.data)
  ##april.mean <- apply(april.data,c(1,2),mean,na.rm=T)
  may.mean <- apply(may.data,c(1,2),mean,na.rm=T)
  ncvar_put(anc,varid=var.name,vals=may.mean)
  ##ncvar_put(anc,varid=var.name,vals=april.mean)

##  system(paste('ncks --overwrite -d time,',st,',',en,' ',read.dir,day.file,' ',write.dir,sub.file,sep=''))
#  system(paste('cdo -s -O seltimestep,',timesteps,' ',read.dir,day.file,' ',write.dir,'tmp.nc',sep=''))
#  system(paste('cdo -s -O timmean ',write.dir,'tmp.nc ',write.dir,seas.file,sep=''))
#  system(paste('rm ',write.dir,'tmp.nc',sep=''))
  nc_close(nc)
  nc_close(anc)  
}

get.apr1.snowpack <- function() {
  ds.type <- 'bccaq'
  var.list <- 'snowdepth'
  past.int <- '1971-2000'
  proj.int <- '2041-2070'
  scenario <- 'rcp85'
  
  proj.dir <-  '/home/data/scratch/ssobie/bccaq_gcm_van_whistler_subset/rcp85/snow/'
  for (gcm in gcm.list) {
    print(gcm)
    read.dir <- paste(proj.dir,gcm,'/',sep='')
    write.dir  <- paste(proj.dir,gcm,'/',sep='')
    if (!file.exists(write.dir))
      dir.create(write.dir)
    for (var.name in var.list) {
      print(var.name)
      all.files <- list.files(path=read.dir,pattern=paste(var.name,'_BCCAQ-PRISM',sep=''))
      past.file <- all.files[grep('1951-2100',all.files)]

      if (1==0) {
        ##For april data      
        pattern <- paste(var.name,sep='') ##,'_gcm',sep='')
        replacement <- paste(var.name,'_apr1',sep='')  
        write.past <- gsub(pattern=pattern,replacement=replacement,past.file)
        write.proj <- gsub(pattern=pattern,replacement=replacement,past.file)
        write.past <- gsub(pattern='1951-2100',replacement=past.int,write.past)
        write.proj <- gsub(pattern='1951-2100',replacement=proj.int,write.proj)
      }
        ##For may data
      april.files <- list.files(path=read.dir,pattern='apr1')
      april.past <- april.files[grep('1971-2000',april.files)]
      write.past <- gsub(pattern='apr1',replacement='may1',april.past)
      file.copy(from=paste(read.dir,april.past,sep=''),
                to=paste(write.dir,write.past,sep=''),overwrite=TRUE)
      april.proj <- april.files[grep(proj.int,april.files)]
      write.proj <- gsub(pattern='apr1',replacement='may1',april.proj)
      file.copy(from=paste(read.dir,april.proj,sep=''),
                to=paste(write.dir,write.proj,sep=''),overwrite=TRUE)      

      avg.sub.time(past.file,write.past,var.name,past.int,read.dir,write.dir)
      avg.sub.time(past.file,write.proj,var.name,proj.int,read.dir,write.dir)

    }

  }
}


