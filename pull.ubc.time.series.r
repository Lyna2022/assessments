##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)

extract.series <- function(read.dir,gcm,var.name,lon.ix,lat.ix) {

    file.names <- list.files(path=paste0(read.dir,gcm),pattern=var.name,full.name=TRUE) 
    past.file <- file.names[grep('1951-2000',file.names)]
    proj.file <- file.names[grep('2001-2100',file.names)]
    past.nc <- nc_open(past.file)
    proj.nc <- nc_open(proj.file)
    
    data.past <- ncvar_get(past.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.proj <- ncvar_get(proj.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    data.sub <- c(data.past,data.proj)
    return(data.sum)    
}

read.dir <- '/storage/data/scratch/ssobie/bccaq_gcm_bc_subset/'
write.dir <- ''

coords <- c(-123.252562,49.262532)
var.names <- c('pr','tasmax','tasmin')

gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CNRM-CM5')

for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    file.names <- list.files(path=paste0(read.dir,gcm),pattern='pr',full.name=TRUE) 
    past.file <- file.names[grep('1951-2000',file.names)]
    past.nc <- nc_open(past.file)
    lon <- ncvar_get(past.nc,'lon')
    lon.ix <- which.min(abs(lon-coords[1]))
    lat <- ncvar_get(past.nc,'lat')
    lat.ix <- which.min(abs(lat-coords[2]))
    
    pr.data <- extract.series(read.dir,gcm,'pr',lon.ix,lat.ix)
    tasmax.data <- extract.series(read.dir,gcm,'tasmax',lon.ix,lat.ix)
    tasmin.data <- extract.series(read.dir,gcm,'tasmin',lon.ix,lat.ix)

    browser()
}


