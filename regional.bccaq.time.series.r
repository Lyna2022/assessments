##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(rgdal)
library(rgeos)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=T)

read_ann_seas_data <- function(gcm.list,scenario,var.type,clip.shp,save.dir,shp.name) {

  read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/',scenario,'/',var.type,'/')
  var.list <- c('pr','tasmax','tasmin')
  for (var.name in var.list) {

     gcm.data <- vector(mode='list',length=length(gcm.list))
       
     for (g in seq_along(gcm.list)) {
        gcm <- gcm.list[g]
        var.files <- list.files(path=paste0(read.dir,gcm),pattern=var.name,full.name=TRUE)

        past.file <- var.files[grep('1951-2000',var.files)]
        past.nc <- nc_open(past.file)
        past.time <- netcdf.calendar(past.nc)
        nc_close(past.nc)

        proj.file <- var.files[grep('2001-2100',var.files)]
        proj.nc <- nc_open(proj.file)
        proj.time <- netcdf.calendar(proj.nc)
        nc_close(proj.nc)
  
        past.series <- apply(gcm.netcdf.climatologies(past.file,var.name,gcm,NULL,clip.shp),2,mean,na.rm=T)
        proj.series <- apply(gcm.netcdf.climatologies(proj.file,var.name,gcm,NULL,clip.shp),2,mean,na.rm=T)

        full.series <- c(past.series,proj.series)
        full.time <- c(past.time,proj.time)    
        rv <- list(time=full.time,series=full.series)
        gcm.data[[g]] <- rv
      }
      names(gcm.data) <- gcm.list
      save(gcm.data,file=paste0(save.dir,shp.name,'.',var.name,'.',var.type,'.',scenario,'.bccaq.series.RData'))
  }
}


##---------------------------------------------------------------------

read_degree_day_data <- function(gcm.list,scenario,var.type,clip.shp,save.dir,shp.name) {

  read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/',scenario,'/degree_days/')

  var.list <- c('cdd','fdd','gdd','hdd')

  for (var.name in var.list) {

     gcm.data <- vector(mode='list',length=length(gcm.list))
       
     for (g in seq_along(gcm.list)) {
        gcm <- gcm.list[g]
        dd.file <- list.files(path=paste0(read.dir,gcm),pattern=paste0(var.name,'_annual'),full.name=TRUE)

        dd.nc <- nc_open(dd.file)
        dd.time <- netcdf.calendar(dd.nc)
        nc_close(dd.nc)

        dd.series <- apply(gcm.netcdf.climatologies(dd.file,var.name,gcm,NULL,clip.shp),2,mean,na.rm=T)
        rv <- list(time=dd.time,series=dd.series)
        gcm.data[[g]] <- rv
     }
     names(gcm.data) <- gcm.list
     save(gcm.data,file=paste0(save.dir,shp.name,'.',var.name,'.degree.days.',scenario,'.bccaq.series.RData'))
  }
}


##---------------------------------------------------------------------

read_climdex_data <- function(gcm.list,scenario,var.type,clip.shp,save.dir,shp.name) {

  read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/',scenario,'/climdex/')

  var.list <- c('fdETCCDI','suETCCDI','idETCCDI','trETCCDI','gslETCCDI',
                 'txxETCCDI','txnETCCDI','tnnETCCDI','tnxETCCDI','dtrETCCDI',
                 'rx1dayETCCDI','rx5dayETCCDI',
                 'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI',
                 'cwdETCCDI','cddETCCDI',
                 'prcptotETCCDI','r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI')

  for (var.name in var.list) {
     print(var.name)
     gcm.data <- vector(mode='list',length=length(gcm.list))
       
     for (g in seq_along(gcm.list)) {
        gcm <- gcm.list[g]
        clim.file <- list.files(path=paste0(read.dir,gcm),pattern=paste0('^',var.name),full.name=TRUE)[1]

        clim.nc <- nc_open(clim.file)
        clim.time <- netcdf.calendar(clim.nc)
        nc_close(clim.nc)

        clim.series <- apply(gcm.netcdf.climatologies(clim.file,var.name,gcm,NULL,clip.shp),2,mean,na.rm=T)

        rv <- list(time=clim.time,series=clim.series)
        gcm.data[[g]] <- rv
     }
     names(gcm.data) <- gcm.list
     save(gcm.data,file=paste0(save.dir,shp.name,'.',var.name,'.climdex.',scenario,'.bccaq.series.RData'))
  }
}


##---------------------------------------------------------------------


rcp26.list <- c('CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-ES','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp45.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')
rcp85.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G','HadGEM2-CC','HadGEM2-ES','inmcm4','MIROC5','MPI-ESM-LR','MRI-CGCM3')

##---------------------------------------------------------------------
##PCDS Data
##
regions <- c('okanagan')

rd.file <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/RD_2011.shp'
shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan/'
save.dir <- '/storage/data/projects/rci/data/assessments/okanagan/okanagan_districts/series_files/'

rd.shp <- shapefile(rd.file)
rd.names <- rd.shp[[2]]

for (i in seq_along(regions)) {
   ##rd.ix <- which(toupper(rd.names) %in% regions[i])
   ##district <- rd.shp[rd.ix,]
   ##shp.name <- rd.names[i]
   district <- shapefile(paste0(shape.dir,regions[i]))
   shp.name <- regions[i]

   ##rcp26.data <- read_gcm_data(rcp26.list,'rcp26',var.name,var.type,district)
   ##save(rcp26.data,file=paste0(save.dir,shp.name,'.',var.name,'.rcp26.bccaq.series.RData'))
   ##rm(rcp26.data)

   ##rcp45.ann.data <- read_ann_seas_data(rcp45.list,'rcp45','annual',district,save.dir,shp.name)
   ##rcp45.seas.data <- read_ann_seas_data(rcp45.list,'rcp45','seasonal',district,save.dir,shp.name)
   ##rcp45.dd.data <- read_degree_day_data(rcp45.list,'rcp45','degree_days',district,save.dir,shp.name)
   ##rcp45.clim.data <- read_climdex_data(rcp45.list,'rcp45','climdex',district,save.dir,shp.name)

   rcp85.ann.data <- read_ann_seas_data(rcp85.list,'rcp85','annual',district,save.dir,shp.name)
   ##rcp85.seas.data <- read_ann_seas_data(rcp85.list,'rcp85','seasonal',district,save.dir,shp.name)
   ##rcp85.dd.data <- read_degree_day_data(rcp85.list,'rcp85','degree_days',district,save.dir,shp.name)
   ##rcp85.clim.data <- read_climdex_data(rcp85.list,'rcp85','climdex',district,save.dir,shp.name)

   
}

##

##  clip.shp <- readOGR('/storage/data/projects/rci/data/assessments/shapefiles/vancouver_island','vancouver_island', stringsAsFactors=F)

