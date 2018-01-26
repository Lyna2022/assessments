##R Script to generate robjects needed for scatterplotting

source('/storage/home/ssobie/code/hg/pievc/scripts/extras_time_series_postgis_narccap.r',chdir=T)
source('/storage/home/ssobie/code/hg/pievc/spatial.r',chdir=T)
source('/storage/home/ssobie/code/hg/gcm.scatter/excel.scatterplot.prep.r',chdir=T)

library(ncdf4)
library(ncdf4.helpers)
library(rgdal)
library(rgeos)
library(udunits2)
library(PCICt)

##-------------------------------------------------
##Main extraction, clipping and plotting functions
test <- FALSE
if (test) {
  nc.fname <- '/home/data/climate/downscale/NARCCAP/BCCI/climdex_output/bcci/CCSM/historical/CRCM/tx90pETCCDI_yr_CCSM_bcci_historical_CRCM-CRCM_19710101-20701231.nc'
  var.name <- 'tx90pETCCDI'
  gcm.name <- 'ccsm'
  rcm.name <- 'crcm'
  scen <- 'sresa2'
  
  big.base.name <- '/home/ssobie/general/scatterplots/shapefiles/'
  shape.name <- 'bc_test_merge'  
  big.clip.shp <- readOGR(big.base.name,shape.name, stringsAsFactors=F)
  
  big.base.name <- '/home/data/projects/rci/moti/nrcan-precip_case_studies/shapefiles/'
  shape.name <- 'bella.coola'  
  big.clip.shp <- readOGR(big.base.name,shape.name, stringsAsFactors=F)
  
  date.names <- 1
}

find.continuous.coords <- function(coord.list) {
  lon.ix <- coord.list$xi
  lat.ix <- coord.list$yi
  ix.uni <- unique(lat.ix)

  lon.cols <- lapply(ix.uni,function(x) { return(lon.ix[lat.ix==x])})
  
}

netcdf.calendar <- function(nc) {

  time.calendar <- ncatt_get(nc,'time','calendar')$value
  time.units <- ncatt_get(nc, 'time', 'units')$value
  time.values <- ncvar_get(nc, 'time')
  if(grepl('days', time.units)) time.values <- time.values*86400
  if(grepl('hours', time.units)) time.values <- time.values*3600
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
  time.values <- origin.pcict + time.values
  return(time.values)  
}

##This clips the netcdf file to the provided shapefile
gcm.netcdf.climatologies <- function(nc.fname,var.name,gcm.name,rcm.name,clip.shp) {

  nc <- nc_open(nc.fname, readunlim=F)
  
  print(gcm.name)
  prj <- "+init=epsg:4326" ##"+proj=longlat +datum=WGS84 +no_defs"  ##

  nc.grid <- get.netcdf.grid(nc)

##  str(nc.grid)
  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  climatic.regions.prj <- spTransform(clip.shp, nc.grid@proj4string)
  ##i <- which(overlay(nc.grid, climatic.regions.prj) != "NA")

##  i <- which(over(nc.grid, climatic.regions.prj)[[7]]!= "NA")  
  i <- which(over(nc.grid, climatic.regions.prj)[[1]]!= "NA")  
  clipped <- nc.grid[i,]

  coord.list <- clipped@data
  contig <- find.continuous.columns(coord.list)
  spatial.coords <- clipped@coords
  ts <- netcdf.calendar(nc)

##  rm(nc.grid)
  
  data <- NULL

  for (region in contig){
    x0 <- coord.list[region,'xi'][1]
    y0 <- coord.list[region,'yi'][1]
    x.count <- length(region)

    new.data <- ncvar_get(nc, varid=var.name, start=c(x0, y0, 1), count=c(x.count, 1, -1))

    data <- rbind(data, new.data)
    rm(new.data)
  }
  my.units <- nc$var[[var.name]]$units

  dimnames(data) <- list(space=NULL, time=NULL)
  class(data) <- append(class(data), "narccap.var", after=0)
  attributes(data) <- append(attributes(data), list(gcm=gcm.name,
                                                    rcm=rcm.name,
                                                    var.name=var.name,                                                    
                                                    units=my.units,
                                                    p4s=prj,
                                                    spatial.coords=spatial.coords,
                                                    time.values=ts)
                             )
  nc_close(nc)

  rm(nc)
  return(data)
}

gcm.test.extract <- function(nc.fname,var.name,gcm.name,rcm.name,clip.shp) {

  nc <- nc_open(nc.fname, write=FALSE)
  prj <- "+init=epsg:4326"
  nc.grid <- get.netcdf.grid(nc)
  ts <- netcdf.calendar(nc)
##  str(nc.grid)
  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  climatic.regions.prj <- spTransform(clip.shp, nc.grid@proj4string)
##  i <- which(overlay(nc.grid, climatic.regions.prj) != "NA"
  i <- which(over(nc.grid, climatic.regions.prj)[[1]]!= "NA")
  clipped <- nc.grid[i,]
  coord.list <- clipped@data
  contig <- find.continuous.columns(coord.list)
  spatial.coords <- clipped@coords

#  rm(nc.grid)

  data <- NULL

  x.vals <- range(coord.list[,1])
  x0 <- x.vals[1]
  x.count <- diff(x.vals)+1

  y.vals <- range(coord.list[,2])
  y0 <- y.vals[1]
  y.count <- diff(y.vals)+1

  data <- ncvar_get(nc, varid=var.name, start=c(x0, y0, 1), count=c(x.count, y.count, -1))

  my.units <- nc$var[[var.name]]$units
  attr(data,'spatial.coords') <- spatial.coords
  attr(data,'time.values') <- ts
  attr(data,'gcm') <- gcm.name
  attr(data,'rcm') <- rcm.name
  attr(data,'var.name') <- var.name
  attr(data,'units') <- my.units
  attr(data,'p4s') <- prj
##browser()
  nc_close(nc)
  rm(nc)

  return(data)

}


##This clips the netcdf file to the provided shapefile
gcm.region.extract <- function(nc.fname,var.name,gcm.name,rcm.name,clip.shp) {

  nc <- nc_open(nc.fname, readunlim=F)

  prj <- "+init=epsg:4326"
  
  nc.grid <- get.netcdf.grid(nc)

##  str(nc.grid)
  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  climatic.regions.prj <- spTransform(clip.shp, nc.grid@proj4string)

  ##i <- which(overlay(nc.grid, climatic.regions.prj) != "NA")
  j <- which(over(nc.grid, climatic.regions.prj)[[1]]!= "NA")
  clipped <- nc.grid[j,]
  coord.list <- clipped@data
  spatial.coords <- clipped@coords
  ts <- netcdf.calendar(nc)

  lon.ix <- coord.list$xi
  lat.ix <- coord.list$yi
  ix.uni <- unique(lat.ix)
  lon.cols <- lapply(ix.uni,function(x) { return(lon.ix[lat.ix==x])})

  rm(nc.grid)
  data <- NULL
  for (i in 1:length(ix.uni)){
    x0 <- lon.cols[[i]][1]
    x.count <- diff(range(lon.cols[[i]]))+1
    y0 <- ix.uni[i]    

    full.list <- seq(from=x0,by=1,length.out=x.count)
    subset <- full.list %in% lon.cols[[i]]
    new.data <- ncvar_get(nc, varid=var.name, start=c(x0, y0, 1), count=c(x.count, 1, -1))

    if (length(dim(new.data)) > 1) {
      new.data <- new.data[subset,]
      data <- rbind(data, new.data)      
    } else {
      if (i==1 ) { #| grepl('(rp.)',var.name)) { ##|snowdepth
        data <- c(data, new.data[subset])
      } else {
        ###data <- c(data, new.data[subset]) ##Annual Values
        data <- rbind(data,new.data) ##Return Periods with CI values
      }
      ##new.data <- new.data##[subset]
    }
    ##browser()
    ##rm(new.data)
  }

  my.units <- nc$var[[var.name]]$units
  class(data) <- append(class(data), "narccap.var", after=0)
  attributes(data) <- append(attributes(data), list(gcm=gcm.name,
                                                    rcm=rcm.name,
                                                    var.name=var.name,                                                    
                                                    units=my.units,
                                                    p4s=prj,
                                                    spatial.coords=spatial.coords,
                                                    time.values=ts))

  if ('dimnames' %in% names(attributes(data))) {
    ix <- grep('dimnames',names(attributes(data)))
    attributes(data) <- attributes(data)[-ix]
  }
  nc_close(nc)
  
  rm(nc)
  return(data)
}


gcm.clipped.weighted.mean <- function(nc.data,inner.clip.shp) {
  
  spatial.coords <- attr(nc.data, "spatial.coords")
  p4s <- attr(nc.data, "p4s")
  my.raster <- SpatialPixelsDataFrame(points=SpatialPoints(spatial.coords, proj4string=CRS(p4s)),
                                      data=data.frame(field=nc.data),tolerance=0.002)
  my.raster <- as(my.raster, "SpatialGridDataFrame")
  ras <- raster(my.raster)

  shp.proj <- spTransform(inner.clip.shp, my.raster@proj4string)

  ## clip the raster with the shapefiles & return the coverage percent (to be used as a weight)
  intersected.ras <- rasterize(shp.proj, ras, getCover=T)
  intersected.sgdf <- as(intersected.ras, "SpatialGridDataFrame")
  ## get the original number of cells back from the spatialpixelsdataframe (no NA's)
  index <- which(!is.na(my.raster@data[,1]))
  ## return the weights of cells within the original spatialpixeldataframe
  weights <-  intersected.sgdf@data[,1][index]/100

  spatial.weighted.means <- apply(nc.data, 2, function(x) {weighted.mean(x, weights)})
  ##spatial.weighted.means <- apply(nc.data, 2, mean,na.rm=T)

  rv <- t(as.matrix(spatial.weighted.means, nrow=1))
  retain.attrs <- c('units', 'var.name','rcm', 'gcm','scenario','time')
  attributes(rv)[retain.attrs] <- attributes(nc.data)[retain.attrs]
  class(rv) <- append(class(rv), "narccap.var", after=0)
  rm(nc.data)
  return(rv)
}

##---------------------------------------------------------------------------
##CMIP3 Specific Functions
get.cmip3.scenario.files <- function(var.name,scenario,period) {

  past.all.files <- grab.cmip3.file.paths(var.name,scenario='20c3m',period)
  scenario.all.files <- grab.cmip3.file.paths(var.name,scenario,period)

  scenario.slct <- past.all.files$gcms %in% scenario.all.files$gcms

  if (length(past.all.files$gcms[scenario.slct]) != length(scenario.all.files$gcms)) {
    sub.slct <- scenario.all.files$gcms %in% past.all.files$gcms[scenario.slct]
    scenario.all.files$gcms <- scenario.all.files$gcms[sub.slct]
    scenario.all.files$files <- scenario.all.files$files[sub.slct]
  }
    
  scenario.files <- list(gcms=scenario.all.files$gcms,
                            past.files=past.all.files$files[scenario.slct],
                            future.files=scenario.all.files$files)

  return(scenario.files)
}

gcm.load.and.average <- function(var.name,scenario,type,scenario.files,interval,big.clip.shp,inner.clip.shp) {
  date.names <- c(tolower(month.abb),'win','spr','sum','aut','ann')
  if (scenario=='variability') date.names <- tolower(month.abb)
  if (type=='cmip5' & scenario=='historical') date.names <- tolower(month.abb)
  
  models <- scenario.files$gcms

  avg.tree <- c()  
  for (m in 1:length(models)) {
    ##print(m)
    model <- models[m]
    print(model)
    nc.fname <- scenario.files[[interval]][m]
    nc.data <- gcm.netcdf.climatologies(nc.fname,var.name,gcm.name=model,scen=scenario,big.clip.shp,date.names)
    if (var.name=='pr' & scenario=='variability') {
      nc.data <- ud.convert(nc.data,attr(nc.data,'units'),'kg m-2 month-1')
      attr(nc.data,'units') <- 'kg m-2 month-1'
    }
    ####TESTING
      nc.avg <- gcm.clipped.weighted.mean(nc.data,inner.clip.shp)
##    nc.avg <- testing.clipped.weighted.mean(nc.data,inner.clip.shp)
    avg.tree <- rbind(avg.tree,nc.avg)
    rm(nc.data,nc.avg)
  }
  avg.tree <- as.data.frame(avg.tree)
  names(avg.tree) <- date.names
  rownames(avg.tree) <- models
  return(avg.tree)
}

get.cmip3.ncdf <- function(var.name,scenario,period,big.clip.shp,inner.clip.shp) {
  
  var.past <- gcm.load.and.average(var.name,scenario='20c3m',type='cmip3',
                                   scenario.files=get.cmip3.scenario.files(var.name,scenario,period),
                                   interval='past.files',big.clip.shp,inner.clip.shp)
  
  var.future <- gcm.load.and.average(var.name,scenario,type='cmip3',
                                     scenario.files=get.cmip3.scenario.files(var.name,scenario,period),
                                     interval='future.files',big.clip.shp,inner.clip.shp)
  return(list(past=var.past,future=var.future))
}

compute.cmip3.values <- function(var.name,scenario,period,big.clip.shp,inner.clip.shp) {
  
  data  <- get.cmip3.ncdf(var.name,scenario,period,big.clip.shp,inner.clip.shp)

  if (scenario=='variability') {
    ##Create the seasonal columns
    seas <- list(djf=c(1,2,12),mam=c(3,4,5),jja=c(6,7,8),son=c(9,10,11),ann=1:12)
    seas.names <- c(tolower(month.abb),'win','spr','sum','aut','ann')
    rv <- as.data.frame(cbind(data$future, ##Not necessarily future variability
                              apply(data$future[seas$djf],1,mean,na.rm=T),
                              apply(data$future[seas$mam],1,mean,na.rm=T),
                              apply(data$future[seas$jja],1,mean,na.rm=T),
                              apply(data$future[seas$son],1,mean,na.rm=T),
                              apply(data$future[seas$ann],1,mean,na.rm=T)))
    names(rv) <- seas.names

  } else {
    if (var.name=='tas') rv  <- data$future-data$past
    if (var.name=='pr')  rv <- (data$future-data$past)/data$past*100
  }

  rm(data)
  
  return(rv)
}

get.area.shape <- function(region) {
  print('If this region is not within BC, this will not work!')
  big.base.name <- '/home/ssobie/general/scatterplots/shapefiles/'   ##'/home/data/gis/basedata/base_layers/'
  shape.name <- 'bc_test_merge'  ##'bc_province'
  big.clip.shp <- readOGR(big.base.name,shape.name, stringsAsFactors=F)

  inner.base.name <- switch(region,
                            lower_mainland='/home/data/projects/georgia_basin/sub_regions/',
                            crd='/home/data/projects/georgia_basin/sub_regions/',
                            cbt='/home/data/projects/cbt_cacci/',
                            ecodiv='/home/ssobie/general/scatterplots/shapefiles/',
                            ecoprov='/home/ssobie/general/scatterplots/shapefiles/',
                            fraser='/home/data/projects/forecasting_extremes/VIC_emulation/watersheds/')
  inner.shape.name <- switch(region,
                             lower_mainland='metro_vancouver',
                             crd='crd',
                             cbt='climatic_regions4_NO_USA_outer_lines',
                             ecodiv='bc_ecodivisions_land_only',
                             ecoprov='bc_ecoprovinces_land_only',
                             fraser='fraser_basin')

  inner.clip.shp <- readOGR(inner.base.name,inner.shape.name,stringsAsFactors=F)
  return(list(big=big.clip.shp,inner=inner.clip.shp))
  ##  inner.clip.shp <- readOGR(inner.base.name,'climatic_regions4_NO_USA_75km_buffer',stringsAsFactors=F)
}


compare.variable.lists <- function(y.values,x.values) {
  y.models <- rownames(y.values)
  x.models <- rownames(x.values)
  y.slct <- y.models %in% x.models
  x.slct <- x.models %in% y.models
  
  rv <- list(y.values=y.values[y.slct,],
             x.values=x.values[x.slct,])
  return(rv)
}


create.ds.anomaly.robject <- function(var.name,region,sub.region,period,interval) {

  shapes <- get.region.shape(region)
  big.clip.shp <- shapes$big
  inner.clip.shp <- shapes$inner
 
  cmip3.sresa2.tas <-  compute.cmip3.values('tas','sresa2',period,big.clip.shp,inner.clip.shp)
  cmip3.sresa1b.tas <-  compute.cmip3.values('tas','sresa1b',period,big.clip.shp,inner.clip.shp)
  cmip3.sresb1.tas <-  compute.cmip3.values('tas','sresb1',period,big.clip.shp,inner.clip.shp)

  cmip3.sresa2.pr <-  compute.cmip3.values('pr','sresa2',period,big.clip.shp,inner.clip.shp)
  cmip3.sresa1b.pr <-  compute.cmip3.values('pr','sresa1b',period,big.clip.shp,inner.clip.shp)
  cmip3.sresb1.pr <-  compute.cmip3.values('pr','sresb1',period,big.clip.shp,inner.clip.shp)

  values.sresa2  <- compare.variable.lists(cmip3.sresa2.tas,cmip3.sresa2.pr)
  values.sresa1b <- compare.variable.lists(cmip3.sresa1b.tas,cmip3.sresa1b.pr)
  values.sresb1  <- compare.variable.lists(cmip3.sresb1.tas,cmip3.sresb1.pr)
  cmip3.results <- list(sresa2=list(y.values=values.sresa2$y.values,x.values=values.sresa2$x.values),
                        sresa1b=list(y.values=values.sresa1b$y.values,x.values=values.sresa1b$x.values),
                        sresb1=list(y.values=values.sresb1$y.values,x.values=values.sresb1$x.values))
  
  save(cmip3.results,file=paste('/home/ssobie/general/scatterplots/anomaly.robjects/',region,'.',sub.region,'.',var.name,'.cmip3.anomalies.with.weights.',interval,'.Robject',sep=''))
   
}

##Create robjects with anomalies from the downscaled output
##Must supply 2 shapefiles!
create.ds.anom.objects <- function() { ##Generate plots for all months and seasons at once
  region <- 'bella.coola'
  sub.region <- region
 
  ##Takes awhile to run. Best to run once and save an Robject.
  create.ds.anomaly.robject(var.name='tas.pr',region,sub.region,period,interval)
  
}


##create.gcm.anom.objects()
