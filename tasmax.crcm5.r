##Script to plot CRCM5 RCM data

library(ncdf4)
library(raster)
library(PCICt)
library(graticule)
library(rgdal)

##--------------------------------------------------
##Coordinates

convert.to.nalcc.coords <- function(lon,lat,nalcc.crs) {

  d <- data.frame(lon=lon,lat=lat)
  coordinates(d) <- c('lon','lat')
  proj4string(d) <- CRS("+init=epsg:4326")
  d.albers <- spTransform(d,nalcc.crs)
  rv <- d.albers@coords
  return(rv)
}

##X-Axis Ticks
get.proj.xaxis <- function(lons,nalcc.crs,plot.window.ylim) {

  y <- seq(0,80,0.1)
  xm <- sapply(lons,rep,length(y))
  S <- apply(xm,2,function(x) {SpatialPoints(cbind(x,y), proj4string = CRS("+proj=longlat +datum=WGS84"))})
  S2<- lapply(S,spTransform, nalcc.crs)
  indices <- lapply(S2,function(x){which.min(abs(x@coords[,'y']-plot.window.ylim[1]))})
  xticks <- mapply(FUN=function(s,i){s@coords[,'x'][i]},S2,indices)
  return(xticks)
}


  ##Y-Axis Ticks
get.proj.yaxis <- function(lats,nalcc.crs,plot.window.xlim) {

  x <- seq(-180,-80,0.1)
  ym <- sapply(lats,rep,length(x))
  S <- apply(ym,2,function(y) {SpatialPoints(cbind(x,y), proj4string = CRS("+proj=longlat +datum=WGS84"))})
  S2<- lapply(S,spTransform, nalcc.crs)
  indices <- lapply(S2,function(x){which.min(abs(x@coords[,'x']-plot.window.xlim[1]))})
  yticks <- mapply(FUN=function(s,i){s@coords[,'y'][i]},S2,indices)
  return(yticks)
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


get.coords <- function(nc) {

  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  lon <- ((lon + 180) %% 360) - 180
  prj <- "+init=epsg:4326"

  i <- expand.grid(seq(nc$dim$rlon$len), seq(nc$dim$rlat$len))
  names(i) <- c("xi", "yi")
  xc <- as.vector(nc$dim$rlon$vals[i$xi])
  yc <- as.vector(nc$dim$rlat$vals[i$yi])
  ll <- as.vector(apply(i, 1, function(x) {lat[x[1], x[2]]}))
  ln <- as.vector(apply(i, 1, function(x) {lon[x[1], x[2]]}))
  nc.grid <- data.frame(lon=ln, lat=ll, xc=xc, yc=yc, i)
  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  spatial.coords <- nc.grid@coords
  return(spatial.coords)
}

make.raster.version <- function(nc,data) {

  spatial.coords <- get.coords(nc)
  ##Create empty raster
  e <- extent(spatial.coords)

  rs <- raster(e,ncol=nc$dim$rlon$len,nrow=nc$dim$rlat$len)
  ##Rasterize data matrix using gridded spatial coordinates
  ts <- rasterize(spatial.coords,rs,data,fun=mean)

  ##Assign WGS84 projection
  ts@crs <- CRS("+init=epsg:4326")

  ##Reproject to North America Lambert Conformal Conic
  nalcc.crs <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'")
  map.raster <- projectRaster(ts,crs=nalcc.crs)
print(extent(map.raster))
browser()
  return(map.raster)
}


##------------------------------------------
##Annual Average
annual.averages <- function(nc,var.name) {

  series <- netcdf.calendar(nc)

  hourly <- grepl('hours',nc$dim$t$units)
  daily <- grepl('days',nc$dim$t$units)
  yrs <- as.factor(format(series,'%Y'))

  ylen <- length(levels(yrs))
  tx.mean <- array(0,c(nc$dim$rlon$len,nc$dim$rlat$len,ylen))

  data <- ncvar_get(nc, varid=var.name,start=c(1,1,1),count=c(-1,-1,31))
  tx.avg <- apply(data,c(1,2),mean)
  plot.dir <- '/storage/data/projects/rci/data/RCM/CRCM5/plots/'

  make.crcm5.plot(tx.avg,nc,var.name,
                  plot.title='TX AVG',plot.file=paste0(plot.dir,'era.tx.avg.png'))





}




read.crcm5.data <- function(nc,var.name) {

  prj <- "+init=epsg:4326" ##"+proj=longlat +datum=WGS84 +no_defs"  ##

  spatial.coords <- get.coords(nc)

  series <- netcdf.calendar(nc)
  yrs <- as.factor(format(series,'%Y'))
  
  data2 <- ncvar_get(nc, varid=var.name,start=c(1,1,1,1),count=c(-1,-1,-1,1))##-273
browser()
  ##data2 <- apply(data,c(1,2),sum)
##  data1 <- ncvar_get(nc, varid=var.name,start=c(1,1,1,1),count=c(-1,-1,-1,36))-273

  my.units <- 'm/s' ##nc$var[[var.name]]$units

  ##Create empty raster
  e <- extent(spatial.coords)
  rs <- raster(e,ncol=nc$dim$y$len,nrow=nc$dim$x$len)
  ##Rasterize data matrix using gridded spatial coordinates
  ts <- rasterize(spatial.coords,rs,aperm(data2,c(2,1)),fun=mean)

  ##Assign WGS84 projection
  ts@crs <- CRS("+init=epsg:4326")

  ##Reproject to North America Lambert Conformal Conic
  nalcc.crs <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'")
  map.raster <- projectRaster(ts,crs=nalcc.crs)

  shape.dir <- '/storage/data/gis/basedata/base_layers/'
  bc.shp <- readOGR(shape.dir, 'bc_province', stringsAsFactors=F, verbose=F)
  bc.proj.shp <- spTransform(bc.shp,nalcc.crs)
##test <- rasterize(bc.proj.shp,map.raster,getCover=T)
##test[test==0] <- NA
##region.subset <- mask(map.raster,bc.proj.shp)
  
  bounds <- extent(map.raster)
  xlim.min <- bounds@xmin
  xlim.max <- bounds@xmax
  ylim.min <- bounds@ymin
  ylim.max <- bounds@ymax

  ##Set plot boundaries
  xlim.adj <- (xlim.max - xlim.min) * 0.3 ##0.025
  ylim.adj <- (ylim.max - ylim.min) * 0.2 ##0.025

  plot.window.xlim <- c((xlim.min + 1.01*xlim.adj), (xlim.max - xlim.adj))
  plot.window.ylim <- c((ylim.min + ylim.adj), (ylim.max - 1.5*ylim.adj))

##------------------------------------
##Start Plotting
  plot.title <- 'ERA-Interim TASMAX'      
  png('proj.png',width=800,height=800)
  par(mar=c(3,3,3,5))
  par(oma=c(1,1,0,0))
  par(mgp=c(2,1,0))
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
       bg='lightgray',axes=FALSE,
       xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main=plot.title,
       cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  plot(map.raster,add=TRUE)
  ###plot(region.subset,add=TRUE)

  lons <- c(-170.0,-150.0,-140.0,-130.0,-120.0,-110.0,-100.0,-90)
  lats <- c(35,40,45,50,55,60,65,70,75,80)
  grat <- graticule(lons, lats, proj = nalcc.crs)
  plot(grat,add=TRUE,lty=3,col='gray',lwd=2)

  shape.dir <- '/storage/data/gis/basedata/base_layers/'
  can.shp <- readOGR(shape.dir, 'north_america_state_provincial_boundaries', stringsAsFactors=F, verbose=F)
  plot(spTransform(can.shp,nalcc.crs),add=TRUE)

  xtks <- get.proj.xaxis(lons,nalcc.crs,plot.window.ylim)
  ytks <- get.proj.yaxis(lats,nalcc.crs,plot.window.xlim)

  axis(2,at=ytks,label=lats,cex.axis=1.5)
  axis(1,at=xtks,label=lons,cex.axis=1.5)
  box(which='plot',lwd=3)
  dev.off()
}


##Annual Average


make.crcm5.plot <- function(data,nc,var.name,
                            plot.title,plot.file) {

  map.raster <- make.raster.version(nc,data) 
  bounds <- extent(map.raster)

  xlim.min <- bounds@xmin
  xlim.max <- bounds@xmax
  ylim.min <- bounds@ymin
  ylim.max <- bounds@ymax

  ##Set plot boundaries
  xlim.adj <- (xlim.max - xlim.min) * 0.3 ##0.025
  ylim.adj <- (ylim.max - ylim.min) * 0.2 ##0.025

  plot.window.xlim <- c((xlim.min + 1.01*xlim.adj), (xlim.max - xlim.adj))
  plot.window.ylim <- c((ylim.min + ylim.adj), (ylim.max - 1.5*ylim.adj))

  nalcc.crs <- CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'")

##------------------------------------
##Start Plotting
  png(plot.file,width=800,height=800)
  par(mar=c(3,3,3,5))
  par(oma=c(1,1,0,0))
  par(mgp=c(2,1,0))
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='lightgray',axes=FALSE,
       xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main=plot.title,
       cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
  plot(map.raster,add=TRUE)

  lons <- c(-170.0,-150.0,-140.0,-130.0,-120.0,-110.0,-100.0,-90)
  lats <- c(35,40,45,50,55,60,65,70,75,80)
  grat <- graticule(lons, lats, proj = nalcc.crs)
  plot(grat,add=TRUE,lty=3,col='gray',lwd=2)

  shape.dir <- '/storage/data/gis/basedata/base_layers/'

  can.shp <- readOGR(shape.dir, 'north_america_state_provincial_boundaries', stringsAsFactors=F, verbose=F)
  plot(spTransform(can.shp,nalcc.crs),add=TRUE)

xtks <- get.proj.xaxis(lons,nalcc.crs,plot.window.ylim)
ytks <- get.proj.yaxis(lats,nalcc.crs,plot.window.xlim)

axis(2,at=ytks,label=lats,cex.axis=1.5)
axis(1,at=xtks,label=lons,cex.axis=1.5)
box(which='plot',lwd=3)
dev.off()

}



##**************************************************************************************************
##**************************************************************************************************
##rcm.file <- '/storage/data/climate/downscale/RCM/CRCM5/ERA-Interim/WC011_modified_T9_1980-2014.nc'
##rcm.file <- '/storage/data/climate/downscale/RCM/CRCM5/ERA-Interim/WC011_modified_PR_1980-2014.nc'
rcm.file <- '/local_temp/ssobie/crcm5/T9/tasmax_day_WC011_ERA-Interim+CRCM5_historical+rcp85_198001-20141231.nc'
nc <- nc_open(rcm.file)


test <- annual.averages(nc,var.name='tasmax')



