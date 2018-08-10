##Script to plot CRCM5 RCM data

library(ncdf4)
library(raster)
library(PCICt)

##--------------------------------------------------
##Coordinates

netcdf.calendar <- function(nc) {

  time.calendar <- ncatt_get(nc,'t','calendar')$value
  time.units <- ncatt_get(nc, 't', 'units')$value
  time.values <- ncvar_get(nc, 't')
  if(grepl('days', time.units)) time.values <- time.values*86400
  if(grepl('hours', time.units)) time.values <- time.values*3600
  origin.pcict <- as.PCICt(strsplit(time.units, ' ')[[1]][3],
                           cal=time.calendar)
  time.values <- origin.pcict + time.values
  return(time.values)
}


get.coords <- function(nc) {
  lat <- ncvar_get(nc, "lat")
  latp <- aperm(lat,c(2,1))
  lon <- ncvar_get(nc, "lon")
  lon <- ((lon + 180) %% 360) - 180
  lonp <- aperm(lon,c(2,1))

  i <- expand.grid(seq(nc$dim$x$len), seq(nc$dim$y$len))
  names(i) <- c("xi", "yi")
  xc <- as.vector(nc$dim$x$vals[i$xi])
  yc <- as.vector(nc$dim$y$vals[i$yi])
  ll <- as.vector(apply(i, 1, function(x) {latp[x[1], x[2]]}))
  ln <- as.vector(apply(i, 1, function(x) {lonp[x[1], x[2]]}))
  rv <- data.frame(lon=ln, lat=ll, xc=xc, yc=yc, i)
  return(rv)
}

read.crcm5.data <- function(nc,var.name) {

  prj <- "+init=epsg:4326" ##"+proj=longlat +datum=WGS84 +no_defs"  ##

  nc.grid <- get.coords(nc)

  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  coord.list <- nc.grid@data
  spatial.coords <- nc.grid@coords
  ts <- netcdf.calendar(nc)

  data2 <- ncvar_get(nc, varid=var.name,start=c(1,1,1,1),count=c(-1,-1,-1,1))-273
  data <- as.vector(aperm(ncvar_get(nc, varid=var.name,start=c(1,1,1,1),count=c(-1,-1,-1,1)),c(2,1)))-273

  my.units <- 'degC' ##nc$var[[var.name]]$units
  class(data) <- append(class(data), "narccap.var", after=0)
  attributes(data) <- append(attributes(data), list(rcm='CRCM5',
                                                    var.name='TASMAX',
                                                    units=my.units,
                                                    p4s=prj,
                                                    spatial.coords=spatial.coords,
                                                    time.values=ts))

  data <- unclass(data)

  ##Previous version for raster
  p4s <- prj
  my.points <- SpatialPoints(spatial.coords, proj4string=CRS(p4s))

##  my.raster <- SpatialPixelsDataFrame(points=SpatialPoints(spatial.coords, proj4string=CRS(p4s)),
##                                      data=data.frame(data),tolerance=5e-01)
  my.raster <- SpatialPixelsDataFrame(points=spatial.coords[1:3640,],
                                      data=data.frame(data[1:3640]),tolerance=0.98954)


  lon <- ncvar_get(nc,'lon')
  lat <- ncvar_get(nc,'lat')
  e <- extent(spatial.coords)

  rs <- raster(e,ncol=340,nrow=364)
  ts <- rasterize(spatial.coords,rs,aperm(data2,c(2,1)),fun=mean)
  ts@crs <- CRS("+init=epsg:4326")
  ##Reproject to North America Lambert Conformal Conic
  bc <- projectRaster(ts,crs=CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'"))
  plot(bc,xlim=c(-3.5e+06,0.75e+06),ylim=c(0.1e+06,4e+06))
  shape.dir <- '/storage/data/gis/basedata/base_layers/'
  library(rgdal)
  can.shp <- readOGR(shape.dir, 'north_america_state_provincial_boundaries', stringsAsFactors=F, verbose=F)
  plot(spTransform(can.shp,CRS("+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'")),add=TRUE)



browser()

  xv <- seq(e@xmin,e@xmax,length.out=364)
  yv <- seq(e@ymin,e@ymax,length.out=340)

  t.raster <- raster(data2)
  extent(t.raster) <- extent(spatial.coords)
  test.raster <- flip(t.raster,direction='y')
  test.raster@crs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  wg.raster <- projectRaster(test.raster,crs=CRS("+init=epsg:4326"))


  my.raster <- as(my.raster, "SpatialGridDataFrame")

  bounds <- extent(my.raster)
  xlim.min <- bounds@xmin
  xlim.max <- bounds@xmax
  ylim.min <- bounds@ymin
  ylim.max <- bounds@ymax

plot(my.raster,axes=T)

  browser()
}

rcm.file <- '/storage/data/climate/downscale/RCM/CRCM5/WC011_modified_T9_1980-2014.nc'
nc <- nc_open(rcm.file)
test <- read.crcm5.data(nc,'T9')

