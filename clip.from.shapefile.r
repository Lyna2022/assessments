##R Script to generate robjects needed for scatterplotting

library(ncdf4)
library(rgdal)
library(rgeos)
library(PCICt)


get.netcdf.grid <- function(nc, degrees.east=T) {

  lat <- ncvar_get(nc, "lat")
  lon <- ncvar_get(nc, "lon")
  if (degrees.east) {
    lon <- ((lon + 180) %% 360) - 180
  }

  ## For NARCCAP like grids
  if ("xc" %in% names(nc$dim)) {
    i <- expand.grid(seq(nc$dim$xc$len), seq(nc$dim$yc$len))
    names(i) <- c("xi", "yi")
    xc <- as.vector(nc$dim$xc$vals[i$xi])
    yc <- as.vector(nc$dim$yc$vals[i$yi])
    ll <- as.vector(apply(i, 1, function(x) {lat[x[1], x[2]]}))
    ln <- as.vector(apply(i, 1, function(x) {lon[x[1], x[2]]}))
    return(data.frame(lon=ln, lat=ll, xc=xc, yc=yc, i))
  }
  ## For GCM grids
  else if (any(grepl("lon", names(nc$dim)))) {
    i <- expand.grid(seq(length(lon)), seq(length(lat)))
    names(i) <- c("xi", "yi")
    lon <- as.vector(lon[i$xi])
    lat <- as.vector(lat[i$yi])
    return(data.frame(lon=lon, lat=lat, i))
  }
  ## For other grids with projected coordinates (ex. NARR)
  else if ("x" %in% names(nc$dim)) {
    i <- expand.grid(seq(nc$dim$x$len), seq(nc$dim$y$len))
    names(i) <- c("xi", "yi")
    xc <- as.vector(nc$dim$x$vals[i$xi])
    yc <- as.vector(nc$dim$y$vals[i$yi])
    ll <- as.vector(apply(i, 1, function(x) {lat[x[1], x[2]]}))
    ln <- as.vector(apply(i, 1, function(x) {lon[x[1], x[2]]}))
    return(data.frame(lon=ln, lat=ll, xc=xc, yc=yc, i))
  }
}


##-------------------------------------------------
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

  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  climatic.regions.prj <- spTransform(clip.shp, nc.grid@proj4string)
  i <- which(over(nc.grid, climatic.regions.prj)[[1]]!= "NA")  
  clipped <- nc.grid[i,]

  coord.list <- clipped@data
  contig <- find.continuous.columns(coord.list)
  spatial.coords <- clipped@coords
  ts <- netcdf.calendar(nc)

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




##This clips the netcdf file to the provided shapefile
gcm.region.extract <- function(nc.fname,var.name,gcm.name,rcm.name,clip.shp) {

  nc <- nc_open(nc.fname, readunlim=F)

  prj <- "+init=epsg:4326"
  
  nc.grid <- get.netcdf.grid(nc)

  coordinates(nc.grid) <- c("lon", "lat")
  proj4string(nc.grid) <- CRS(prj)
  climatic.regions.prj <- spTransform(clip.shp, nc.grid@proj4string)

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
         if (grepl('rp',var.name)) {
            data <- rbind(data,new.data) ##Return Periods with CI values
         } else {
            data <- c(data, new.data[subset]) ##Annual Values
         }
      }
    }
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

  rv <- t(as.matrix(spatial.weighted.means, nrow=1))
  retain.attrs <- c('units', 'var.name','rcm', 'gcm','scenario','time')
  attributes(rv)[retain.attrs] <- attributes(nc.data)[retain.attrs]
  rm(nc.data)
  return(rv)
}


