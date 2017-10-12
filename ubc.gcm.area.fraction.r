##Script to get the areas covered by land and ocean in a gcm cell near UBC
  library(raster)
  library(ncdf4)
  library(rgdal)         

  source('~/code/hg/pievc/spatial.r')

  gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'HadGEM2-ES',
              'inmcm4',
              'MIROC5',
              'MRI-CGCM3')
  area.fraction <- rep(0,length(gcm.list))
  ##UBC
  coords <- c(-123.252562,49.262532)
  shape.dir <- '/storage/data/gis/basedata/base_layers/'
  shape.name <- 'bc_province_wgs84'
  clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)

  for ( g in seq_along(gcm.list)) {
    gcm <- gcm.list[g]
    nc.fname <- paste0('/storage/data/climate/downscale/BCCAQ2/CMIP5/global/annual/',gcm,'/test.nc')  
    ras <- raster(nc.fname)
    ras.points <- rasterToPoints(ras)

    ## clip the raster with the shapefiles & return the coverage percent (to be used as a weight)
    intersected.ras <- rasterize(clip.shp, ras, getCover=T)
    intersected.sgdf <- as(intersected.ras, "SpatialGridDataFrame")
    cell.ix <- which.min(sqrt( (coords[1] - ras.points[,1])^2 + (coords[2] - ras.points[,2])^2))
    area.fraction[g] <- intersected.sgdf@data[cell.ix,1]
  }

print(cbind(gcm.list,area.fraction))