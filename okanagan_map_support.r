##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  return('Okanagan')
}

get.region.names <- function(region) {
  return(list(area='okanagan',subset='interior_health',region='okanagan'))
}

get.leg.loc <- function(region) {
  return('topleft')
}

get.crop.box <- function(region) {
  return(c(-121.0,-117.0,49.0,51.0))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(1200,1000))
}


##Set plot boundaries

make.plot.window <- function(bounds) {

  xleft  <- 0.35
  xright <- -0.35
  ybot   <- 0.02
  ytop   <- -0.5

  xlim.min <- bounds@xmin
  xlim.max <- bounds@xmax
  ylim.min <- bounds@ymin
  ylim.max <- bounds@ymax

  ##Set plot boundaries
  xlim.adj <- (xlim.max - xlim.min)
  ylim.adj <- (ylim.max - ylim.min)
  plot.window.xlim <- c((xlim.min + xlim.adj*xleft), (xlim.max + xlim.adj*xright))
  plot.window.ylim <- c((ylim.min + ylim.adj*ybot),  (ylim.max + ylim.adj*ytop))
  rv <- list(xlim=plot.window.xlim,
             ylim=plot.window.ylim)
  return(rv)
}

add.graticules <- function(crs) {   
  lons <- c(-122,-121,-120,-119,-118,-117,-116)
  lats <- c(48.5,49,49.5,50,50.5,51,51.5)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan/'
  region.shp <- readOGR(shape.dir,'okanagan',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles/','bc_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR('/storage/data/gis/basedata/base_layers/','bc_lakes',stringsAsFactors=F, verbose=F)

  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
}

add.cities <- function(crs) {
  ##Coordinates of cities to plot on the map

  ##list(name='Chetwynd',lon=-121.6297,lat=55.6977,xoffset=-8000,yoffset=4000),
  ##list(name='Dawson Creek',lon=-120.2377,lat=55.7596,xoffset=-8000,yoffset=8000))

  city.coords <- list(
                     list(name='Nelson',lon=-117.2948,lat=49.4928,xoffset=0,yoffset=0.04),
                     list(name='Vernon',lon=-119.31,lat=50.261,xoffset=0,yoffset=0.04))

  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      coords <- convert.to.alb.coords(city$lon,city$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      points(cx,cy,pch=17,cex=1.5,col='black')
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.75,col='black',bg='white',r=0.1)
  }
}

add.districts <- function(crs) {

  district.coords <- list(
                         list(name='Interior Health',lon=-119.8,lat=51.435))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      ##text(cx,cy,district.name,font=2,adj=4,pos=1,cex=1.75,col='black',bg='white')
  }
}

get.title.info <- function(crs,plot.title) {
  ##Upper Title
  lower <- FALSE
  title.mar <- c(4.5,4.75,5.2,4)   
  upper.title <- plot.title
  lower.title <- ''

  ##lower <- TRUE
  ##title.mar <- c(6,4.75,4,4)   
  ##upper.title <- ''
  ##lower.title <- gsub('\n','',plot.title) 
  
  rv <- list(lower=lower,
             mar=title.mar,
             upper.title=upper.title,
             lower.title=lower.title)
  return(rv)
}
##-------------------------------------------------------


   

