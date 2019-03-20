##Plotting script for Terrace

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  return('Terrace')
}

get.region.names <- function(region) {
  return(list(area='terrace',subset='terrace',region='terrace'))
}

get.leg.loc <- function(region) {
  return('topright')
}

get.crop.box <- function(region) {
  return(c(-131.0,-121.0,53.0,56.0))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(1200,1000))
}


##Set plot boundaries

make.plot.window <- function(bounds) {

  xleft  <- 0.1
  xright <- -0.2
  ybot   <- 0.2
  ytop   <- -0.2

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
  lons <- c(-131,-130,-129.0,-128.0,-128.0,-127.0,-126.0)
  lats <- c(53.5,54.0,54.5,54.5,54.5,55.0,55.5)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/terrace/'
  region.shp <- readOGR(shape.dir,'terrace',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles/','bc_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR('/storage/data/gis/basedata/base_layers/','bc_lakes',stringsAsFactors=F, verbose=F)

  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  ##plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
}

add.cities <- function(crs) {
  ##Coordinates of cities to plot on the map

  ##list(name='Chetwynd',lon=-121.6297,lat=55.6977,xoffset=-8000,yoffset=4000),
  ##list(name='Dawson Creek',lon=-120.2377,lat=55.7596,xoffset=-8000,yoffset=8000))

  city.coords <- list(
                     list(name='Terrace',lon=-128.6032,lat=54.5182,xoffset=0,yoffset=0.0),
                     list(name='Smithers',lon=-127.1686,lat=54.7824,xoffset=0,yoffset=0.0),
                     list(name='Kitimat',lon=-128.6284,lat=54.0494,xoffset=0,yoffset=0.0),
                     list(name='Prince Rupert',lon=-130.3208,lat=54.3150,xoffset=0,yoffset=0.0),
                     list(name='Vernon',lon=-119.296,lat=50.261,xoffset=0,yoffset=0.0))

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
                         list(name='Alaska',lon=-130.5,lat=55.325))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      text(cx,cy,district.name,font=2,adj=4,pos=1,cex=1.75,col='black',bg='white')
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


   

