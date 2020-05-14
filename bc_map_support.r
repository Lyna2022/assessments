##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:3005")
}

get.region.title <- function(region) {
  return('British Columbia')
}

get.region.names <- function(region) {
  return(list(area='bc',subset='bc',region='bc'))
}

get.leg.loc <- function(region) {
  return('topright')
}

get.crop.box <- function(region) {
  return(c(-140.0,-115.2,48.7,60.0))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(8,7))
}

##Set plot boundaries

make.plot.window <- function(bounds,region.shp) {

  xleft  <- 0.1
  xright <- -0.00
  ybot   <- 0.02
  ytop   <- -0.00

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

add.graticules <- function(lons,lats,crs) {

  xl <-  range(lons)
  yl <- range(lats)

  rv <- grat
  return(rv)
}


add.graticules <- function(crs) {   

  lons <- c(-145.0, -140.0,-135.0,-130.0,-125.0,-120.0,-115.0,-110.0)
  lats <- c(  48.0,  50.0,  52.0,  54.0,  56.0,  58.0,  60.0,  62.0)
  grat <- graticule(lons, lats, proj = CRS(crs),xlim=c(-145,-110),ylim=c(48,62))
  rv <- list(lons=lons,lats=lats,grats=grat)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/'
  region.shp <- readOGR(shape.dir,'h_land_WGS84',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,'h_rivers_WGS84',stringsAsFactors=F, verbose=F)
  ##lakes.shp <- readOGR(shape.dir,'',stringsAsFactors=F, verbose=F)

   wco.shp <- readOGR('/storage/data/projects/rci/data/assessments/shapefiles/bc_common',
                           'ocean_mask', stringsAsFactors=F, verbose=F)

##  plot(spTransform(coast.shp,CRS(crs)),add=TRUE,col='lightgray') ##'lightblue',border='lightblue')##'lightgray')  
  ##plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  ##plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  ##plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=1,col='gray')
  plot(spTransform(wco.shp,CRS(crs)),add=TRUE,col='lightgray',lwd=0.5)
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=0.75)


}

add.cities <- function(crs,region) {
  ##Coordinates of cities to plot on the map

  city.coords <- list(
                     list(name='Ucluelet',lon=-125.54606,lat=48.94201,xoffset=-0.05,yoffset=-0.03,        
                          xline=c(0,-0.05),yline=c(0.0,-0.01)),
                     list(name='Bamfield',lon=-125.14280,lat=48.83330,xoffset=0.06,yoffset=0,
                          xline=c(0,+0.025),yline=c(0.0,0.009)),
                     list(name='Salmon\nBeach',lon=-125.433827,lat=48.959508,xoffset=0,yoffset=-0.05,
                          xline=c(0,0.0),yline=c(0.0,-0.02)),   
                     list(name='Macoah',lon=-125.376,lat=48.99,xoffset=0.02,yoffset=-0.03,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Secret\nBeach',lon=-125.38,lat=49.01,xoffset=0,yoffset=0.02,
                          xline=c(0.01,0.0),yline=c(0.00,0.025)), 
                     list(name='Stuart\nBay',lon=-125.51,lat=48.93,xoffset=0,yoffset=-0.08,                                                    xline=c(0,0.0),yline=c(0.0,-0.04)))                    

  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      coords <- convert.to.alb.coords(city$lon,city$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      ##points(cx,cy,pch=17,cex=1.5,col='black')
      ##lines(city$lon+city$xline,city$lat+city$yline,col='white',lwd=3)
      ##shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.55,col='black',bg='white',r=0.1)
  }
}

add.districts <- function(crs,region) {

  district.coords <- list(
                         list(name='Community\nForest',lon=-125.52,lat=49.02,size=1),
                         list(name='Toquaht\nNation',lon=-125.34,lat=49.14,size=1.5))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      ##text(cx,cy,district.name,font=2,adj=4,pos=1,cex=district$size,col='black',bg='white')
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


   

