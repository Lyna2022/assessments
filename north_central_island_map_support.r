##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  return('North Central Vancouver Island')
}

get.region.names <- function(region) {
  return(list(area='north_central_island',subset='van_coastal_health',region='comox_valley_regional_district'))
}

get.leg.loc <- function(region) {
  return('bottomleft')
}

get.crop.box <- function(region) {
  return(c(-126.0,-124.5,49.3,50.5))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(1000,800))
}

##Set plot boundaries

make.plot.window <- function(bounds,region.shp) {

  xleft  <- 0.010
  xright <- -0.74
  ybot   <- 0.4
  ytop   <- -0.37

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
  lons <- c(-125.6,-125.4,-125.2,-125.0,-124.8,-124.6)
  lats <- c(  49.4,  49.8,  50.0,  50.2,  50.4,  50.6)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/north_central_island/'
  bc.common.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/'
  region.shp <- readOGR(shape.dir,'comox_valley_regional_district',stringsAsFactors=F, verbose=F)
  strath.shp <- readOGR(shape.dir,'strathcona_regional_district',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  coast.shp <- readOGR(bc.common.dir,'west_coast_ocean',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,'north_central_island_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR(shape.dir,'north_central_island_lakes',stringsAsFactors=F, verbose=F)
  reserves.shp <- readOGR(shape.dir,'CLAB_INRES_polygon',stringsAsFactors=F, verbose=F)

  plot(spTransform(coast.shp,CRS(crs)),add=TRUE,col='lightgray') ##'lightblue',border='lightblue')##'lightgray')  
  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(strath.shp,CRS(crs)),add=TRUE,lwd=2)
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=2)
  plot(spTransform(reserves.shp,CRS(crs)),add=TRUE,lwd=2)

}

add.cities <- function(crs,region) {
  ##Coordinates of cities to plot on the map

  city.coords <- list(
                     list(name='Campbell River',lon=-125.2733,lat=50.0331,xoffset=-0.01,yoffset=-0.05,        
                          xline=c(0,-0.01),yline=c(0.0,-0.03)),
                     list(name='Courtenay',lon=-124.9904,lat=49.6841,xoffset=-0.05,yoffset=-0.04,        
                          xline=c(0,-0.03),yline=c(0.0,-0.01)),
                     list(name='Comox',lon=-124.9283,lat=49.6735,xoffset=0.07,yoffset=-0.03,        
                          xline=c(0,0.05),yline=c(0.0,0.01)))
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
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.55,col='black',bg='white',r=0.1)
  }
}

add.districts <- function(crs,region) {

  district.coords <- list(
                         list(name='Comox Valley\nRegional District',lon=-125.3,lat=49.8,size=1.2),
                         list(name='Strathcona\nRegional District',lon=-125.4,lat=50.1,size=1.2))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      text(cx,cy,district.name,font=2,adj=4,pos=1,cex=district$size,col='black',bg='white')
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


   

