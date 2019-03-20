##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  return('Toquaht Traditional Territory')
}

get.region.names <- function(region) {
  return(list(area='toquaht',subset='toquaht',region='toquaht'))
}

get.leg.loc <- function(region) {
  return('bottomleft')
}

get.crop.box <- function(region) {
  return(c(-125.6,-125.2,48.7,49.11))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(1200,800))
}

##Set plot boundaries

make.plot.window <- function(bounds,region.shp) {

  xleft  <- 0.05
  xright <- -0.05
  ybot   <- 0.05
  ytop   <- -0.1

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
  lons <- c(-126.0,-125.8,-125.6,-125.4,-125.2,-125.0,-124.8)
  lats <- c(48.6,48.7,48.8,48.9,49.0,49.1,49.2)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/toquaht/'
  region.shp <- readOGR(shape.dir,'Toquaht_Traditional_Territory',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,'Watercourse',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR(shape.dir,'toquaht_lakes',stringsAsFactors=F, verbose=F)
  forest.shp <- readOGR(shape.dir,'Community Forest',stringsAsFactors=F, verbose=F)
  coast.shp <- readOGR(shape.dir,'toquaht_coast',stringsAsFactors=F, verbose=F)
  land.shp <- readOGR(shape.dir, 'toquaht_land', stringsAsFactors=F, verbose=F)
  watershed.shp <- readOGR(shape.dir, 'Community Watershed', stringsAsFactors=F, verbose=F)
  hydro.shp <- readOGR(shape.dir, 'hydro_line', stringsAsFactors=F, verbose=F)
  culture.shp <- readOGR(shape.dir, 'Cultural_SiteFeatures', stringsAsFactors=F, verbose=F)
  parcels.shp <- readOGR(shape.dir, 'Toquaht_Treaty_Parcels', stringsAsFactors=F, verbose=F)
  flands.shp <- readOGR(shape.dir, 'Toquaht_Future_Lands', stringsAsFactors=F, verbose=F)


  plot(spTransform(coast.shp,CRS(crs)),add=TRUE,col='lightgray') ##'lightblue',border='lightblue')##'lightgray')  
  plot(spTransform(land.shp,CRS(crs)),add=TRUE)
  plot(spTransform(forest.shp,CRS(crs)),add=TRUE,lwd=3,border='darkgray')
  plot(spTransform(parcels.shp,CRS(crs)),add=TRUE,lwd=2,border='black')
  plot(spTransform(flands.shp,CRS(crs)),add=TRUE,lwd=2,border='black')
  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(hydro.shp,CRS(crs)),add=TRUE,col='darkblue',lwd=4)
  plot(spTransform(hydro.shp,CRS(crs)),add=TRUE,col='lightblue',lwd=2)
##  plot(spTransform(culture.shp,CRS(crs)),add=TRUE,col='white',pch=18,cex=1.3)
##  plot(spTransform(culture.shp,CRS(crs)),add=TRUE,col='black',pch=18)
##  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(watershed.shp,CRS(crs)),add=TRUE,col='lightblue',border='darkblue',pch=18)
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)

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
      lines(city$lon+city$xline,city$lat+city$yline,col='white',lwd=3)
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.55,col='black',bg='white',r=0.1)
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


   

