##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  return('Penticton Regional Airport')
}

get.region.names <- function(region) {
  return(list(area='okanagan/penticton_airport',subset='interior_health',region='penticton_airport'))
}

get.leg.loc <- function(region) {
  return('bottomright')
}

get.crop.box <- function(region) {
###  return(c(-120.0,-119.0,49.25,49.75))
  return(c(-119.69,-119.5,49.45,49.5))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(7,7))
}

##Set plot boundaries

make.plot.window <- function(region,bounds,region.shp) {

  xleft  <- 0.496
  xright <- -0.465
  ybot   <- 0.107
  ytop   <- -0.823

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
  rv <- list(xlim=c(-119.7,-119.5),ylim=c(49.4,49.53))
  return(rv)
}

add.graticules <- function(crs) {   

  lons <- c(-120.0,-119.9, -119.8, -119.65,-119.6,-119.55,-119.5,-119.3)
  lats <- c(49.3,49.35,49.40,49.45,49.50,49.55,49.6,49.65)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {

  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan'
  region.shp <- readOGR(shape.dir,'penticton_airport',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR(shape.dir,'penticton_airport_lakes',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,'penticton_airport_rivers',stringsAsFactors=F, verbose=F)
  oid.shp <- readOGR(shape.dir,'19050_CYYF_4kmrin_poly_epsg2955',stringsAsFactors=F, verbose=F)

  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue')
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=1.5)
  plot(spTransform(oid.shp,CRS(crs)),add=TRUE,lwd=1.25,lty=2)

}

add.cities <- function(crs,region) {
  ##Coordinates of cities to plot on the map

  city.coords <- list(
                     list(name='Summerland',lon=-119.6769,lat=49.6073,xoffset=-0.0275,yoffset=0.0,        
                          xline=c(0,-0.05),yline=c(0.0,-0.01)),
                     list(name='Naramata',lon=-119.593747,lat=49.595959,xoffset=0.023,yoffset=0.0,
                          xline=c(0,+0.025),yline=c(0.0,0.009)),
                     list(name='Okanagan Falls',lon=-119.572161,lat=49.345409,xoffset=0.0275,yoffset=0.005,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Penticton\nAirport',lon=-119.605275,lat=49.460288,xoffset=-0.015,yoffset=0,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Penticton',lon=-119.593291,lat=49.504,xoffset=0.001,yoffset=-0.005,
                          xline=c(0,0.01),yline=c(0.005,-0.015)))

  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      coords <- convert.to.alb.coords(city$lon,city$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      ##points(cx,cy,pch=17,cex=1.75,col='black')
      ##lines(city$lon+city$xline,city$lat+city$yline,col='white',lwd=3)
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.55,col='black',bg='white',r=0.1)
  }
}

add.districts <- function(crs,region) {

  district.coords <- list(
                         list(name='Okanagan\nLake',lon=-119.598,lat=49.5195,size=1.75),
                         list(name='Skaha\nLake',lon=-119.58,lat=49.41,size=1.75))

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


   

