##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.region.title <- function(region) {
  return('Kootenays')
}


##Set plot boundaries

make.plot.window <- function(bounds) {

  xleft  <- 0
  xright <- 0
  ybot   <- 0
  ytop   <- 0

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

  lons <- seq(-130,-114,2)
  lats <- seq(49,70,1)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

  ##Additional overlays to add specific to the region
add.plot.overlays <- function(crs) {

  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/kootenays/'
  nr.shp <- readOGR(shape.dir,'northern_rockies_muni',stringsAsFactors=F, verbose=F)
  tr.shp <- readOGR(shape.dir,'tumbler_ridge',stringsAsFactors=F, verbose=F)

  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
  plot(spTransform(nr.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)
  plot(spTransform(tr.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)

}


add.cities <- function(crs) {
  ##Coordinates of cities to plot on the map

  city.coords <- list(
                     list(name='Golden',lon=-116.9631,lat=51.2961,xoffset=0,yoffset=15000),
                     list(name='Invermere',lon=-116.0291,lat=50.5065,xoffset=0,yoffset=-15000),
                     list(name='Elkford',lon=-114.935,lat=50.0246,xoffset=-8000,yoffset=10000),
                     list(name='Sparwood',lon=-114.8797,lat=49.7345,xoffset=-8000,yoffset=0),
                     list(name='Fernie',lon=-115.0631,lat=49.5040,xoffset=-8000,yoffset=0),
                     list(name='Kimberley',lon=-115.9967,lat=49.6652,xoffset=-8000,yoffset=0),
                     list(name='Cranbrook',lon=-115.7694,lat=49.5130,xoffset=-8000,yoffset=0),
                     list(name='Nelson',lon=-117.2948,lat=49.4928,xoffset=-8000,yoffset=0),
                     list(name='Creston',lon=-116.5135,lat=49.0955,xoffset=-8000,yoffset=0),
                     list(name='Castlegar',lon=-117.6593,lat=49.3237,xoffset=-8000,yoffset=0),
                     list(name='Trail',lon=-117.7117,lat=49.0966,xoffset=-8000,yoffset=0),
                     list(name='Greenwood',lon=-118.6781,lat=49.0879,xoffset=-8000,yoffset=0),
                     list(name='Grand Forks',lon=-118.4451,lat=49.0301,xoffset=-8000,yoffset=0))
                       
  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      coords <- convert.to.alb.coords(city$lon,city$lat,alb.crs=CRS(crs))
      cx <- coords[1]
      cy <- coords[2]
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      points(cx,cy,pch=17,cex=2,col='black')
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.25,col='black',bg='white',r=0.1)
  }
}

add.districts <- function(crs) {

  district.coords <- list(
                         list(name='East Kootenay',lon=-115.5539,lat=50.0183),
                         list(name='Central Kootenay',lon=-117.4463,lat=49.8286),
                         list(name='Kootenay Boundary',lon=-118.7035,lat=49.3743))

  for (dc in seq_along(district.coords)) {
      district <- unclass(distric.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=CRS(crs))
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      text(cx,cy,district.name,font=2,adj=4,pos=1,cex=1.25,col='black',bg='white')
  }
}


##-------------------------------------------------------


   

