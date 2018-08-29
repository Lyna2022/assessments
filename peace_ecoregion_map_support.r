##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.region.title <- function(region) {
  return('Northeast Lowlands')
}

get.region.names <- function(region) {
  return(list(area='northeast/peace_ecoregion',subset='northeast',region='peace_ecoregion'))
}

get.leg.loc <- function(region) {
  return('bottomleft')
}

get.crop.box <- function(region) {
  return(c(-122.5,-120.0,57.5,60))
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
  xright <- -0.016
  ybot   <- 0.05
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

add.graticules <- function(crs) {   
  lons <- c(-128,-126,-124,-122,-120,-118,-116)
  lats <- c(54,55,56,57,58,59,60)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/northeast/'
  region.shp <- readOGR(shape.dir,'peace_ecoregion',stringsAsFactors=F, verbose=F)
  nrrd.shp <- readOGR(shape.dir,'northern_rockies_muni',stringsAsFactors=F, verbose=F)
  tumbler.shp <- readOGR(shape.dir,'tumbler_ridge',stringsAsFactors=F, verbose=F)
  ##road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)

  ##plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
  plot(spTransform(nrrd.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)
  plot(spTransform(tumbler.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)  
}

add.cities <- function(crs) {
  ##Coordinates of cities to plot on the map

  city.coords <- list(
                     list(name='Fort St. John',lon=-120.84773,lat=56.25600,xoffset=0,yoffset=12000),
                     list(name='Fort Nelson',lon=-122.68722,lat=58.80452,xoffset=0,yoffset=8000),
                     list(name='Dawson Creek',lon=-120.23144,lat=55.76058,xoffset=-4000,yoffset=4000),
                     list(name='Chetwynd',lon=-121.62955,lat=55.70771,xoffset=-30000,yoffset=0),
                     list(name='Pouce Coupe',lon=-120.13359,lat=55.71034,xoffset=-2000,yoffset=-20000))

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
                         list(name='Northern Rockies Regional Municipality',lon=-122.649,lat=59.4),
                         list(name='Tumbler Ridge',lon=-120.994,lat=55.126))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      text(cx,cy,district.name,font=2,adj=4,pos=1,cex=1.75,col='black',bg='white')
  }
}


##-------------------------------------------------------


   

