##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
get.projection <- function(region) {
  return("+init=epsg:3005")
}

get.region.title <- function(region) {
  return('Central Interior')
}

get.region.names <- function(region) {
  return(list(area='agriculture/central',subset='central',region='central'))
}

get.leg.loc <- function(region) {
  return('topright')
}

get.crop.box <- function(region) {
  return(c(-127.2,-118.0,52.9,56.4))
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(1200,1000))
}


##Set plot boundaries

make.plot.window <- function(bounds) {

  xleft  <- 0.036
  xright <- -0.016
  ybot   <- 0.07
  ytop   <- -0.07

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
  lats <- c(52,53,54,55,56,57,58)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/central/'
  region.shp <- readOGR(shape.dir,'central',stringsAsFactors=F, verbose=F)
  bulkley.shp <- readOGR(shape.dir,'bulkley_nechako',stringsAsFactors=F, verbose=F)
  ffg.shp <- readOGR(shape.dir,'fraser_fort_george',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles/','bc_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR('/storage/data/gis/basedata/base_layers/','bc_lakes',stringsAsFactors=F, verbose=F)

  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
  plot(spTransform(bulkley.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)
  plot(spTransform(ffg.shp,CRS(crs)),add=TRUE,lwd=2,lty=2)  
}

add.cities <- function(crs) {
  ##Coordinates of cities to plot on the map

  ##list(name='Chetwynd',lon=-121.6297,lat=55.6977,xoffset=-8000,yoffset=4000),
  ##list(name='Dawson Creek',lon=-120.2377,lat=55.7596,xoffset=-8000,yoffset=8000))

  city.coords <- list(
                     list(name='New Hazelton',lon=-127.5916,lat=55.2476,xoffset=-10000,yoffset=8000),
                     list(name='Houston',lon=-126.6482,lat=54.3980,xoffset=4000,yoffset=4000),
                     list(name='Telkwa',lon=-127.0476,lat=54.6950,xoffset=-4000,yoffset=-14000),
                     list(name='Vanderhoof',lon=-124.0130,lat=54.0140,xoffset=-10000,yoffset=4000),
                     list(name='Prince George',lon=-122.7497,lat=53.9171,xoffset=-1000,yoffset=4000),
                     list(name='Fort St. James',lon=-124.2510,lat=55.4426,xoffset=-22000,yoffset=4000),
                     list(name='Smithers',lon=-127.1686,lat=54.7824,xoffset=-2000,yoffset=4000),
                     list(name='Burns Lake',lon=-125.7636,lat=54.2334,xoffset=8000,yoffset=4000),
                     list(name='McBride',lon=-120.1685,lat=53.3013,xoffset=-2000,yoffset=4000),
                     list(name='Valemount',lon=-119.2643,lat=52.8312,xoffset=-2000,yoffset=4000))
                     ##list(name='Kitimat',lon=-128.6284,lat=54.0495,xoffset=4000,yoffset=-8000),                       

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
                         list(name='Bulkley - Nechako',lon=-125.5,lat=54.8),
                         list(name='Fraser - Fort\nGeorge',lon=-122.0,lat=54.5))

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


   

