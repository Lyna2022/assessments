##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  rv <- switch(region,
               okanagan='Greater Okanagan',
               central_okanagan='Central Okanagan',
               north_okanagan='North Okanagan',
               okanagan_similkameen='Okanagan-Similkameen')
  return(rv)
}

get.region.names <- function(region) {

  areas <- list(okanagan=list(area='okanagan/okanagan/',subset='interior_health',region='okanagan'),      
                central_okanagan=list(area='okanagan/central_okanagan',subset='interior_health',region='central_okanagan'),      
                north_okanagan=list(area='okanagan/north_okanagan',subset='interior_health',region='north_okanagan'),      
                okanagan_similkameen=list(area='okanagan/okanagan_similkameen',subset='interior_health',region='okanagan_similkameen'))
  rv <- areas[[region]]
  return(rv) 
}

get.leg.loc <- function(region) {
  return('topleft')
}

get.crop.box <- function(region) {
  rv <- switch(region,
               okanagan=c(-121.0,-117.0,49.0,51.0),
               central_okanagan=c(-120.0,-118.5,49.5,50.5),
               north_okanagan=c(-119.5,-117.0,49.8,51.0),
               okanagan_similkameen=c(-121.0,-119.0,49.0,50.0))
  return(rv)
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  rv <- switch(region,
               okanagan=c(1200,1000),
               central_okanagan=c(1200,800),
               north_okanagan=c(1200,1000),
               okanagan_similkameen=c(1200,800))
               
  return(rv)
}


##Set plot boundaries

make.plot.window <- function(bounds,region.shp) {
  ext <- extent(region.shp)

  off <- switch(region,
                okanagan=list(xleft=-0.1,xright=0.1,ybot=-0.01,ytop=0.1),
                central_okanagan=list(xleft=-0.1,xright=0.1,ybot=-0.1,ytop=0.11),
                north_okanagan=list(xleft=-0.1,xright=0.1,ybot=-0.1,ytop=0.1),
                okanagan_similkameen=list(xleft=-0.1,xright=0.1,ybot=-0.01,ytop=0.1))

  xleft <- off$xleft
  xright <- off$xright
  ybot <- off$ybot
  ytop <- off$ytop
  bounds <- ext
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
  ##browser()
  return(rv)
}

add.graticules <- function(crs) {   
  
  lons <- c(-122,-121,-120.5,-120,-119.5,-119,-118.5,-118.0)
  lats <- c(48.5,49,49.5,50,50.5,51,51.5,52.0)

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {
  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan/'
  electoral.shp <- readOGR(shape.dir,'RDCO_east_west',stringsAsFactors=F, verbose=F)
  
  region.shp <- readOGR(shape.dir,region,stringsAsFactors=F, verbose=F)
  road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles/','bc_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR('/storage/data/gis/basedata/base_layers/','bc_lakes',stringsAsFactors=F, verbose=F)

  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  if (region=='central_okanagan') {
     plot(spTransform(electoral.shp,CRS(crs)),add=TRUE,border='black',lwd=2,lty=2)
  }

  plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=2,col='gray')
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=4)
}

add.cities <- function(crs,region) {
  ##Coordinates of cities to plot on the map

  ##list(name='Chetwynd',lon=-121.6297,lat=55.6977,xoffset=-8000,yoffset=4000),
  ##list(name='Dawson Creek',lon=-120.2377,lat=55.7596,xoffset=-8000,yoffset=8000))

  central_okanagan.coords <- list(
                     list(name='Lake Country',lon=-119.4148,lat=50.0549,xoffset=0,yoffset=0.01),
                     list(name='Kelowna',lon=-119.4960,lat=49.8880,xoffset=0.08,yoffset=0.01),
                     list(name='West\nKelowna',lon=-119.5645,lat=49.8636,xoffset=-0.08,yoffset=0.01),
                     list(name='Peachland',lon=-119.7408,lat=49.7703,xoffset=0,yoffset=0.01),
                     list(name='Coldstream',lon=-119.1683,lat=50.2241,xoffset=0,yoffset=-0.03),
                     list(name='Lumby',lon=-118.9678,lat=50.2507,xoffset=0.0,yoffset=0.01),
                     list(name='Vernon',lon=-119.31,lat=50.261,xoffset=0.0,yoffset=0.01))

  north_okanagan.coords <- list(
                     list(name='Spallumcheen',lon=-119.2469,lat=50.4279,xoffset=0.07,yoffset=-0.04),
                     list(name='Armstrong',lon=-119.1970,lat=50.4477,xoffset=0.09,yoffset=0.01),
                     list(name='Enderby',lon=-119.1397,lat=50.5508,xoffset=0,yoffset=0.01),
                     list(name='Coldstream',lon=-119.1683,lat=50.2241,xoffset=0,yoffset=-0.05),
                     list(name='Lumby',lon=-118.9678,lat=50.2507,xoffset=0.0,yoffset=0.01),
                     list(name='Cherryville',lon=-118.6299,lat=50.2467,xoffset=0.0,yoffset=0.01),
                     list(name='Lake Country',lon=-119.4148,lat=50.0549,xoffset=0,yoffset=0.01),
                     list(name='Kelowna',lon=-119.4960,lat=49.8880,xoffset=0.09,yoffset=0.01),
                     list(name='West\nKelowna',lon=-119.5645,lat=49.8636,xoffset=-0.08,yoffset=0.01),
                     list(name='Peachland',lon=-119.7408,lat=49.7703,xoffset=0,yoffset=0.01),
                     list(name='Vernon',lon=-119.31,lat=50.261,xoffset=-0.02,yoffset=0.01),
                     list(name='Revelstoke',lon=-118.1957,lat=50.9981,xoffset=-0.02,yoffset=-0.08))


  okanagan.coords <- list(
                     list(name='Penticton',lon=-119.5937,lat=49.4991,xoffset=0,yoffset=-0.08),
                     list(name='Princeton',lon=-120.5062,lat=49.4590,xoffset=0,yoffset=0.01),
                     list(name='Keremeos',lon=-119.8295,lat=49.2025,xoffset=0,yoffset=0.01),
                     list(name='Oliver',lon=-119.5504,lat=49.1823,xoffset=0,yoffset=0.01),
                     list(name='Spallumcheen',lon=-119.2469,lat=50.4279,xoffset=0.17,yoffset=-0.08),
                     list(name='Armstrong',lon=-119.1970,lat=50.4477,xoffset=0.13,yoffset=0.01),
                     list(name='Enderby',lon=-119.1397,lat=50.5508,xoffset=0,yoffset=0.01),
                     list(name='Coldstream',lon=-119.1683,lat=50.2241,xoffset=0,yoffset=-0.08),
                     list(name='Lumby',lon=-118.9678,lat=50.2507,xoffset=0.04,yoffset=0.01),
                     list(name='Cherryville',lon=-118.6299,lat=50.2467,xoffset=0.04,yoffset=0.01),
                     list(name='Lake Country',lon=-119.4148,lat=50.0549,xoffset=0,yoffset=0.01),
                     list(name='Kelowna',lon=-119.4960,lat=49.8880,xoffset=0.12,yoffset=0.01),
                     list(name='West\nKelowna',lon=-119.5645,lat=49.8636,xoffset=-0.12,yoffset=0.01),
                     list(name='Peachland',lon=-119.7408,lat=49.7703,xoffset=0,yoffset=0.01),
                     list(name='Merritt',lon=-120.7862,lat=50.1113,xoffset=0,yoffset=0.02),
                     list(name='Kamloops',lon=-120.3273,lat=50.6745,xoffset=0,yoffset=0.02),
                     list(name='Vernon',lon=-119.31,lat=50.261,xoffset=-0.02,yoffset=0.01),
                     list(name='Revelstoke',lon=-118.1957,lat=50.9981,xoffset=-0.02,yoffset=-0.08),
                     list(name='Osoyoos',lon=-119.4682,lat=49.0323,xoffset=0.0,yoffset=0.01),
                     list(name='Summerland',lon=-119.6769,lat=49.6073,xoffset=-0.12,yoffset=0.01))                     

  okanagan_similkameen.coords <- list(
                     list(name='Penticton',lon=-119.5937,lat=49.4991,xoffset=0,yoffset=0.01),
                     list(name='Princeton',lon=-120.5062,lat=49.4590,xoffset=0,yoffset=0.01),
                     list(name='Keremeos',lon=-119.8295,lat=49.2025,xoffset=0,yoffset=0.01),
                     list(name='Oliver',lon=-119.5504,lat=49.1823,xoffset=0,yoffset=0.01),
                     list(name='Kelowna',lon=-119.4960,lat=49.8880,xoffset=0.12,yoffset=0.01),
                     list(name='West\nKelowna',lon=-119.5645,lat=49.8636,xoffset=-0.12,yoffset=0.01),
                     list(name='Peachland',lon=-119.7408,lat=49.7703,xoffset=0,yoffset=0.01),
                     list(name='Osoyoos',lon=-119.4682,lat=49.0323,xoffset=0.0,yoffset=0.01),
                     list(name='Summerland',lon=-119.6769,lat=49.6073,xoffset=-0.12,yoffset=0.01))                     

  city.coords <- switch(region,
                        central_okanagan=central_okanagan.coords,
                        okanagan=okanagan.coords,
                        north_okanagan=north_okanagan.coords,
                        okanagan_similkameen=okanagan_similkameen.coords)

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

add.districts <- function(crs,region=NULL) {

  district.coords <- list(
                         list(name='Central Okanagan\nWest Electoral Area',lon=-119.65,lat=50.04),
                         list(name='Central Okanagan\nEast Electoral Area',lon=-119.18,lat=49.96))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      if (region=='central_okanagan') {
         text(cx,cy,district.name,font=2,adj=4,pos=1,cex=1.2,col='black',bg='white')
      }
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


   

