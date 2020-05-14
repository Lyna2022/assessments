##Plotting script for the Kootenay Regional Districts

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

get.projection <- function(region) {
  return("+init=epsg:4326")
}

get.region.title <- function(region) {
  rv <- switch(region,
               vancouver_island='Vancouver Island',
               comox_valley='Comox Valley Regional District',
               comox_strathcona='Comox Valley and Strathcona Regional Districts', 
               cowichan_valley='Cowichan Valley Regional District',
               alberni_clayoquot='Alberni-Clayoquot Regional District',
               alberni_nanaimo='Alberni-Clayoquot and Nanaimo Regional Districts',
               capital_region='Capital Regional District',
               capital_cowichan='Capital and Cowichan Regional Districts',
               nanaimo_regional_district='Nanaimo Regional District')
  return(rv)
}

get.region.names <- function(region) {
  areas <- list(vancouver_island=list(area='vancouver_island/vancouver_island',subset='vancouver_island',region='vancouver_island'),
                comox_valley=list(area='vancouver_island/comox_valley',subset='vancouver_island',region='comox_valley'),
                comox_strathcona=list(area='vancouver_island/comox_strathcona',subset='vancouver_island',region='comox_strathcona'),
                cowichan_valley=list(area='vancouver_island/cowichan_valley',subset='vancouver_island',region='cowichan_valley'),
                alberni_clayoquot=list(area='vancouver_island/alberni_clayoquot',subset='vancouver_island',region='alberni_clayoquot'),
                alberni_nanaimo=list(area='vancouver_island/alberni_nanaimo',subset='vancouver_island',region='alberni_nanaimo'),
                capital_region=list(area='vancouver_island/capital_region',subset='vancouver_island',region='capital_region'),
                capital_cowichan=list(area='vancouver_island/capital_cowichan',subset='vancouver_island',region='capital_cowichan'),
                nanaimo_regional_district=list(area='vancouver_island/nanaimo_regional_district',subset='vancouver_island',region='nanaimo_regional_district'))
  rv <- areas[[region]]
  return(rv)
 
}

get.leg.loc <- function(region) {
  rv <- switch(region,
               vancouver_island='bottomleft',
               comox_valley='bottomleft',
               comox_strathcona='topright',
               cowichan_valley='topleft',
               alberni_clayoquot='bottomleft',
               alberni_nanaimo='bottomleft',
               capital_region='topleft',
               capital_cowichan='topright',
               nanaimo_regional_district='topright')
  return(rv)
}

get.crop.box <- function(region) {
  rv <- switch(region,
               vancouver_island=c(-128.25,-123.0,48.25,50.8),
               comox_valley=c(-125.6,-124.5,49.38,49.98),
               comox_strathcona=c(-126.2,-124.2,49.27,50.30),
               cowichan_valley=c(-124.99,-123.45,48.47,49.27),
               alberni_clayoquot=c(-126.65,-124.5,48.5,49.7),
               alberni_nanaimo=c(-126.65,-123.65,48.5,49.8),
               capital_region=c(-124.56,-123.0,48.28,49.04),
               capital_cowichan=c(-125.04,-123.0,48.26,49.37),
               nanaimo_regional_district=c(-125.05,-123.65,48.88,49.56))
  return(rv)
}

get.file.type <- function(region) {
  return('.png')
}

get.plot.size <- function(region) {
  return(c(8,6))
}

##Set plot boundaries

make.plot.window <- function(region,bounds,region.shp) {

  if (region=='vancouver_island') {
    xleft  <- 0.2
    xright <- -0.02
    ybot   <- 0.03
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
  } else {
    rv <- switch(region,
                 vancouver_island=list(xlim=c(-128.25,-123.0),ylim=c(48.25,50.8)),
                 comox_valley=list(xlim=c(-125.6,-124.5),ylim=c(49.38,49.98)),
                 comox_strathcona=list(xlim=c(-126.2,-124.2),ylim=c(49.27,50.30)),
                 cowichan_valley=list(xlim=c(-124.99,-123.45),ylim=c(48.47,49.27)),
                 alberni_clayoquot=list(xlim=c(-126.65,-124.5),ylim=c(48.5,49.7)), 
                 alberni_nanaimo=list(xlim=c(-126.65,-123.45),ylim=c(48.5,49.8)), 
                 capital_region=list(xlim=c(-124.56,-123.0),ylim=c(48.28,49.04)),
                 capital_cowichan=list(xlim=c(-125.04,-123.0),ylim=c(48.26,49.37)),
                 nanaimo_regional_district=list(xlim=c(-125.05,-123.65),ylim=c(48.88,49.56)))
  }
  return(rv)
}

add.graticules <- function(crs,region) {   

  lons <- c(-129.0,-128.0,-127.0, -126.0, -125.0,-124.0,-123.0,-122.0) *-1
  lats <- c(48.0,48.5,49.0,49.5,50.0,50.5,60.0,60.5)*-1

  grat <- graticule(lons, lats, proj = CRS(crs))
  labs <- graticule_labels(lons = lons, lats = lats, xline = -129, yline = 54, proj = CRS(crs))

  rv <- list(grat=grat,labs=labs,lons=lons,lats=lats)
  return(rv)
}

##Additional overlays to add specific to the region
add.plot.overlays <- function(crs,region) {

  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/vancouver_island'
  region.shp <- readOGR(shape.dir,region,stringsAsFactors=F, verbose=F)
##browser()  
  rivers.shp <- readOGR(shape.dir,'vi_rivers',stringsAsFactors=F, verbose=F)
  rds.shp <- readOGR(shape.dir,'vi_reg_districts',stringsAsFactors=F, verbose=F)
  common.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common'
  lakes.shp <- readOGR(common.dir,'bc_lakes',stringsAsFactors=F, verbose=F)
  ocean.mask.shp <- readOGR(common.dir,'ocean_mask',stringsAsFactors=F, verbose=F)
  bc.shp <- readOGR(common.dir,'bc',stringsAsFactors=F, verbose=F)

  plot(spTransform(ocean.mask.shp,CRS(crs)),add=TRUE,col='lightgray',border='lightgray')
  plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
  plot(spTransform(bc.shp,CRS(crs)),add=TRUE,lwd=1)
  plot(spTransform(rds.shp,CRS(crs)),add=TRUE,lwd=1,lty=2)
  plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=1)


}

add.cities <- function(crs,region) {
  ##Coordinates of cities to plot on the map

##                     list(name='Ladysmith',lon=-123.8161,lat=48.9953,xoffset=-0.25,yoffset=-0.01,
##                          xline=c(0,0.01),yline=c(0.005,-0.015)),
##                     list(name='Tofino',lon=-125.9066,lat=49.1530,xoffset=-0.16,yoffset=-0.07,
##                          xline=c(0,0.01),yline=c(0.005,-0.015)),
##                     list(name='Salt Spring Island',lon=-123.5089,lat=48.8167,xoffset=0.25,yoffset=0.05,
##                          xline=c(0,0.01),yline=c(0.005,-0.015)),
##                     list(name='Gabriola Island',lon=-123.7893,lat=49.1578,xoffset=0.27,yoffset=0.05,
##                          xline=c(0,0.01),yline=c(0.005,-0.015)))
##                     list(name='Sooke',lon=-123.7356,lat=48.3740,xoffset=0.0,yoffset=0.01,        
##                          xline=c(0,-0.05),yline=c(0.0,-0.01)),
##                     list(name='Sidney',lon=-123.3990,lat=48.6502,xoffset=0.0,yoffset=0.01,
##                          xline=c(0,0.01),yline=c(0.005,-0.015)),
  

  city.coords <- list(
                     list(name='Victoria',lon=-123.3656,lat=48.4284,xoffset=0.0,yoffset=0.01,
                          xline=c(0,+0.025),yline=c(0.0,0.009)),
                     list(name='Duncan',lon=-123.7079,lat=48.7787,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Nanaimo',lon=-123.9401,lat=49.1659,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Port Alberni',lon=-124.8055,lat=49.2339,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Courtenay',lon=-124.9904,lat=49.6841,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Campbell River',lon=-125.2733,lat=50.0331,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Sayward',lon=-125.9602,lat=50.3837,xoffset=0.0,yoffset=0.01,
                          xline=c(0,0.01),yline=c(0.005,-0.015)))

  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      coords <- convert.to.alb.coords(city$lon,city$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      points(cx,cy,pch=17,cex=1.75,col='black')
      ##lines(city$lon+city$xline,city$lat+city$yline,col='white',lwd=3)
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.25,col='black',bg='white',r=0.175)
  }
}

add.districts <- function(crs,region) {

  district.coords <- list(
                         list(name='Capital',lon=-123.965788,lat=48.57,size=1),
                         list(name='Cowichan\nValley',lon=-124.43,lat=48.87,size=1),
                         list(name='Comox\nValley',lon=-125.31,lat=49.8,size=1),
                         list(name='Strathcona',lon=-125.82,lat=50.0,size=1),
                         list(name='Mount Waddington',lon=-126.7,lat=50.4,size=1),
                         list(name='Nanaimo',lon=-124.31,lat=49.15,size=1),
                         list(name='Alberni\nClayoquot',lon=-125.51,lat=49.41,size=1))
                         

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name      
      text(cx,cy,district.name,font=3,adj=4,pos=1,cex=district$size,col='black',bg='white')
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


   

