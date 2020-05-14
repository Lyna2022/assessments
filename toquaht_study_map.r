##Script to plot the fraction of useable MODIS data
library(raster)
library(rgdal)
library(TeachingDemos)
library(maps)

convert.to.alb.coords <- function(lon,lat,alb.crs="+init=epsg:3005") {

  d <- data.frame(lon=lon,lat=lat)
  coordinates(d) <- c('lon','lat')
  proj4string(d) <- CRS("+init=epsg:4326")
  d.albers <- spTransform(d,CRS(alb.crs))
  rv <- d.albers@coords
  return(rv)
}

        
##---------------------------------------------------------------------------------
##Make Plot

plot.file <- '/storage/data/projects/rci/data/assessments/toquaht/study_map/toquaht.study.area.map.png'
shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/toquaht/'

crs <- "+init=epsg:4326"


  plot.window.xlim <- c(-125.9,-124.9)
  plot.window.ylim <- c(48.69,49.2)

  width <- 1000 ##3926
  height <- 800 ##2383
##  pdf(file=plot.file,width=12,height=8,pointsize=3,bg='gray98')

  png(file=plot.file,width=12,height=8,units='in',res=300,pointsize=6,bg='white')
  ##png(file=plot.file,width=3600,height=1560,bg='white')
  par(mar=c(6,6,6,5))
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='white',# 'lightgray',
     xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='',
     cex.axis=2.2,cex.lab=2.2,cex.main=2.4,mgp=c(3.5,2,0))
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')

     shade <- raster(paste0('/storage/data/projects/rci/data/assessments/toquaht/study_map/','toquaht_hillshade.tif'),native=TRUE)
     shade.crop <- crop(shade,c(-125.9,-124.9,48.69,49.2))
     image(shade.crop,add=T,col = grey(1:100/100))

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

     coastal.shp <- readOGR(shape.dir,'toquaht_coastal',stringsAsFactors=F, verbose=F)
     mountains.shp <- readOGR(shape.dir,'toquaht_mountains',stringsAsFactors=F, verbose=F)

     plot(spTransform(coast.shp,CRS(crs)),add=TRUE,col='lightgray') ##'lightblue',border='lightblue')##'lightgray')
     plot(spTransform(land.shp,CRS(crs)),add=TRUE)
     plot(spTransform(forest.shp,CRS(crs)),add=TRUE,lwd=1,border='darkgreen')
     plot(spTransform(parcels.shp,CRS(crs)),add=TRUE,lwd=1,border='black')
     plot(spTransform(flands.shp,CRS(crs)),add=TRUE,lwd=1,border='black')
     plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
     plot(spTransform(hydro.shp,CRS(crs)),add=TRUE,col='darkblue',lwd=4)
     plot(spTransform(hydro.shp,CRS(crs)),add=TRUE,col='lightblue',lwd=2)
     ##  plot(spTransform(culture.shp,CRS(crs)),add=TRUE,col='white',pch=18,cex=1.3)
     ##  plot(spTransform(culture.shp,CRS(crs)),add=TRUE,col='black',pch=18)
     ##  plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
     plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=1,col='yellow')
     plot(spTransform(watershed.shp,CRS(crs)),add=TRUE,col='lightblue',border='darkblue',pch=18)
     plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=1,lty=2)
     plot(spTransform(mountains.shp,CRS(crs)),add=TRUE,lwd=2)
     plot(spTransform(coastal.shp,CRS(crs)),add=TRUE,lwd=2)


     city.coords <- list(
                     list(name='Ucluelet',lon=-125.54606,lat=48.94201,xoffset=-0.05,yoffset=-0.03,
                          xline=c(0,-0.05),yline=c(0.0,-0.01)),
                     list(name='Bamfield',lon=-125.14280,lat=48.83330,xoffset=0.06,yoffset=0,
                          xline=c(0,+0.025),yline=c(0.0,0.009)),
                     list(name='Salmon\nBeach',lon=-125.433827,lat=48.959508,xoffset=0,yoffset=-0.05,
                          xline=c(0,0.0),yline=c(0.0,-0.02)),
                     list(name='Macoah',lon=-125.376,lat=48.99,xoffset=0.02,yoffset=-0.03,
                          xline=c(0,0.01),yline=c(0.005,-0.015)),
                     list(name='Secret\nBeach',lon=-125.38,lat=49.01,xoffset=0.04,yoffset=-0.01,
                          xline=c(0.0,0.02),yline=c(0.000,-0.005)),
                     list(name='Stuart\nBay',lon=-125.51,lat=48.93,xoffset=0,yoffset=-0.08,                             
                          xline=c(0,0.0),yline=c(0.0,-0.04)))
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
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=1.15,col='black',bg='white',r=0.2)
  }

  district.coords <- list(
                         list(name='Community\nForest',lon=-125.52,lat=49.02,size=1),
                         list(name='Toquaht\nMountains',lon=-125.31,lat=49.14,size=1.5),
                         list(name='Toquaht\nCoastal',lon=-125.39,lat=49.08,size=1.5))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      coords <- convert.to.alb.coords(district$lon,district$lat,alb.crs=crs)
      cx <- coords[1]
      cy <- coords[2]
      district.name <- district$name
      shadowtext(cx,cy,district.name,font=2,adj=4,pos=1,cex=district$size,col='black',bg='white',r=0.2)
  }




     box(which='plot',lwd=3)
 
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)     
##v <- c( 0.8*v[2], v[2], 0.75*v[4], v[4] )
v <- c( v[1], 1.2*v[1], v[3], 1.3*v[3] )
     vi.shp <- readOGR('/storage/data/projects/rci/data/assessments/shapefiles/vancouver_island', 
                       'vancouver_island',stringsAsFactors=F, verbose=F)
     vi.proj <- spTransform(vi.shp,CRS(crs))  
     ##snow.ex <- spTransform(readOGR(shape.dir, 'snow_extent', stringsAsFactors=F, verbose=F),CRS(crs))  
     bounds <- extent(vi.proj)
     xlim <- c(bounds@xmin,bounds@xmax)
     ylim <- c(bounds@ymin,bounds@ymax)

par( fig=v, new=TRUE, mar=c(0,0,0,0) )

  plot(c(),xlim=xlim,ylim=ylim,xaxs='i',yaxs='i',
     bg='gray94',# 'lightgray',
     xlab='',ylab='',main='',axes=F)
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='gray94')
     plot(vi.proj,add=T,col='darkgray')
##     plot(snow.ex,add=T,border='black',lwd=2)
     text(0.6*xlim[2],0.6*ylim[2],'Vancouver\nIsland',cex=1.7)
     box(which='plot')
    dev.off()


