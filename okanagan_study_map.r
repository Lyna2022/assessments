##Script to plot the fraction of useable MODIS data
library(raster)
library(rgdal)
library(TeachingDemos)
library(maps)

library(ncdf4)
        
##region.shp <- readOGR(shape.dir, region, stringsAsFactors=F, verbose=F)

nc <- nc_open('/storage/data/projects/rci/data/prism/bc_prism_dem_elevations.nc')
lon <- ncvar_get(nc,'lon')
lat <- ncvar_get(nc,'lat')

lon.st <- which.min(abs(-121.5-lon))
lon.en <- which.min(abs(-117.75-lon))
lat.st <- which.min(abs(48.90-lat))
lat.en <- which.min(abs(51.25-lat))

dem <- ncvar_get(nc,'elevation',start=c(lon.st,lat.st,1),
                                count=c(lon.en-lon.st+1,lat.en-lat.st+1,1))
nc_close(nc)
dem.range <- range(dem)

dem.breaks <- seq(0,2750,25)
dem.raster <- raster('/storage/data/projects/rci/data/prism/bc_prism_dem_elevations.nc')

colour.map <- function() {
  green.to.tan <- colorRampPalette(colors=c('#99FF99','#FFDD97'),bias=1, space = "Lab", interpolate = "linear")
  tan.to.brown <- colorRampPalette(colors=c('#FFDD97','#FFFF99','#C1934D'),bias=1, space = "Lab", interpolate = "linear")
  brown.to.purple <- colorRampPalette(colors=c('#C1934D','#CB9ED0'),bias=1, space = "Lab", interpolate = "linear")
  purple.to.white <- colorRampPalette(colors=c('#CB9ED0','#F7F5F7'),bias=1, space = "Lab", interpolate = "linear")

  rv <- c(green.to.tan(30),tan.to.brown(20),brown.to.purple(20),purple.to.white(20))
  return(rv)
}


##---------------------------------------------------------------------------------
##Make Plot
crs <- "+init=epsg:4326"
plot.file <- '/storage/data/projects/rci/data/assessments/okanagan/okanagan/okanagan.study.elev.map.png'
shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/okanagan/'

  plot.window.xlim <- c(-121.3,-117.8)
  plot.window.ylim <- c(48.90,51.2)

  png(file=plot.file,width=12,height=8.5,units='in',res=600,pointsize=6,bg='white')
  layout(matrix(c(1,1,1,1,1,1,1,1,2,3),nrow=2))
  par(mar=c(8,8,6,1))
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='white',# 'lightgray',
     xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='',
     cex.axis=3,cex.lab=3,cex.main=2.4,mgp=c(5,2.5,0))
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')

     bc.dir <- '/storage/data/gis/basedata/base_layers/'
     bc.overlay <- 'bc_province_wgs84'
     bc.shp <- readOGR(bc.dir, bc.overlay, stringsAsFactors=F, verbose=F)
     us.shp <- readOGR(bc.dir, 'united_states', stringsAsFactors=F, verbose=F)

     shade <- raster(paste0(shape.dir,'okanagan.tif'))
     image(shade,add=T,col = colour.map())    

     lons <- c(-122,-121,-120.5,-120,-119.5,-119,-118.5,-118.0)
     lats <- c(48.5,49,49.5,50,50.5,51,51.5,52.0)
     
     abline(h=lats,v=lons,lty=3,col='gray')

     electoral.shp <- readOGR(shape.dir,'co_electoral_areas',stringsAsFactors=F, verbose=F)
     reserves.shp <- readOGR(shape.dir,'westbank_first_nation',stringsAsFactors=F, verbose=F)

     region.shp <- readOGR(shape.dir,'okanagan',stringsAsFactors=F, verbose=F)
     road.shp <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)
     rivers.shp <- readOGR('/storage/data/projects/rci/data/assessments/bc/shapefiles/','bc_rivers',stringsAsFactors=F, verbose=F)
     lakes.shp <- readOGR('/storage/data/gis/basedata/base_layers/','bc_lakes',stringsAsFactors=F, verbose=F)

     plot(spTransform(lakes.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
     plot(spTransform(rivers.shp,CRS(crs)),add=TRUE,col='lightblue',border='lightblue')
     plot(spTransform(electoral.shp,CRS(crs)),add=TRUE,border='black',lwd=1,lty=2)
     plot(spTransform(reserves.shp,CRS(crs)),add=TRUE,border='black',lwd=1,lty=2)

     plot(spTransform(road.shp,CRS(crs)),add=TRUE,lwd=1.5,col='gray')
     plot(spTransform(region.shp,CRS(crs)),add=TRUE,lwd=2.2)

     plot(spTransform(us.shp,CRS("+init=epsg:4326")),add=TRUE,border='black',lwd=2)

     city.coords <- list(
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
  for (cc in seq_along(city.coords)) {
      city <- unclass(city.coords[[cc]])
      cx <- city$lon
      cy <- city$lat
      xoffset <- city$xoffset
      yoffset <- city$yoffset
      city.name <- city$name
      points(cx,cy,pch=17,cex=2,col='black')
      shadowtext(cx+xoffset,cy+yoffset,city.name,adj=4,pos=3,cex=3,col='black',bg='white',r=0.2)
  }
  district.coords <- list(
                         list(name='Central Okanagan\nWest Electoral Area',lon=-119.65,lat=50.04),
                         list(name='Central Okanagan\nEast Electoral Area',lon=-119.18,lat=49.96),
                         list(name='Westbank\nFirst Nation',lon=-119.46,lat=49.85))

  for (dc in seq_along(district.coords)) {
      district <- unclass(district.coords[[dc]])
      cx <- district$lon
      cy <- district$lat
      district.name <- district$name
      #text(cx,cy,district.name,font=2,adj=4,pos=1,cex=2.2,col='black',bg='white')
      #lines(c(-119.52,-119.6),c(49.827,49.84),col='black') ##Bottom Left
      #lines(c(-119.51,-119.53),c(49.841,49.88),col='black') ##Top Left
      #lines(c(-119.41,-119.33),c(49.841,49.844),col='black') ##Bottom Right
      #lines(c(-119.40,-119.275),c(49.827,49.828),col='black') ##Top Right

  }


     box(which='plot',lwd=2)
 
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)     

##    legend('bottomright',legend=c(col=colour.map(),cex=2,pch=15,title='Elev. (m)')


v <- c( 0.8*v[2], v[2], 0.75*v[4], v[4] )
##v <- c( v[2], 1.2*v[2], 0.75*v[4], v[4] )
     bc.proj <- spTransform(bc.shp,CRS("+init=epsg:3005"))  
     okanagan <- spTransform(readOGR(shape.dir, 'okanagan', stringsAsFactors=F, verbose=F),CRS("+init=epsg:3005"))  
     pnw.bnds <- spTransform(readOGR('/storage/data/projects/rci/data/assessments/shapefiles/bc_common/', 
                             'north_america_state_provincial_boundaries', stringsAsFactors=F, verbose=F),CRS("+init=epsg:3005"))  
     bounds <- extent(bc.proj)
     xlim <- c(bounds@xmin,bounds@xmax)
     ylim <- c(bounds@ymin,bounds@ymax)

##par( fig=v, new=TRUE, mar=c(0,0,0,0) )
  par(mar=c(22,0,6,3))
  plot(c(),xlim=xlim,ylim=ylim,xaxs='i',yaxs='i',
     bg='gray94',# 'lightgray',
     xlab='',ylab='',main='',axes=F)
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='lightblue')
     plot(pnw.bnds,add=T,col='darkgray')
     plot(okanagan,add=T,border='black',lwd=2)
     text(0.6*xlim[2],0.6*ylim[2],'British\nColumbia',cex=2.7,family='Helvetica')
     box(which='plot',lwd=2)

legend_image <- as.raster(matrix(rev(colour.map()), ncol=1))
  par(mar=c(1,1,1,1))
  plot(c(0,3),c(0,3),type = 'n', axes = F,xlab = '', ylab = '', main='',yaxs='i',xaxs='i')
  rect(0.03,0.35,1,2.26,border='black',lwd=2.5)
  text(x=0.5, y = 2.4, labels ='Elev. (m)',cex=3)
  text(x=1.3, y = c(0.4,1.3,2.2), labels =c('0   ','1375','2750'),cex=3)
  rasterImage(legend_image, 0.05, 0.36, 0.98,2.25)
##  box(which='plot')
  dev.off()