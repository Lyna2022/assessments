##Script to plot the fraction of useable MODIS data
library(raster)
library(rgdal)
library(TeachingDemos)
library(maps)
library(scales)
        
##region.shp <- readOGR(shape.dir, region, stringsAsFactors=F, verbose=F)

site.dir <- '/storage/data/projects/rci/data/assessments/van_coastal_health/production/plots/'

##---------------------------------------------------------------------------------
##Make Plot

plot.file <- '/storage/data/projects/rci/data/assessments/van_coastal_health/production/plots/vch.area.map.png'
shape.dir <- '/storage/data/projects/rci/data/assessments/van_coastal_health/shapefiles/'

  plot.window.xlim <- c(-129.8,-122.0)
  plot.window.ylim <- c(48.85,53.7)

  png(file=plot.file,width=9,height=7,units='in',res=600,pointsize=6,bg='gray94')
  par(mar=c(6,6,6,5))
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='white',# 'lightgray',
     xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='',
     cex.axis=2.2,cex.lab=2.2,cex.main=2.4,mgp=c(3.5,2,0))
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='gray94')

     bc.dir <- '/storage/data/gis/basedata/base_layers/'
     bc.overlay <- 'bc_province_wgs84'
     bc.shp <- readOGR(bc.dir, bc.overlay, stringsAsFactors=F, verbose=F)
     us.shp <- readOGR(bc.dir, 'united_states', stringsAsFactors=F, verbose=F)

     shade <- raster(paste0(shape.dir,'vch_hillshade.tif'))
     image(shade,add=T,col = grey(1:100/100))

##     rivers.shp <- spTransform(readOGR(shape.dir, 'van_whistler_rivers', stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))  
##     lakes.shp <- spTransform(readOGR(shape.dir, 'van_whistler_lakes', stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))  

##     plot(lakes.shp,add=TRUE,col='lightsteelblue',border='lightblue',xlim=plot.window.xlim,ylim=plot.window.ylim)
##     plot(rivers.shp,add=TRUE,col='lightsteelblue',border='lightblue',xlim=plot.window.xlim,ylim=plot.window.ylim)

     ocean.shp <- readOGR('/storage/data/projects/rci/data/assessments/crd/shapefiles/','west_coast_ocean',stringsAsFactors=F, verbose=F)
     plot(spTransform(ocean.shp,CRS("+init=epsg:4326")),add=TRUE,col='lightsteelblue',border='lightsteelblue')
     plot(spTransform(ocean.shp,CRS("+init=epsg:4326")),add=TRUE,border='black')

     plot(spTransform(us.shp,CRS("+init=epsg:4326")),add=TRUE,border='black',lwd=2)

     vch.shp <- readOGR(shape.dir,'van_coastal_health',stringsAsFactors=F, verbose=F)
     richmond.shp <- readOGR(shape.dir,'richmond',stringsAsFactors=F, verbose=F)
     vancity.shp <- readOGR(shape.dir,'vancity',stringsAsFactors=F, verbose=F)
     plot(vch.shp,add=TRUE,lwd=2,col=alpha('red',0.2))
     plot(richmond.shp,add=TRUE,lwd=2,col=alpha('red',0.2))
     plot(vancity.shp,add=TRUE,lwd=2,col=alpha('red',0.2))

     rs <- 0.25
     points(-123.0678,49.3241,pch=18,cex=1.6,col='white')
     points(-123.0678,49.3241,pch=18,cex=1.4,col='black')
     shadowtext(-123.0678,49.3741,'Lionsgate\nHospital',adj=4,pos=3,cex=1.75,col='black',bg='white',r=rs)

     points(-128.1443,52.16786,pch=18,cex=1.6,col='white')
     points(-128.1443,52.16786,pch=18,cex=1.4,col='black')
     shadowtext(-128.1243,52.18786,'Bella Bella',adj=4,pos=3,cex=1.85,col='black',bg='white',r=rs)

  shadowtext(-123.5,50.35,'North Shore/\nCoast Garibaldi',adj=4,pos=3,cex=1.85,col='black',bg='white',r=rs)
  shadowtext(-123.65,49.25,'Vancouver',adj=4,pos=3,cex=1.85,col='black',bg='white',r=rs)
  shadowtext(-123.65,49.1,'Richmond',adj=4,pos=3,cex=1.85,col='black',bg='white',r=rs)

     box(which='plot',lwd=3)
 
if (1==0) {
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)     
v <- c( 0.8*v[2], v[2], 0.75*v[4], v[4] )
     bc.proj <- spTransform(bc.shp,CRS("+init=epsg:3005"))  
     snow.ex <- spTransform(readOGR(shape.dir, 'snow_extent', stringsAsFactors=F, verbose=F),CRS("+init=epsg:3005"))  
     bounds <- extent(bc.proj)
     xlim <- c(bounds@xmin,bounds@xmax)
     ylim <- c(bounds@ymin,bounds@ymax)

par( fig=v, new=TRUE, mar=c(0,0,0,0) )

  plot(c(),xlim=xlim,ylim=ylim,xaxs='i',yaxs='i',
     bg='gray94',# 'lightgray',
     xlab='',ylab='',main='',axes=F)
     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='gray94')
     plot(bc.proj,add=T,col='darkgray')
     plot(snow.ex,add=T,border='black',lwd=2)
     text(0.6*xlim[2],0.6*ylim[2],'British\nColumbia',cex=1.7)
     box(which='plot')

}
    dev.off()


