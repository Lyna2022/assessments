##Script to map the downscaled output

##Copied from precip.scale.mapping.r and is meant to be a script that
##creates better maps specifically for the BC resource regions.

##----------------------------------------------------------------------

make.fixed.colour.ramps <- function(var.type) {

  ##Annual Precipitation
  pr.ramp <- c(rgb(11,44,122,maxColorValue=256),
               rgb(30,144,148,maxColorValue=256),
               rgb(14,196,65,maxColorValue=256),
               rgb(123,237,0,maxColorValue=256),
               rgb(247,215,7,maxColorValue=256),
               rgb(230,142,28,maxColorValue=256),
               rgb(194,82,60,maxColorValue=256))
  
  temp.ramp <- c(rgb(214,47,39,maxColorValue=256),
                 rgb(237,117,81,maxColorValue=256),
                 rgb(250,185,132,maxColorValue=256),
                 rgb(255,255,191,maxColorValue=256),
                 rgb(192,204,190,maxColorValue=256),
                 rgb(132,158,186,maxColorValue=256),
                 rgb(69,117,181,maxColorValue=256))

  colour.ramp <- switch(var.type,
                        pr=pr.ramp,
                        temp=temp.ramp,
                        fd=temp.ramp)
  
  return(colour.ramp)
  
}

convert.to.raster <- function(data) {
  data <- unclass(data)                  
  ##Previous version for raster
  p4s <- attr(data, "p4s")
  spatial.coords <- attr(data, "spatial.coords")
  my.raster <- SpatialPixelsDataFrame(points=SpatialPoints(spatial.coords, proj4string=CRS(p4s)),
                                      data=data.frame(data),tolerance=5e-05)
  my.raster <- as(my.raster, "SpatialGridDataFrame") 
  return(my.raster)
}

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

lm.ds.maps <- function(metro.data,metro.box,
                       cvrd.data,cvrd.box,
                       crd.data,crd.box,
                       bc.data,bc.box,
                       type,ds.type,
                       metro.shp,cvrd.shp,crd.shp,
                       plot.file,plot.title,proj=proj,
                       overlays=overlays,leg.loc=leg.loc,
                       shared.range=shared.range,shared.box=shared.box) {

                        
  metro.raster <- convert.to.raster(metro.data) 
  cvrd.raster <- convert.to.raster(cvrd.data) 
  crd.raster <- convert.to.raster(crd.data) 
  bc.raster <- convert.to.raster(bc.data) 

  ##----------------------------------  
  
  ##Metadata
  data <- unclass(metro.data)
  var.name <- attr(data, "var.name")
  gcm.name <- attr(data, "gcm")
  rcm.name <- attr(data, "rcm")
  p4s <- attr(data, "p4s")

  ##Metro Van Extent
  e <- extent(c(-124.866,-121.680,48.3,49.90))
  plot.window.xlim <- c(-124.866,-121.680)
  plot.window.ylim <- c(48.3,49.90)

  ##Set breakpoints for colour ramp
  
  map.range <- range(c(range(metro.raster@data,na.rm=TRUE),
                       range(cvrd.raster@data,na.rm=TRUE),
                       range(crd.raster@data,na.rm=TRUE),
                       range(bc.raster@data,na.rm=TRUE)))
  box.range <- range(c(range(metro.box@data@values,na.rm=TRUE),
                       range(cvrd.box@data@values,na.rm=TRUE), 
                       range(crd.box@data@values,na.rm=TRUE),
                       range(bc.box@data@values,na.rm=TRUE)))
                
  print(map.range)
  
  if (!is.null(shared.range))
    map.range <- shared.range
  class.breaks <- get.class.breaks(var.name,type,map.range,manual.breaks='')

  if (!is.null(shared.box))
    box.range <- shared.box

    class.breaks <- get.class.breaks(var.name,type,map.range,manual.breaks='')

    ##class.breaks <- c(seq(0,150,25),500) ##RP20 Precip
    ##class.breaks <- seq(10,40,5)
    colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=map.range,
                                        my.bp=0,class.breaks=class.breaks,
                                        type)    
                                          
  map.class.breaks.labels <- get.class.break.labels(class.breaks,greater.sign=T)
  print(map.class.breaks.labels)

  ##------------------------------------------------------------------------------------------------
  ##Fix the colour.ramp to include greater or less than breaks for the bounding
  ##box if needed
  ##Both
  ##Note: for ranges that are very narrow (i.e. 1 unit or less) or box.ranges and map.ranges that are very close, this
  ##doesn't work as well. The legend repeats intervals as this gets rounded similar values.
if(1==1) {
  if ((1==0) & (box.range[1] < map.range[1]) & (box.range[2] > map.range[2])) {
    dx <- diff(class.breaks)[1]
    class.breaks <- c(floor(box.range[1]/dx)*dx,class.breaks,ceiling(box.range[2]/dx)*dx)
    colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                        my.bp=0,class.breaks=class.breaks,
                                        type)        
    map.class.breaks.labels <- get.class.break.labels(class.breaks,lesser.sign=TRUE,greater.sign=TRUE)
  } else {  
    ##Greater than
    if (box.range[2] > map.range[2]) {
      dx <- diff(class.breaks)[1]
      class.breaks <- c(class.breaks,ceiling(box.range[2]/dx)*dx)
      colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                          my.bp=0,class.breaks=class.breaks,
                                          type)        
      map.class.breaks.labels <- get.class.break.labels(class.breaks,greater.sign=TRUE)
    }
    ##Less than
    if (box.range[1] < map.range[1]) {
      dx <- diff(class.breaks)[1]
      class.breaks <- c(floor(box.range[1]/dx)*dx,class.breaks)
      colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                          my.bp=0,class.breaks=class.breaks,
                                          type)        
      map.class.breaks.labels <- get.class.break.labels(class.breaks,lesser.sign=TRUE)
    }
  }
}  
  colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                      my.bp=0,class.breaks=class.breaks,
                                      type)

  ##------------------------------------------------------------------------------------------------

  ##Set up plot image
  png(file=plot.file,width=1400,height=900,bg='gray94') ##'gray94') ##,width=500,height=1100,bg='white')
  par(mar=c(6,6,7,6))    
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='white',# 'lightgray',
       xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main=plot.title,
       cex.axis=2,cex.lab=2,cex.main=2.2,mgp=c(3.5,2,0))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='lightsteelblue') ##'lightgray')

  shape.dir <- '/storage/data/projects/rci/data/assessments/bc/shapefiles/'  
  bc.overlay <- 'bc'
  bc.shp <- readOGR(shape.dir, bc.overlay, stringsAsFactors=F, verbose=F)  

  shape.dir <- '/storage/data/projects/rci/data/assessments/metro_van/shapefiles/'  
  us.shp <- readOGR(shape.dir,'pnw_provinces_states',stringsAsFactors=F, verbose=F)
  plot(spTransform(bc.shp,CRS(proj)),add=TRUE,xlim=plot.window.xlim,ylim=plot.window.ylim,col='lightgray')
  plot(spTransform(us.shp,CRS(proj)),add=TRUE,xlim=plot.window.xlim,ylim=plot.window.ylim,col='lightgray')
  image(bc.box, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE,alpha=0.3)
  image(metro.box, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE,alpha=0.3)
  image(cvrd.box, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE,alpha=0.3)
  image(crd.box, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE,alpha=0.3)
  ##Add the baselayers for the plot box

  bc.roads <- spTransform(get.region.shape('bc_hwy_geo','/storage/data/gis/basedata/BC_Roads/'),CRS(proj))

  ##Add the region raster data
  image(metro.raster, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE)  
  image(cvrd.raster, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE)  
  image(crd.raster, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE)  
  
  ##-------------------------------------------------------------------------------------------------
  ##Add the region overlays to plot
  coast.overlay <- 'h_coast_WGS84'

#  coast.shp <- readOGR(shape.dir, coast.overlay, stringsAsFactors=F, verbose=F)


#  rivers.shp <- readOGR(shape.dir,'h_rivers_WGS84',stringsAsFactors=F, verbose=F)
  ocean.shp <- readOGR('/storage/data/gis/basedata/base_layers','ocean_bc',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR('/storage/data/projects/rci/data/assessments/metro_van/shapefiles','watershed_lakes',stringsAsFactors=F, verbose=F)
  watershed.shp <- readOGR('/storage/data/projects/rci/data/assessments/metro_van/shapefiles','MVWaterSheds',stringsAsFactors=F, verbose=F)
  
##  plot(spTransform(coast.shp,CRS(proj)),add=TRUE,col='blue')

##  plot(spTransform(bc.shp,CRS(proj)),add=TRUE,xlim=plot.window.xlim,ylim=plot.window.ylim,col='lightgray')
  plot(spTransform(ocean.shp,CRS(proj)),add=TRUE,col='lightsteelblue',border='lightsteelblue',xlim=plot.window.xlim,ylim=plot.window.ylim)
##  plot(spTransform(rivers.shp,CRS(proj)),add=TRUE,col='lightblue',xlim=plot.window.xlim,ylim=plot.window.ylim)
  plot(spTransform(lakes.shp,CRS(proj)),add=TRUE,col='lightsteelblue',border='lightsteelblue',xlim=plot.window.xlim,ylim=plot.window.ylim)
  plot(spTransform(lakes.shp,CRS(proj)),add=TRUE,border='gray',xlim=plot.window.xlim,ylim=plot.window.ylim)
  lines(bc.roads,col='gray',lwd=3) 


  plot(spTransform(metro.shp,CRS(proj)),add=TRUE,lwd=2)
  plot(spTransform(cvrd.shp,CRS(proj)),add=TRUE,lwd=2)
  plot(spTransform(crd.shp,CRS(proj)),add=TRUE,lwd=2)


    cx <- -123.943
    cy <- 49.168
    points(cx,cy,pch=19,cex=2,col='black',bg='black')
    shadowtext(cx+0.01,cy-0.01,'Nanaimo',adj=4,pos=4,cex=2,col='black',bg='white',r=0.2)

    ##Vancouver
    points(-123.124,49.280,pch=19,cex=2,col='black')
    shadowtext(-123.124+0.01,49.280-0.01,'Vancouver',adj=4,pos=4,cex=2,col='black',bg='white',r=0.2)
    ##Victoria
    points(-123.367,48.426,pch=19,cex=2,col='black')
    shadowtext(-123.367+0.01,48.426-0.01,'Victoria',adj=4,pos=4,cex=2,col='black',bg='white',r=0.2)
    ##Duncan
    points(-123.707,48.779,pch=19,cex=2,col='black')
    shadowtext(-123.707+0.01,48.779-0.01,'Duncan',adj=4,pos=4,cex=2,col='black',bg='white',r=0.2)


  my.label.units <- leg.label.units(var.name,type)
  par(xpd=NA)
  legend(leg.loc, col = "black", legend=map.class.breaks.labels, pch=22, pt.bg = colour.ramp,
         pt.cex=2.0, y.intersp=0.8, title.adj=0.2, title=my.label.units, xjust=0, cex=1.7)
  box(which='plot',lwd=3)
  dev.off()

}
##-------------------------------------------------------


   

