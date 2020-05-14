##Script to map the downscaled output
library(graticule)

##Plotting script for maps in the BC Albers Projection

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

convert.to.alb.coords <- function(lon,lat,alb.crs="+init=epsg:3005") {
 
  d <- data.frame(lon=lon,lat=lat)
  coordinates(d) <- c('lon','lat')
  proj4string(d) <- CRS("+init=epsg:4326")
  d.albers <- spTransform(d,CRS(alb.crs))
  rv <- d.albers@coords
  return(rv)
}


##X-Axis Ticks
get.proj.xaxis <- function(lons,crs,plot.window.ylim) {

  y <- seq(0,80,0.1)
  xm <- sapply(lons,rep,length(y))
  S <- apply(xm,2,function(x) {SpatialPoints(cbind(x,y), proj4string = CRS("+proj=longlat +datum=WGS84"))})
  S2<- lapply(S,spTransform, crs)
  indices <- lapply(S2,function(x){which.min(abs(x@coords[,'y']-plot.window.ylim[1]))})
  xticks <- mapply(FUN=function(s,i){s@coords[,'x'][i]},S2,indices)
  return(xticks)
}

 ##Y-Axis Ticks
get.proj.yaxis <- function(lats,crs,plot.window.xlim) {

  x <- seq(-180,-80,0.1)
  ym <- sapply(lats,rep,length(x))
  S <- apply(ym,2,function(y) {SpatialPoints(cbind(x,y), proj4string = CRS("+proj=longlat +datum=WGS84"))})
  S2<- lapply(S,spTransform, crs)
  indices <- lapply(S2,function(x){which.min(abs(x@coords[,'x']-plot.window.xlim[1]))})
  yticks <- mapply(FUN=function(s,i){s@coords[,'y'][i]},S2,indices)
  return(yticks)
}


reg.ds.maps <- function(box.data,region,region.range,box.range,
                        var.name,type,ds.type,region.shp,
                        plot.file,plot.title,
                        make.plot.window=NULL,
                        set.breaks=NULL,
                        add.overlays=NULL,
                        add.cities=NULL,
                        add.districts=NULL,
                        add.graticules=NULL,
                        leg.loc='topright',width=1200,height=1000,                        
                        shared.range=NULL,shared.box=NULL,draft=TRUE) { 

  alb.crs <- get.projection(region) ##"+init=epsg:3005"

  box.data <-projectRaster(box.data,crs=CRS(alb.crs))
  bounds <- extent(box.data)

  plot.window <- make.plot.window(region,bounds,region.shp)

  white.box <- box.data - box.data
  white.box[is.na(white.box)] <- 0

  map.range <- region.range
  
  if (!is.null(shared.range))
    map.range <- shared.range

  if (!is.null(shared.box))
    box.range <- shared.box

  class.breaks <- get.class.breaks(var.name,type,map.range,manual.breaks='')

  if (!is.null(set.breaks)) {
    old.breaks <- get.class.breaks(var.name,type,map.range,manual.breaks='')
    class.breaks <- set.breaks
    colour.subset <- class.breaks %in% old.breaks
    colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=map.range,
                                        my.bp=0,class.breaks=class.breaks,
                                        type)    
  } else {
    class.breaks <- get.class.breaks(var.name,type,map.range,manual.breaks='')
    colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=map.range,
                                        my.bp=0,class.breaks=class.breaks,
                                        type)    
  }

  map.class.breaks.labels <- get.class.break.labels(class.breaks,type)

  ##------------------------------------------------------------------------------------------------
  ##Fix the colour.ramp to include greater or less than breaks for the bounding
  ##box if needed
  ##Both
  ##Note: for ranges that are very narrow (i.e. 1 unit or less) or box.ranges and map.ranges that are very close, this
  ##doesn't work as well. The legend repeats intervals as this gets rounded similar values.
  if ((box.range[1] < map.range[1]) & (box.range[2] > map.range[2]) & (class.breaks[1] != 0)) {
    dx <- diff(class.breaks)[1]
    class.breaks <- c(floor(box.range[1]/dx)*dx,class.breaks,ceiling(box.range[2]/dx)*dx)
    colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                        my.bp=0,class.breaks=class.breaks,
                                        type)        
    map.class.breaks.labels <- get.class.break.labels(class.breaks,type,lesser.sign=TRUE,greater.sign=TRUE)
  } else {  
    ##Greater than
    if (box.range[2] > map.range[2]) {
      dx <- diff(class.breaks)[1]
      class.breaks <- c(class.breaks,ceiling(box.range[2]/dx)*dx)
      colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                          my.bp=0,class.breaks=class.breaks,
                                          type)        
      map.class.breaks.labels <- get.class.break.labels(class.breaks,type,greater.sign=TRUE)
    }
    ##Less than
    if ((box.range[1] < map.range[1]) & (class.breaks[1] !=0) ) {
      dx <- diff(class.breaks)[1]
      class.breaks <- c(floor(box.range[1]/dx)*dx,class.breaks)
      colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                          my.bp=0,class.breaks=class.breaks,
                                          type)        
      map.class.breaks.labels <- get.class.break.labels(class.breaks,type,lesser.sign=TRUE)

    }
  }
  ##class.breaks <- c(0,25,50,75,100,150,200,250,300,350,1000) ##Past/Future
  ##class.breaks <- c(0,10,20,40,60,80,1000) ##Anoms
  ##class.breaks <- c(1000,1250,1500,1750,2000,2500,3000,3500,1000000)
  ##map.class.breaks.labels <- get.class.break.labels(class.breaks,type,lesser.sign=FALSE,greater.sign=TRUE)
  ##colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
  ##                                    my.bp=0,class.breaks=class.breaks,
  ##                                    type)        

  map.class.breaks.labels <- rev(map.class.breaks.labels)
  class.breaks <- class.breaks
  colour.ramp < rev(colour.ramp)

  grats <- add.graticules(alb.crs,region)
  file.type <- get.file.type(region)
  plot.size <- get.plot.size(region)
  width <- plot.size[1]
  height <- plot.size[2]
  ##------------------------------------------------------------------------------------------------

  ##Set up plot image
  if (grepl('pdf',file.type)) {
    pdf(file=plot.file,width=8,height=7,pointsize=6,bg='gray98')
  }
  if (grepl('tiff',file.type)) {
    tiff(file=plot.file,width=15,height=15,units='cm',res=150,bg='gray98')
  } 
  if (grepl('png',file.type)) {
   png(file=plot.file,width=width,height=height,units='in',res=600,pointsize=6,bg='white')
   ### png(file=plot.file,width=width,height=height,bg='gray98')
  }

  title.info <- get.title.info(alb.crs,plot.title)
  

###'Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='',
  par(mar=title.info$mar)    
  plot(c(),xlim=plot.window$xlim,ylim=plot.window$ylim,xaxs='i',yaxs='i',
     bg='white',axes=FALSE,
       xlab='',ylab='',###xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main='', ## 
       cex.axis=1.95,cex.lab=1.95,cex.main=1.95)
  ###title(main=strsplit(title.info$upper.title,'\n')[[1]][1],line=3.5,cex.main=1.95)
  ###title(main=strsplit(title.info$upper.title,'\n')[[1]][2],line=1.75,cex.main=1.95)
  ###title(main=strsplit(title.info$upper.title,'\n')[[1]][3],line=0.5,cex.main=1)
  grats <- add.graticules(alb.crs,region)
  xtks <- get.proj.xaxis(grats$lons,alb.crs,plot.window$ylim)
  ytks <- get.proj.yaxis(grats$lats,alb.crs,plot.window$xlim)
  ###axis(2,at=ytks,label=grats$lats,cex.axis=1.95)   
  ###axis(1,at=xtks,label=grats$lons,cex.axis=1.95)  
  
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='gray94')
  
  ##First plot the entire rectangular region with lighter transparency
  image(box.data, col=colour.ramp,breaks=class.breaks,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE)   

  ##-------------------------------------------------------------------------------------------------

  ##Add the region overlays to plot 
  ##Overlays common to all regions    

  shape.dir <- '/storage/data/projects/rci/data/assessments/shapefiles/bc_common/'

  bc.overlay <- 'h_land_WGS84'
  coast.overlay <- 'west_coast_ocean'
  rivers.overlay <- 'h_rivers_WGS84'
  us.overlay <- 'washington'
  bc.shp <- readOGR(shape.dir, bc.overlay, stringsAsFactors=F, verbose=F)
  coast.shp <- readOGR(shape.dir, coast.overlay, stringsAsFactors=F, verbose=F)
  us.shp <- readOGR(shape.dir,us.overlay,stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,rivers.overlay,stringsAsFactors=F, verbose=F)

  ##plot(spTransform(coast.shp,CRS(alb.crs)),add=TRUE,col='lightgray') ##'lightblue',border='lightblue')##'lightgray')                

##  plot(spTransform(bc.shp,CRS(alb.crs)),add=TRUE)
##plot(spTransform(rivers.shp,CRS(alb.crs)),add=TRUE,col='lightblue')

  ##Plot additional overlays if necessay
  add.plot.overlays(alb.crs,region)
  plot(spTransform(us.shp,CRS(alb.crs)),add=TRUE,lwd=1)

  ##Add the lon/lat lines
  plot(grats$grat,add=TRUE,lty=3,col='gray',lwd=0.75)

  if (draft) {
    text(x = grconvertX(0.5, from = "npc"),  # align to center of plot X axis
         y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
         labels = "DRAFT", # our watermark
         cex = 10, font = 2, # large, bold font - hard to miss
         col = rgb(1, 1, 1, .4), # translucent (0.2 = 20%) red color
         srt = 45) # srt = angle of text: 45 degree angle to X axis
  }
  
  ##------------------------------------------------------ 

  my.label.units <- leg.label.units(var.name,type)

  ##Functions to add city and regional district lines
  add.cities(alb.crs,region)
  add.districts(alb.crs,region)

  if (title.info$lower) {
     mtext(side=1,adj=0.5,line=4,title.info$lower.title,cex=0.9)
  }

  par(xpd=NA)
  legend(leg.loc, col = "black", bg='white',legend=map.class.breaks.labels, pch=22, pt.bg = rev(colour.ramp),
         pt.cex=1.95, y.intersp=0.8, title.adj=0.2, title=my.label.units, xjust=0, cex=1.95)

  if (region=='toquaht') {
    legend('topleft', legend=c('Toquaht Parcels','Community Forest','Traditional Territory'), 
                      lty=c(1,1,2),lwd=c(3,4,3),col=c('black','darkgray','black'),
                      pt.cex=1.95, y.intersp=0.8, title.adj=0.2, title='Regions', xjust=0, cex=1.95)
  }

  box(which='plot',lwd=2)

  dev.off()


}
##-------------------------------------------------------


   

