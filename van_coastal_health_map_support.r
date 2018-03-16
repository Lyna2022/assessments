##Script to map the downscaled output

library(graticule)


##Plotting script for Capital Regional District

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

reg.ds.maps <- function(box.data,region,region.range,box.range,
                        var.name,type,ds.type,region.shp,shp.buffer,
                        plot.file,plot.title,
                        coords=NULL,set.breaks=NULL,proj,
                        overlays=NULL,
                        leg.loc='topright',width=1000,height=1000,
                        shared.range=NULL,shared.box=NULL,draft=TRUE) { ##width=1000,height=800) {

  wg.crs <- "+init=epsg:4326"

  box.data <-projectRaster(box.data,crs=CRS("+init=epsg:4326"))
  bounds <- extent(box.data)
  xlim.min <- bounds@xmin
  xlim.max <- bounds@xmax
  ylim.min <- bounds@ymin
  ylim.max <- bounds@ymax

  white.box <- box.data - box.data
  white.box[is.na(white.box)] <- 0
  
  ##Metadata
  ##Set plot boundaries
  xlim.adj <- (xlim.max - xlim.min) * 0.02 ##0.025
  ylim.adj <- (ylim.max - ylim.min) * 0.02 ##0.025 

  plot.window.xlim <- c((xlim.min + xlim.adj*1), (xlim.max - xlim.adj*1))
  plot.window.ylim <- c((ylim.min - ylim.adj), (ylim.max + ylim.adj))
  plot.window.ylim <- c((ylim.min + ylim.adj*1), (ylim.max - ylim.adj*1))

  e <- extent(c(plot.window.xlim,plot.window.ylim))

  map.range <- region.range
  print(map.range)
  
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

  map.class.breaks.labels <- get.class.break.labels(class.breaks)
  print(map.class.breaks.labels)

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
    if ((box.range[1] < map.range[1]) & (class.breaks[1] !=0) ) {
      dx <- diff(class.breaks)[1]
      class.breaks <- c(floor(box.range[1]/dx)*dx,class.breaks)
      colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                          my.bp=0,class.breaks=class.breaks,
                                          type)        
      map.class.breaks.labels <- get.class.break.labels(class.breaks,lesser.sign=TRUE)

    }
  }

  colour.ramp <- get.legend.colourbar(var.name=var.name,map.range=box.range,
                                      my.bp=0,class.breaks=class.breaks,
                                      type)        

  map.class.breaks.labels <- rev(map.class.breaks.labels)
  class.breaks <- rev(class.breaks)
  colour.ramp < rev(colour.ramp)

  ##------------------------------------------------------------------------------------------------

  ##Set up plot image
  png(file=plot.file,width=width,height=height,bg='gray94') ##,width=500,height=1100,bg='white')
  par(mar=c(6,6,7,6))    
  plot(c(),xlim=plot.window.xlim,ylim=plot.window.ylim,xaxs='i',yaxs='i',
     bg='lightgray',
       xlab='Longitude (\u00B0E)',ylab='Latitude (\u00B0N)',main=plot.title,
       cex.axis=2,cex.lab=2,cex.main=2.1)
##  axis(1,at=unclass(labs@coords)[1:7,1],label=lons,cex.axis=2)  
##  axis(2,at=unclass(labs@coords)[8:14,2],label=lats,cex.axis=2)  

  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='lightgray')
  
  ##First plot the entire rectangular region with lighter transparency
  image(box.data, col=colour.ramp,breaks=class.breaks,ribbon=FALSE,xlim=plot.window.xlim, ylim=plot.window.ylim, add=TRUE)   

  pos <- get.region.text.loc(region)

  ##-------------------------------------------------------------------------------------------------
  ##Add the region overlays to plot
  
  ##Overlays common to all regions    

  shape.dir <- '/storage/data/projects/rci/data/assessments/van_coastal_health/shapefiles/'

  bc.overlay <- 'h_land_WGS84'
  bc.shp <- readOGR(shape.dir, bc.overlay, stringsAsFactors=F, verbose=F)
  coast.shp <- readOGR(shape.dir, 'west_coast_ocean', stringsAsFactors=F, verbose=F)
  us.shp <- readOGR(shape.dir,'pnw_us_wgs84',stringsAsFactors=F, verbose=F)
  richmond.shp <- readOGR(shape.dir,'richmondhealth',stringsAsFactors=F, verbose=F)
  vancity.shp <- readOGR(shape.dir,'vanhealth',stringsAsFactors=F, verbose=F)
  bc.roads <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)

  plot(coast.shp,add=TRUE,col='lightgray')                
  plot(us.shp,add=TRUE,col='gray')
  plot(bc.shp,add=TRUE)
  plot(bc.roads,add=TRUE,col='gray')
  plot(region.shp,add=TRUE,lwd=2)
  plot(richmond.shp,add=TRUE,lwd=2)
  plot(vancity.shp,add=TRUE,lwd=2)


  if (draft) {
    text(x = grconvertX(0.5, from = "npc"),  # align to center of plot X axis
         y = grconvertY(0.5, from = "npc"), # align to center of plot Y axis
         labels = "DRAFT", # our watermark
         cex = 10, font = 2, # large, bold font - hard to miss
         col = rgb(1, 1, 1, .4), # translucent (0.2 = 20%) red color
         srt = 45) # srt = angle of text: 45 degree angle to X axis
  }
  
  ##------------------------------------------------------
  ##Plot the grid lines
#  v.cell <- my.raster@grid@cellsize[1]
#  h.cell <- my.raster@grid@cellsize[2]
#  abline(v=spatial.coords[,1]+v.cell/2,lty=2,col='black')
#  abline(h=spatial.coords[,2]+h.cell/2,lty=2,col='black')
  ##------------------------------------------------------ 

  my.label.units <- leg.label.units(var.name,type)
  
##  points(cx,cy,pch=17,cex=2,col='black')


  shadowtext(-123.5,50.4,'Vancouver\nCoastal',adj=4,pos=3,cex=1.5,col='black',bg='white',r=0.1)
  shadowtext(-123.24,49.27,'Vancouver Acute',adj=4,pos=2,cex=1.45,col='black',bg='white',r=0.1)
  shadowtext(-123.2,49.15,'Richmond',adj=4,pos=2,cex=1.45,col='black',bg='white',r=0.1)
  points(-123.0678,49.3241,pch=18,cex=2.4,col='white')
  points(-123.0678,49.3241,pch=18,cex=1.6,col='black')
  shadowtext(-123.0678,49.3241,'Lionsgate Hospital',adj=4,pos=4,cex=1.25,col='black',bg='white',r=0.1) 

  points(-124.5244,49.8353,pch=18,cex=2.4,col='white')
  points(-124.5244,49.8353,pch=18,cex=1.6,col='black')
  shadowtext(-124.8044,49.7353,'Powell\nRiver',adj=4,pos=3,cex=1.25,col='black',bg='white',r=0.1) 

  points(-122.9572,50.1233,pch=18,cex=2.4,col='white')
  points(-122.9572,50.1233,pch=18,cex=1.6,col='black')
  shadowtext(-122.9272,50.1233,'Whistler',adj=4,pos=4,cex=1.25,col='black',bg='white',r=0.1) 

  points(-123.1563,49.7013,pch=18,cex=2.4,col='white')
  points(-123.1563,49.7013,pch=18,cex=1.6,col='black')
  shadowtext(-123.1263,49.7013,'Squamish',adj=4,pos=4,cex=1.25,col='black',bg='white',r=0.1) 

  points(-122.8045,50.3222,pch=18,cex=2.4,col='white')
  points(-122.8045,50.3222,pch=18,cex=1.6,col='black')
  shadowtext(-122.7345,50.3222,'Pemberton',adj=4,pos=4,cex=1.25,col='black',bg='white',r=0.1) 

  ##Richmond
##  points(-123.1468,49.1689,pch=18,cex=2.4,col='white')
##  points(-123.1468,49.1689,pch=18,cex=1.6,col='black')

  ##Sechelt
  points(-123.7545,49.4742,pch=18,cex=2.4,col='white')
  points(-123.7545,49.4742,pch=18,cex=1.6,col='black')
  shadowtext(-123.7545,49.40,'Sechelt',adj=4,pos=2,cex=1.25,col='black',bg='white',r=0.1) 

  par(xpd=NA)
  legend(leg.loc, col = "black", legend=map.class.breaks.labels, pch=22, pt.bg = rev(colour.ramp),
         pt.cex=2.0, y.intersp=0.8, title.adj=0.2, title=my.label.units, xjust=0, cex=1.7)

  box(which='plot',lwd=3)

  dev.off()

}
##-------------------------------------------------------


   

