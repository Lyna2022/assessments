##Script to map the downscaled output

##Plotting script for Capital Regional District

##------------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------------

reg.ds.maps <- function(box.data,region,region.range,box.range,
                        var.name,type,ds.type,region.shp,shp.buffer,
                        plot.file,plot.title,
                        coords=NULL,set.breaks=NULL,proj,
                        overlays=NULL,
                        leg.loc='topright',width=1000,height=800,
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
  xlim.adj <- (xlim.max - xlim.min) * 0.1 ##0.025
  ylim.adj <- (ylim.max - ylim.min) * 0.1 ##0.025 

  plot.window.xlim <- c((xlim.min + xlim.adj*2), (xlim.max - xlim.adj*2))
  plot.window.ylim <- c((ylim.min - ylim.adj), (ylim.max + ylim.adj))
  plot.window.ylim <- c((ylim.min + ylim.adj*2), (ylim.max - ylim.adj*2))

  e <- extent(c(plot.window.xlim,plot.window.ylim))

  map.range <- region.range
  print(map.range)
  
  if (!is.null(shared.range)) {
    map.range <- shared.range
  } else {
    map.range <- range(c(map.range,box.range))
  }

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


  ##-------------------------------------------------------------------------------------------------
  ##Add the region overlays to plot
  
  ##Overlays common to all regions    

  shape.dir <- '/storage/data/projects/rci/data/assessments/willow_road/shapefiles/'

  bc.overlay <- 'h_land_WGS84'
  bc.shp <- readOGR(shape.dir, bc.overlay, stringsAsFactors=F, verbose=F)
  rivers.shp <- readOGR(shape.dir,'willow_road_rivers',stringsAsFactors=F, verbose=F)
  lakes.shp <- readOGR(shape.dir,'willow_road_lakes',stringsAsFactors=F, verbose=F)
  road.shp <- readOGR(shape.dir,'willow_road_line',stringsAsFactors=F, verbose=F)
  buffer.shp <- spTransform(readOGR(shape.dir,'willow_road_buffer_line',stringsAsFactors=F, verbose=F),CRS("+init=epsg:4326"))
  bc.roads <- readOGR('/storage/data/gis/basedata/BC_Roads/','bc_hwy_geo',stringsAsFactors=F, verbose=F)

  plot(bc.shp,add=TRUE)
  ##plot(region.shp,add=TRUE,lwd=2,lty=2)
  plot(rivers.shp,add=TRUE,col='lightblue')
##  plot(lakes.shp,add=TRUE,col='lightblue')
  plot(bc.roads,add=TRUE,col='black',lwd=3)
  plot(road.shp,add=TRUE,col='black',lwd=5)
  plot(road.shp,add=TRUE,col='yellow',lwd=3)
  plot(buffer.shp,add=TRUE,col='black',lwd=3,lty=3)

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

  points(-122.7497,53.9168,pch=18,cex=1.9,col='white')
  points(-122.7497,53.9168,pch=18,cex=1.7,col='black')
  shadowtext(-122.7597,53.9568,'Prince George',adj=4,pos=3,cex=1.85,col='black',bg='white',r=0.1) 

  par(xpd=NA)
  legend(leg.loc, col = "black", legend=map.class.breaks.labels, pch=22, pt.bg = rev(colour.ramp),
         pt.cex=2.0, y.intersp=0.8, title.adj=0.2, title=my.label.units, xjust=0, cex=1.7)

  box(which='plot',lwd=3)

  dev.off()


}
##-------------------------------------------------------


   

