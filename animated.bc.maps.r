##Script to create animated maps of mean temperature from BCCAQ PCIC12 Ensemble

library(rgdal)
library(raster)
library(rgeos)
library(fields)
library(graticule)
library(animation)

source('/storage/home/ssobie/code/repos/assessments/resource.region.map.support.r',chdir=T)


##------------------------------------

plot.bc.tas <- function(tas.sub,title) {

  lons <- c(-140,-130,-120,-110)
  lats <- c(0,40,49,60,75,90)
  grat <- graticule(lons, lats, proj = bc.albers.crs)
  labs <- graticule_labels(lons,lats,proj=bc.albers.crs)

  plot.window.xlim <- c(100000,2000000)
  plot.window.ylim <- c(350000,1800000)
  years <- 1971:2000

##for (i in 1:30 ) {
##  print(i)
##  tas.sub <- subset(tas.rp.brick,i)
 
    class.breaks <- seq(-8,16,2)
    map.labels <- sapply(1:(length(class.breaks)-1), function(x){
      paste(class.breaks[x], "to", class.breaks[x+1])})
    colour.ramp <- get.legend.colourbar(var.name='tas',map.range=range(class.breaks),
                                        my.bp=0,class.breaks=class.breaks)
                                    

    label.units <- '\u00B0C'
  plot.dir <- '/storage/data/projects/rci/data/assessments/bc/animations/'
##  png(file=paste0(plot.dir,'bc_tas_years_',title,'.png'),width=600,height=600)

  image(tas.sub,col=colour.ramp,breaks=class.breaks,bg='gray94',xlim=plot.window.xlim,ylim=plot.window.ylim,
      main='',xlab='',ylab='',cex.main=2,axes=FALSE)
  plot(ocean.mask.transformed,col='gray94',add=TRUE)
  ##plot(lakes.transformed,col='lightblue',add=TRUE)
  plot(us.transformed,col='gray94',add=TRUE)
  plot(na.bnds.transformed,add=TRUE)

  plot(grat,add=TRUE,lty=5,col='gray')

  legend('topright', col = "black", legend=rev(map.labels), pch=22, pt.bg = rev(colour.ramp),
         pt.cex=2.0, y.intersp=0.8, title.adj=0.2, title=label.units, xjust=0, cex=1.5)
  bnds <- par('usr')
 
  text(1800000,1000000,title,cex=1.5)  
  box(which='plot',lwd=3)
##  dev.off()
##}
}

blend.images <- function(tas.proj,tas.past,len) {

  tas.diff <- (tas.proj - tas.past)/len  
  tas.stack <- tas.past
  for (j in 1:len) {
    tas.step <- tas.past + j*tas.diff
    tas.stack <- stack(tas.stack,tas.step)
  }
  return(tas.stack)
}


##------------------------------------
##Map layers

shape.dir <- '/storage/data/gis/basedata/base_layers'
region <- 'bc_province_albers'
reg.shp <- readOGR(shape.dir, region, stringsAsFactors=F, verbose=F)
lakes.shp <- readOGR(shape.dir, 'bc_lakes', stringsAsFactors=F, verbose=F)
us.shp <- readOGR(shape.dir, 'united_states', stringsAsFactors=F, verbose=F)
na.bnds.shp <- readOGR(shape.dir, 'north_america_state_provincial_boundaries_albers', stringsAsFactors=F, verbose=F)
ocean.mask.shp <- readOGR('/storage/data/projects/rci/data/assessments/crd/shapefiles/','west_coast_ocean', stringsAsFactors=F, verbose=F)
bc.albers.crs <- '+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
wgs.crs <- '+proj=longlat +datum=WGS84 +no_defs'


reg.transformed <- spTransform(reg.shp, CRS(bc.albers.crs))
na.bnds.transformed <- spTransform(na.bnds.shp, CRS(bc.albers.crs))
us.transformed <- spTransform(us.shp, CRS(bc.albers.crs))
ocean.mask.transformed <- spTransform(ocean.mask.shp, CRS(bc.albers.crs))
lakes.transformed <- spTransform(lakes.shp, CRS(bc.albers.crs))

##------------------------------------
##Test of the reprojected mapping
##read.dir <- '/storage/data/projects/rci/data/assessments/high_res_data/bccaq_gcm_bc_subset/rcp45/annual/ACCESS1-0/'

gcm.list <- c('ACCESS1-0',
              'CanESM2',
              'CCSM4',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'HadGEM2-ES',
              'inmcm4',
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

tas.ens.brick <- c()

for (g in seq_along(gcm.list)) {
  gcm <- gcm.list[g]
  print(gcm)  

  read.dir <- paste0('/storage/data/scratch/ssobie/bccaq_gcm_bc_subset/rcp85/annual/',gcm,'/')
  head.file <- list.files(path=read.dir,pattern=paste0('tasmax_ann_BCCAQ_',gcm,'_rcp85_*'),full.name=TRUE)[1]
  file.split <- strsplit(head.file,'_')[[1]]
  run <- file.split[grep('r*i1p1',file.split)]

  txp.file <- paste0(read.dir,'tasmax_ann_BCCAQ_',gcm,'_rcp85_',run,'_1951-2000.nc')
  txf.file <- paste0(read.dir,'tasmax_ann_BCCAQ_',gcm,'_rcp85_',run,'_2001-2100.nc')
  tnp.file <- paste0(read.dir,'tasmin_ann_BCCAQ_',gcm,'_rcp85_',run,'_1951-2000.nc')
  tnf.file <- paste0(read.dir,'tasmin_ann_BCCAQ_',gcm,'_rcp85_',run,'_2001-2100.nc')

  txp.brick <- brick(txp.file)
  txf.brick <- brick(txf.file)

  tnp.brick <- brick(tnp.file)
  tnf.brick <- brick(tnf.file)

  tas.past.brick <- (txp.brick + tnp.brick)/2
  tas.future.brick <- (txf.brick + tnf.brick)/2

  tas.all.brick <- stack(tas.past.brick,tas.future.brick)
  print(dim(tas.all.brick))
  if (is.null(tas.ens.brick)) {
       tas.ens.brick <- tas.all.brick
  } else {
       tas.ens.brick <- tas.ens.brick + tas.all.brick
  }
  
}

tas.ens.brick <- tas.ens.brick/length(gcm.list)
tas.rp.brick <- projectRaster(tas.ens.brick, crs=bc.albers.crs, over=T)

tas.1990s <- subset(tas.rp.brick,31:60)
tas.1990s.clim <- calc(tas.1990s,mean)
##plot.bc.tas(tas.1990s.clim,'1990s')
tas.2020s <- subset(tas.rp.brick,61:90)
tas.2020s.clim <- calc(tas.2020s,mean)
##plot.bc.tas(tas.2020s.clim,'2020s')
tas.2050s <- subset(tas.rp.brick,91:120)
tas.2050s.clim <- calc(tas.2050s,mean)
##plot.bc.tas(tas.2050s.clim,'2050s')
tas.2080s <- subset(tas.rp.brick,121:150)
tas.2080s.clim <- calc(tas.2080s,mean)
##plot.bc.tas(tas.2080s.clim,'2080s')


##-----------------------------------------------
##Test to interpolate the different climatologies
len <- 20
tas.stack.1 <- blend.images(tas.2020s.clim,tas.1990s.clim,len)
tas.stack.2 <- blend.images(tas.2050s.clim,tas.2020s.clim,len)
tas.stack.3 <- blend.images(tas.2080s.clim,tas.2050s.clim,len)

tas.stack <- stack(tas.stack.1,tas.stack.2,tas.stack.3)


##------------------------------------
##Animated in HTML
  years <- 1:60
  
  plot.dir <- '/storage/data/projects/rci/data/assessments/bc/animations/'
  ani.options(interval=0.075,nmax=50)
##  saveHTML({
    saveGIF({
    for (y in 1:len) {     
    plot.bc.tas(tas.1990s.clim,'1990s')
    }
    for (y in 1:len) {     
      plot.bc.tas(subset(tas.stack.1,y),'')
    }
    for (y in 1:len) {     
    plot.bc.tas(tas.2020s.clim,'2020s')
    }
    for (y in 1:len) {     
      plot.bc.tas(subset(tas.stack.2,y),'')
    }
    for (y in 1:len) {     
    plot.bc.tas(tas.2050s.clim,'2050s')
    }
    for (y in 1:len) {     
      plot.bc.tas(subset(tas.stack.3,y),'')
    }
    for (y in 1:len) {     
    plot.bc.tas(tas.2080s.clim,'2080s')
    }
  },    img.name=paste('bc_tas_years',sep=''),
           outdir=plot.dir,
           movie.name='bc_tas_years.gif',
        ani.width=600,ani.height=600,bg='white')


##           htmlfile='bc_tas_years.html',


