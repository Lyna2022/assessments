##Script to plot BC-wide maps of precip, climdex and return period data
##Should add the boundaries of the 8 resource regions to the plot as well.

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)

source('/storage/data/projects/rci/assessments/code/resource.region.map.support.r',chdir=T)       
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)

##-------------------------------------------------------------------------
##Plotting functions
get.region.title <- function(region) {
  rv <- switch(region,
               skeena='Skeena',
               northeast='Northeast',
               kootenay='Kootenay-Boundary',
               omineca='Omineca',
               west='West Coast',
               cariboo='Cariboo',
               thompson='Thompson-Okanagan',
               south='South Coast',
               inshuck_road_boundary_10='Inschuck Forestry Road',
               WhistlerLU_WGS84='Whistler Landscape Unit',
               WhistlerLU_Buffer_10='Whistler Landscape Unit',
               metro_van='',
               metro_van_fraser_valley='Metro Vancouver and Fraser Regional District',
               metro_van_lower_fraser='',
               van_city='',
               north_van='',
               okanagan='Okanagan Regional Districts',
               nanaimo='Nanaimo Regional District')
  return(rv)
}


get.region.names <- function(region) {
 rv <- switch(region,
              metro_van=list(area='metro_van',subset='van_whistler',region='metro_van'),
              metro_van_fraser_valley=list(area='metro_van',subset='van_whistler',region='metro_van_fraser_valley'),
              metro_van_lower_fraser=list(area='metro_van',subset='van_whistler',region='metro_van_lower_fraser'),
              van_city=list(area='metro_van',subset='van_whistler',region='van_city'),
              north_van=list(area='metro_van',subset='van_whistler',region='north_van'),
              WhistlerLU_WGS84=list(area='whistler',subset='van_whistler',region='WhistlerLU_WGS84'),
              okanagan=list(area='okanagan',subset='okanagan',region='okanagan'),
              nanaimo=list(area='nanaimo',subset='nanaimo',region='nanaimo'))
  return(rv)
}


get.region.text.loc <- function(region) {

  rv <- switch(region,
               skeena=4,
               northeast=2,
               kootenay=2,
               omineca=2,
               west=3,
               cariboo=2,
               thompson=2,
               south=4,
               inshuck_road_boundary_10=2,
               WhistlerLU_WGS84=2,
               WhistlerLU_Buffer_10=2,
               metro_van=2,
               metro_van_fraser_valley=2,
               metro_van_lower_fraser=2,
               van_city=2,
               north_van=2,
               okanagan=2,
               nanaimo=2)  
  return(rv)
}

get.leg.loc <- function(region) {

  rv <- switch(region,
               skeena='bottomleft',
               northeast='bottomleft',
               kootenay='topright',
               omineca='topright',
               west='bottomleft',
               cariboo='bottomright',
               thompson='topleft',
               south='topright',
               inshuck_road_boundary_10='topright',
               WhistlerLU_WGS84='topright',
               WhistlerLU_Buffer_10='topright',
               metro_van='topright',
               metro_van_fraser_valley='topright',
               metro_van_lower_fraser='topright',
               van_city='topright',
               north_van='topright',
               okanagan='bottomright',
               nanaimo='bottomleft')               
  return(rv)
}

get.var.title <- function(var.name,rp=NULL) {

  if (!is.null(rp)) {
    rv <- switch(var.name,
                 pr=paste(rp,'-Year Annual Maximum One Day Precipitation',sep=''),
                 tasmax=paste(rp,'-Year Annual Maximum Daily Max Temperature',sep=''),
                 tasmin=paste(rp,'-Year Annual Minimum Daily Min Temperature',sep=''))
  } else {
    rv <- switch(var.name,
                 pr='Precipitation',
                 tasmax='Max Temperature',
                 tmax='Max Temperature',
                 tasmin='Min Temperature',
                 tmin='Min Temperature',
                 cdd='Cooling Degree Days',
                 hdd='Heating Degree Days',
                 gdd='Growing Degree Days',
                 fdd='Freezing Degree Days',
                 ffd='Frost Free Days',
                 s30='Summer Hot Days',
                 gslETCCDI='Growing Season Length',
                 cddETCCDI='Consecutive Dry Days',
                 cwdETCCDI='Consecutive Wet Days',
                 suETCCDI='Summer Days',
                 fdETCCDI='Frost Days',
                 idETCCDI='Ice Days',
                 prcptotETCCDI='Total Precipitation',
                 r95pETCCDI='R95 Precipitation',
                 r99pETCCDI='R99 Precipitation',
                 rx1dayETCCDI='Annual Max 1-Day Precipitation',
                 rx5dayETCCDI='Annual Max 5-Day Precipitation',
                 trETCCDI='Tropical Nights',
                 tx90pETCCDI='Warm Days',
                 tn90pETCCDI='Warm Nights',
                 tx10pETCCDI='Cool Days',
                 tn10pETCCDI='Cool Nights',
                 tnnETCCDI='Coldest Days',
                 txxETCCDI='Hottest Days',
                 pas='Precipitation as Snow',
                 snowdepth='Snowpack') 
  }
}

get.var.grep.name <- function(var.name,rp=NULL) {

  rv <- switch(var.name,
               pr='pr_seas',
               tasmax='tasmax_seas',
               tasmin='tasmin_seas')
}

##-------------------------------------------------------------------------
##Shape and boundary functions

get.region.shape <- function(region,shape.dir) {
  region.shp <- readOGR(shape.dir, region, stringsAsFactors=F, verbose=F)
  return(region.shp)
}

get.bounding.box <- function(region,shape.dir) {

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326"))
  
  ##  ##Bounding box data for rectangular region
  xlim.min <- region.shp@bbox[1,1]
  xlim.max <- region.shp@bbox[1,2]
  ylim.min <- region.shp@bbox[2,1]
  ylim.max <- region.shp@bbox[2,2]
  
  ##Set plot boundaries
  xlim.adj <- (xlim.max - xlim.min) * 0.15 ##0.5 ##0.075
  ylim.adj <- (ylim.max - ylim.min) * 0.15 ##0.25 ##0.075
  plot.window.xlim <- c((xlim.min - xlim.adj), (xlim.max + xlim.adj))
  plot.window.ylim <- c((ylim.min - ylim.adj), (ylim.max + ylim.adj))
  e <- extent(c(plot.window.xlim,plot.window.ylim))
  if (region=='cariboo')
    e <- extent(c(c((xlim.min - xlim.adj), (xlim.max + xlim.adj*10)),plot.window.ylim))
  if (region=='thompson') {
    e <- extent(c(c((xlim.min - xlim.adj*5), (xlim.max + xlim.adj)),
                  c((ylim.min - ylim.adj), (ylim.max + ylim.adj*5))))
  }
  if (region=='metro_van_fraser_valley')
    e <- extent(c(c((xlim.min - xlim.adj*1), (xlim.max + xlim.adj*1)),
                  c((ylim.min - ylim.adj), (ylim.max + ylim.adj*1))))
  if (region=='metro_van')
    e <- extent(c(-123.5857,-121.680,48.91602,49.90))
  if (region=='van_city')
    e <- extent(c(-123.35,-122.9,48.91602,49.375))
  if (region=='north_van')
    e <- extent(c(-123.4,-122.7,48.91602,49.475))

  box.extent <- e
  return(box.extent)
}

get.shp.buffer <- function(region,shape.dir,proj) {

  ##Lat Lon - "+init=epsg:4326"
  ##Ablers  - "+init=epsg:3005"
  shp.buffer <- vector(length=5,mode='list')
  if (region=='metro_van_lower_fraser') {
    shp.buffer[[1]] <- spTransform(get.region.shape('metro_van_lower_fraser_1',shape.dir),CRS(proj))
    shp.buffer[[2]] <- spTransform(get.region.shape('metro_van_lower_fraser_2',shape.dir),CRS(proj))
    shp.buffer[[3]] <- spTransform(get.region.shape('metro_van_lower_fraser_3',shape.dir),CRS(proj))
    shp.buffer[[4]] <- spTransform(get.region.shape('metro_van_lower_fraser_4',shape.dir),CRS(proj))
    shp.buffer[[5]] <- spTransform(get.region.shape('metro_van_lower_fraser_5',shape.dir),CRS(proj))
  } else {
    region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:3005"))    
    bc.format <- spTransform(region.shp,CRS("+init=epsg:3005"))
    xdiff <- extent(bc.format)@xmax - extent(bc.format)@xmin
    ydiff <- extent(bc.format)@ymax - extent(bc.format)@ymin    
    shp.buffer[[1]] <- spTransform(gBuffer(bc.format,width=0.0009*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[2]] <- spTransform(gBuffer(bc.format,width=0.002*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[3]] <- spTransform(gBuffer(bc.format,width=0.004*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[4]] <- spTransform(gBuffer(bc.format,width=0.0055*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[5]] <- spTransform(gBuffer(bc.format,width=0.0065*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
  }
  return(shp.buffer)
}

##-------------------------------------------------------------------------
make.single.plots <- function(past.data,proj.data,past.box,proj.box,
                              region,shape.dir,plot.dir,
                              var.name,scenario,type,ds.type,gcm,proj,
                              seas=NULL,rp.var=NULL,
                              past.int,proj.int,draft=TRUE) {
  
  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS(proj))
  overlays <- NULL ##spTransform(get.region.shape('metro_van_fraser_valley',shape.dir),CRS(proj))
  if (region=='metro_van') {
    shp.buffer <- NULL #get.shp.buffer(region,shape.dir,proj)
  } else {
    shp.buffer <- get.shp.buffer(region,shape.dir,proj)
  }
  var.title <- get.var.title(var.name)                            
  leg.loc <- get.leg.loc(region)                              

  
  ##Individual models
  if (!is.null(seas)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',seas,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Past \n ',toupper(gcm),' ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',seas,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Projections \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',seas,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Anomalies \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',seas,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Percent Change \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
  }
  ##Return Periods
  if (!is.null(rp.var)) {  
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n 20-Year ',var.title,' Past \n ',toupper(gcm),' ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n 20-Year ', var.title,' Projections \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n 20-Year ',var.title,' Anomalies \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n 20-Year ', var.title,' Percent Change \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
  }  
##Climdex
  if (is.null(seas) & is.null(rp.var)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n',var.title,' Past \n ',toupper(gcm),' ',toupper(scenario),' (',past.int,')',sep='')      
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', var.title,' Projections \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n',var.title,' Anomalies \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.',gcm,'.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', var.title,' Percent Change \n ',toupper(gcm),' ',toupper(scenario),' (',proj.int,')',sep='')
  }

  shared.range <- range(c(range(past.data,na.rm=T),range(proj.data,na.rm=T)),na.rm=T)
  shared.box <- range(c(past.box@data@min,past.box@data@max,
                        proj.box@data@min,proj.box@data@max),na.rm=T)

  ##Past
  reg.ds.maps(past.data,past.box,region,
              type,ds.type,region.shp,shp.buffer,
              past.plot.file,past.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=draft)
  
  ##Future
  reg.ds.maps(proj.data,proj.box,region,
              type,ds.type,region.shp,shp.buffer,
              proj.plot.file,proj.plot.title,coords=NULL,proj=proj,
              leg.loc=leg.loc)
  print('Past and Future')

  ##Anomalies
  clim.anoms <- proj.data - past.data
  box.anoms <- proj.box - past.box
  reg.ds.maps(clim.anoms,box.anoms,region,
              type='anomaly',ds.type,region.shp,shp.buffer,
              anoms.plot.file,anoms.plot.title,coords=NULL,proj=proj,
              leg.loc=leg.loc)
  
  ##Percent Change
  clim.prct <- (proj.data - past.data)/abs(past.data)*100
  box.prct <- (proj.box - past.box)/abs(past.box)*100
  clim.prct[is.infinite(clim.prct)] <- NA
  box.prct[is.infinite(box.prct)] <- NA
  reg.ds.maps(clim.prct,box.prct,region,
              type='percent',ds.type,region.shp,shp.buffer,
              prct.plot.file,prct.plot.title,coords=NULL,proj=proj,
              leg.loc=leg.loc)
}

make.ensemble.plots <- function(past.data,proj.data,past.box,proj.box,
                                clim.changes,box.counter,                                
                                region,shape.dir,plot.dir,
                                var.name,scenario,type,ds.type,proj,
                                seas=NULL,rp.var=NULL,
                                past.int,proj.int,draft) {

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS(proj))
  overlays <- NULL ##spTransform(get.region.shape('metro_van_fraser_valley',shape.dir),CRS(proj))
  if (region=='metro_van') {
    shp.buffer <- NULL #get.shp.buffer(region,shape.dir,proj)
  } else {
    shp.buffer <- get.shp.buffer(region,shape.dir,proj)
  }
  var.title <- get.var.title(var.name)                            
  leg.loc <- get.leg.loc(region)                              
                              
  ##Individual models
  if (!is.null(seas)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',type,'.',seas,'.increases.png',sep='')
    num.plot.title <- paste(get.region.title(region),' ', seas,' \n Number of GCMs with increasing ',var.title,
                        '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
  }
  ##Return Periods
  if (!is.null(rp.var)) {
    var.title <- get.var.title(var.name,rp=20)                            
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n ', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n Projected Change in ',var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n Projected Change in ', var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',type,'.increases.png',sep='')
    num.plot.title <- paste(get.region.title(region),' \n Number of GCMs with increasing 20-Year ',var.title,
                            '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')        
  }  
##Climdex
  if (is.null(seas) & is.null(rp.var)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')      
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.increases.png',sep='')
    num.plot.title <- paste(get.region.title(region),' \n Number of GCMs with increasing ',var.title,
                            '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')    

  }

  shared.range <- range(c(range(past.data,na.rm=T),range(proj.data,na.rm=T)),na.rm=T)
  shared.box <- range(c(past.box@data@min,past.box@data@max,
                        proj.box@data@min,proj.box@data@max),na.rm=T)
  ##shared.box <- c(-7.5,7.5)
  ##Ensemble plots
    ##Past

  ##Was 20
  if (var.name=='snowdepth') {

    clim.anoms.ens <- proj.data - past.data
    box.ens.anoms <- proj.box - past.box
    clim.prct.ens <- (proj.data - past.data)/abs(past.data)*100
    box.ens.prct <- (proj.box - past.box)/abs(past.box)*100
    
    flag <- past.data >= 10
    box.flag <- past.box >=10
    past.data[past.data >= 10] <- 50
    past.data[past.data < 0.01] <- NA
    past.box[past.box >= 10] <- 50
    past.box[past.box < 0.01] <- NA

    proj.data[past.data >= 10] <- 50
    proj.box[past.box >= 10] <- 50
    
    clim.anoms.ens[flag] <- 50
    clim.anoms.ens[is.na(past.data)] <- NA
    box.ens.anoms[box.flag] <- 50
    box.ens.anoms[is.na(past.box)] <- NA

    clim.prct.ens[flag] <- 50    
    box.ens.prct[box.flag] <- 50
    clim.prct.ens[is.na(past.data)] <- NA
    box.ens.prct[is.na(past.box)] <- NA
    
    proj.data[proj.data < 0.01] <- NA
    proj.box[proj.box < 0.01] <- NA

  }
##  past.plot.title <- ''
  if (1==1) {  
  reg.ds.maps(past.data,past.box,region,
              type,ds.type,region.shp,shp.buffer,
              past.plot.file,past.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=draft)

  ##Future
  reg.ds.maps(proj.data,proj.box,region,
              type,ds.type,region.shp,shp.buffer,
              proj.plot.file,proj.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=draft)
}

  if (var.name !='snowdepth') {
    ##Anomalies
    clim.anoms.ens <- proj.data - past.data
    box.ens.anoms <- proj.box - past.box
  }
  
  reg.ds.maps(clim.anoms.ens,box.ens.anoms,region,
              type='anomaly',ds.type,region.shp,shp.buffer,
              anoms.plot.file,anoms.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,draft=draft)

  if (grepl("(pr|snm|snd|prcptot|rx|r9|snowdepth)", var.name)) {
    ##Percent Change
    if (var.name !='snowdepth') {
      clim.prct.ens <- (proj.data - past.data)/abs(past.data)*100
      box.ens.prct <- (proj.box - past.box)/abs(past.box)*100
    }

    reg.ds.maps(clim.prct.ens,box.ens.prct,region,
                type='percent',ds.type,region.shp,shp.buffer,
                prct.plot.file,prct.plot.title,coords=NULL,proj=proj,
                overlays=overlays,leg.loc=leg.loc,draft=draft)
  }

  ##Number of models with greater than 0 change
##  browser()
  ##reg.ds.maps(clim.changes,box.counter,region,
  ##            type='increases',ds.type,region.shp,shp.buffer,
  ##            num.plot.file,num.plot.title,coords=NULL,
  ##            leg.loc=leg.loc)  
}##Ensemble plot function

##-------------------------------------------------------------------------
##-------------------------------------------------------------------------

rcp26.list <- c('CanESM2',
                'CCSM4',
                'CNRM-CM5',
                'CSIRO-Mk3-6-0',
                'GFDL-ESM2G',
                'HadGEM2-ES',
                'MIROC5',
                'MPI-ESM-LR',
                'MRI-CGCM3')
 
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


seasonal.directories <- function(var.name,scenario,seas,time.dir,grep.name,meta,draft) {
  
  if (grepl('seas',grep.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/',scenario,'/seasonal/',sep='')
    #read.dir <- '/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm_bc_subset/rcp85/' ##For BCCAQ 10km
  if (grepl('mon',grep.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/',scenario,'/monthly/',sep='')

  if (draft) {
    if (grepl('(seas|apr1|may1)',grep.name))
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/',var.name,'/',tolower(seas),'/',time.dir,'/',sep='')  
    if (grepl('mon',grep.name))
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/',var.name,'/monthly/',time.dir,'/',sep='')
  } else {
    if (grepl('(seas|apr1|may1)',grep.name))
     # plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/plots/',region,'/bccaq/',scenario,'/',var.name,'/',tolower(seas),'/',sep='')
     # plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/plots/',region,'/bccaq_800m/',scenario,'/',var.name,'/',tolower(seas),'/',sep='')
      
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/',var.name,'/',tolower(seas),'/',sep='')  
    if (grepl('mon',grep.name))
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/',var.name,'/monthly/',sep='')    
  }

  if (grepl('(apr1|may1)',grep.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/',scenario,'/seasonal/',sep='')    
  if (var.name=='snowdepth')
    read.dir <- gsub(pattern='seasonal',replacement='snow',read.dir)
      
  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  print('Plot dir')
  print(plot.dir)
  return(rv)  
}
 
plot.single.seasonal <- function(region,scenario,proj.int,
                                 var.name,grep.name,seas,draft) {
  meta <- get.region.names(region)
  
  print('Single Season')
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  past.int <- '1971-2000'

  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]

  proj <- "+init=epsg:4326"
  seas.fx <- mean
  if (var.name=='pr')
    seas.fx <- sum

  ds.type <- 'bccaq_gcm'
  type <- 'gcm'
  leg.loc <- get.leg.loc(region)
  var.title <- get.var.title(var.name)
  print(paste(toupper(var.name),' ',scenario,' ',time.dir,' ',seas,sep=''))
  
  ##Directories
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/shapefiles/',sep='')
  if (grepl('seas',grep.name)) {
    seas.ix <- switch(seas,
                      Winter=1,
                      Spring=2,
                      Summer=3,
                      Fall=4,
                      Annual=5)
  }
  if (grepl('mon',grep.name)) {
    seas.ix <- switch(seas,
                      January=1,
                      February=2,
                      March=3,
                      April=4,
                      May=5,
                      June=6,
                      July=7,
                      August=8,
                      September=9,
                      October=10,
                      November=11,
                      December=12)
  }

  dirs <- seasonal.directories(var.name,scenario,seas,time.dir,grep.name,meta,draft)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir
  
  ##-------------------------------------------------------------------------
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon

  ##Bounding box data for rectangular region
  box.extent <- get.bounding.box(region,shape.dir)
  if (1==1) { ##!file.exists(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))) {

    ##Prep for the ensemble files
    clim.past.all <- c()
    clim.proj.all <- c()
    past.box.ens <- c()
    proj.box.ens <- c()
    
    for (gcm in gcm.list) {
      print(gcm)
      clim.files <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=grep.name,full.name=TRUE)
      past.file <- clim.files[grep(past.int,clim.files)]
      proj.file <- clim.files[grep(proj.int,clim.files)]    

      ##Data clipped by region shapefile
      past.data <- gcm.region.extract(past.file,var.name,gcm,rcm='gcm',clip.shp=region.shp)
      proj.data <- gcm.region.extract(proj.file,var.name,gcm,rcm='gcm',clip.shp=region.shp)    
      
      ##------------------------------------------------
      ##Check for anomalously large values in the projected data
      thrsh <- 5*max(past.data,na.rm=T)
      flags <- which(proj.data > thrsh)
      
      if (length(flags) !=0) {
        print('Warning: Anomalously large values found in the projected data at coordinates:\n')
        for (f in 1:length(flags)) {
          print(attr(past.data,'spatial.coords')[flags[f],])
          proj.data[flags[f]] <- (proj.data[flags[f]-1] + proj.data[flags[f]-2] +
                                  proj.data[flags[f]+1] + proj.data[flags[f]+2])/4
        }
      }
      
      past.box <- brick(past.file)
      proj.box <- brick(proj.file)
      
      clim.atts <- attributes(past.data)
      clim.past.atts <- clim.atts
      clim.proj.atts <- clim.atts

      if(seas=='APRIL1'|seas=='MAY1') {
        clim.past <- past.data
        clim.proj <- proj.data
        past.box.seas <- past.box
        proj.box.seas <- proj.box
      } else {
        if (seas=='Annual') {
          clim.past <- apply(past.data,1,seas.fx,na.rm=T)
          clim.proj <- apply(proj.data,1,seas.fx,na.rm=T)
          past.box.seas <- calc(past.box,seas.fx)
          proj.box.seas <- calc(proj.box,seas.fx)
        } else {

          clim.past <- past.data[,seas.ix]
          past.box.seas <- subset(past.box,seas.ix)
          clim.proj <- proj.data[,seas.ix]
          proj.box.seas <- subset(proj.box,seas.ix)            
        }
      }
      clim.past.atts$dim <- dim(clim.past)
      attributes(clim.past) <- clim.past.atts
      clim.proj.atts$dim <- dim(clim.proj)
      attributes(clim.proj) <- clim.proj.atts
      
      past.box.crop <- crop(past.box.seas,box.extent)
      proj.box.crop <- crop(proj.box.seas,box.extent)
      box.anoms <- proj.box.crop - past.box.crop
      
      ##Use projectRaster to change the box projection to BC Albers
      past.box.project <- past.box.crop ##projectRaster(past.box.crop,crs=proj)
      proj.box.project <- proj.box.crop ##projectRaster(proj.box.crop,crs=proj)
      
      ##Make single GCM plots
##      make.single.plots(past.data=clim.past,proj.data=clim.proj,
##                        past.box=past.box.project,proj.box=proj.box.project,
##                        region=region,shape.dir=shape.dir,plot.dir=plot.dir,
##                        var.name=var.name,scenario=scenario,
##                        type=type,ds.type=ds.type,gcm=gcm,proj=proj,seas=seas,
##                        past.int=past.int,proj.int=proj.int,draft=TRUE)
      ##print('Made one plot')
      
      clim.past.all <- rbind(clim.past.all,clim.past)
      clim.proj.all <- rbind(clim.proj.all,clim.proj)
      
      if (is.null(past.box.ens)) {
        past.box.ens <- past.box.crop
        box.counter <- calc(box.anoms,fun=function(x){x*0})      
        proj.box.ens <- proj.box.crop
      } else {
        past.box.ens <- past.box.ens + past.box.crop
        proj.box.ens <- proj.box.ens + proj.box.crop
      }
      box.counter <- box.counter + calc(box.anoms,fun=function(x){rv=x;rv[x>0]=1;rv[x<=0]=0;return(rv)})    
    }##GCM Loop
    
    clim.past.ens <- apply(clim.past.all,2,mean,na.rm=TRUE)
    clim.proj.ens <- apply(clim.proj.all,2,mean,na.rm=TRUE)  
    past.box.ens <- past.box.ens/length(gcm.list)
    proj.box.ens <- proj.box.ens/length(gcm.list)
    
    clim.past.atts$dim <- dim(clim.past.ens)
    clim.past.atts$gcm <- 'Ensemble'
    attributes(clim.past.ens) <- clim.past.atts
    
    clim.proj.atts$dim <- dim(clim.proj.ens)
    clim.proj.atts$gcm <- 'Ensemble'
    attributes(clim.proj.ens) <- clim.proj.atts
    
    clim.changes <- apply( (clim.proj.all-clim.past.all) , 2, function(x) {sum(x>0,na.rm=T)})
    clim.past.atts$dim <- dim(clim.changes)
    attributes(clim.changes) <- clim.past.atts
    
    ##Use projectRaster to change the box projection to BC Albers
    past.box.ens.project <- past.box.ens ##projectRaster(past.box.ens,crs=proj)
    proj.box.ens.project <- proj.box.ens ##projectRaster(proj.box.ens,crs=proj)

    ##Save the ensemble files
    if (!file.exists(paste(read.dir,'data_files/',sep='')))
      dir.create(paste(read.dir,'data_files/',sep=''),recursive=T)
    save(clim.past.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    save(clim.proj.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    save(past.box.ens.project,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    save(proj.box.ens.project,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
  } else { 
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
  }

  print('Plotting')

  ##Ensemble plots
  make.ensemble.plots(past.data=clim.past.ens,proj.data=clim.proj.ens,
                      past.box=past.box.ens.project,proj.box=proj.box.ens.project,
                      clim.changes=clim.changes,box.counter=box.counter,                                            
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int,draft=draft)
}

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Return Periods
return.period.directories <- function(var.name,scenario,time.dir,region,meta,draft) {
  
  read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/rcp85/return_periods/',sep='')
  if (draft) {
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/return_periods/',var.name,'/',time.dir,'/',sep='')
  } else {
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/return_periods/',var.name,'/',sep='')
  }
  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}


plot.return.periods <- function(region,scenario,proj.int,
                                var.name,rp,draft) {
  print('Return Periods')
  meta <- get.region.names(region)
  ##Change all of these
  rp.title <- get.var.title('pr',rp=rp)
  past.int <- '1971-2000'
  time.intervals <- '2041-2070' ##c('2011-2040','2041-2070','2071-2100')
  time.dirs <- '2050s' ##c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]
  
  rp.var <- paste('rp.',rp,sep='')
  proj <- "+init=epsg:4326" ##BC Albers
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  
  ##--------------------------------------------------
  rp.grep <- paste(var.name,'_RPCI',rp,'_BCCAQ_PRISM',sep='')

  ds.type <- 'bccaq_gcm'
  type <- 'gcm'
  leg.loc <- get.leg.loc(region)  

  ##-------------------------------------------
  
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/shapefiles/',sep='')
  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon

  ##Bounding box data for rectangular region
  box.extent <- get.bounding.box(region,shape.dir)
  ##-------------------------------------------
  
  dirs <- return.period.directories(var.name,scenario,time.dir,region,meta,draft)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
  
  ##First file for attributes
  gcm <- gcm.list[1]

  clim.files <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=rp.grep,full.name=TRUE)
  clim.past.file <- clim.files[grep(past.int,clim.files)]
  clim.proj.file <- clim.files[grep(proj.int,clim.files)]  

  clim.past.data <- gcm.region.extract(clim.past.file,rp.var,gcm,rcm='gcm',clip.shp=region.shp)
  clim.proj.data <- gcm.region.extract(clim.proj.file,rp.var,gcm,rcm='gcm',clip.shp=region.shp)

  time <- attr(clim.past.data,'time.values')
  clim.atts <- attributes(clim.past.data)

  clim.past.data <- clim.past.data[,2]##2 for RP values
  clim.proj.data <- clim.proj.data[,2]
  clim.past.atts <- clim.atts
  clim.proj.atts <- clim.atts

  ##Ensemble files
  clim.past.all <- matrix(NA,nrow=length(clim.past.data),ncol=length(gcm.list))
  clim.proj.all <- matrix(NA,nrow=length(clim.proj.data),ncol=length(gcm.list))
  past.box.ens <- c()
  proj.box.ens <- c()
  
  past.range <- c()
  proj.range <- c()
  anom.range <- c()
  prct.range <- c()

  ##Loop over GCMs
  for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    clim.files <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=rp.grep,full.name=TRUE)
    clim.past.file <- clim.files[grep(past.int,clim.files)]
    clim.proj.file <- clim.files[grep(proj.int,clim.files)]  

    clim.past.data <- gcm.region.extract(clim.past.file,rp.var,gcm,rcm='gcm',clip.shp=region.shp)
    clim.proj.data <- gcm.region.extract(clim.proj.file,rp.var,gcm,rcm='gcm',clip.shp=region.shp)

    attr(clim.past.data,'var.name') <- var.name
    attr(clim.proj.data,'var.name') <- var.name    

    clim.past.data <- clim.past.data[,2]
    clim.proj.data <- clim.proj.data[,2]

    ##------------------------------------------------
    ##Check for anomalously large values in the projected data
    thrsh <- 5*max(clim.past.data,na.rm=T)
    flags <- which(clim.proj.data > thrsh)
    if (var.name=='tasmin')
      flags <- which(clim.proj.data < thrsh)

    if (length(flags) !=0) {
      print('Warning: Anomalously large values found in the projected data at coordinates:\n')
      browser()
      for (f in 1:length(flags)) {
        print(attr(clim.past.data,'spatial.coords')[flags[f],])
        clim.proj.data[flags[f]] <- (clim.proj.data[flags[f]-1] + clim.proj.data[flags[f]-2] +
                                     clim.proj.data[flags[f]+1] + clim.proj.data[flags[f]+2])/4
      }
    }

    ##------------------------------------------------
    clim.atts <- attributes(clim.past.data)
    clim.past.all[,i] <- clim.past.data
    clim.proj.all[,i] <- clim.proj.data ##2 for RP values 

    ##------------------------------------------------
    ##Outer raster box
    past.box <- brick(clim.past.file)
    past.box.crop <- subset(crop(past.box,box.extent),2) ##2 for RP values

    proj.box <- brick(clim.proj.file)
    proj.box.crop <- subset(crop(proj.box,box.extent),2)
    box.anoms <- proj.box.crop - past.box.crop
    ##Use projectRaster to change the box projection to BC Albers
    past.box.project <- past.box.crop ##projectRaster(past.box.crop,crs=proj)
    proj.box.project <- proj.box.crop ##projectRaster(proj.box.crop,crs=proj)
    
    ##------------------------------------------------
    ##Individual plots of models
    ##make.single.plots(past.data=clim.past.data,proj.data=clim.proj.data,
    ##                  past.box=past.box.project,proj.box=proj.box.project,
    ##                  region=region,shape.dir=shape.dir,plot.dir=plot.dir,
    ##                  var.name=var.name,scenario=scenario,proj=proj,
    ##                  type=type,ds.type=ds.type,gcm=gcm,rp.var=rp.var,
    ##                  past.int=past.int,proj.int=proj.int)

    if (is.null(past.box.ens)) {
      past.box.ens <- past.box.crop
      box.counter <- calc(box.anoms,fun=function(x){x*0})      
      proj.box.ens <- proj.box.crop
    } else {
      past.box.ens <- past.box.ens + past.box.crop
      proj.box.ens <- proj.box.ens + proj.box.crop
    }
    box.counter <- box.counter + calc(box.anoms,fun=function(x){rv=x;rv[x>0]=1;rv[x<=0]=0;return(rv)})    

  }##GCM Loop

  clim.past.ens <- apply(clim.past.all,1,mean,na.rm=TRUE)
  clim.proj.ens <- apply(clim.proj.all,1,mean,na.rm=TRUE)  
  past.box.ens <- past.box.ens/length(gcm.list)
  proj.box.ens <- proj.box.ens/length(gcm.list)
  
  clim.past.atts$dim <- dim(clim.past.ens)
  clim.past.atts$gcm <- 'Ensemble'
  clim.past.atts$var.name <- var.name
  attributes(clim.past.ens) <- clim.past.atts
  
  clim.proj.atts$dim <- dim(clim.proj.ens)
  clim.proj.atts$gcm <- 'Ensemble'
  clim.proj.atts$var.name <- var.name  
  attributes(clim.proj.ens) <- clim.proj.atts

  clim.changes <- apply( (clim.proj.all-clim.past.all) , 1, function(x) {sum(x>0,na.rm=T)})
  clim.past.atts$dim <- dim(clim.changes)
  attributes(clim.changes) <- clim.past.atts

  ##Use projectRaster to change the box projection to BC Albers
  past.box.ens.project <- past.box.ens ##projectRaster(past.box.ens,crs=proj)
  proj.box.ens.project <- proj.box.ens ##projectRaster(proj.box.ens,crs=proj)

  ##Ensemble plots
  make.ensemble.plots(past.data=clim.past.ens,proj.data=clim.proj.ens,
                      past.box=past.box.ens.project,proj.box=proj.box.ens.project,
                      clim.changes=clim.changes,box.counter=box.counter,                      
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,rp.var=rp.var,
                      past.int=past.int,proj.int=proj.int,draft=draft)
}

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Climdex 

climdex.directories <- function(var.name,scenario,time.dir,meta,draft) {

  ##Read directories
  if (scenario=='rcp45' | scenario=='rcp26') {
    read.dir <- paste('/storage/data/projects/rci/data/stat.downscaling/BCCAQ/bccaq_gcm/',scenario,'/climdex/full/',sep='') ##For RCP45 and 26
  } else {
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/rcp85/climdex/',sep='') ##For RCP85
  }    
  if (grepl('(ffd|dd|s30)',var.name) & !grepl('ETCCDI',var.name)) 
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/rcp85/degree_days/',sep='')
  if (grepl('pas',var.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/rcp85/pas/',sep='')
  
  if (draft) {
    ##Plot directories
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/climdex/',var.name,'/',time.dir,'/',sep='')
    if (seas != 'Annual') 
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/climdex/',var.name,'/seasonal/',time.dir,'/',sep='')    
    if (grepl('(pas|s30)',var.name))
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',region,'/bccaq_800m/',scenario,'/degree_days/',var.name,'/',time.dir,'/',sep='')    
  } else {
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/climdex/',var.name,'/',sep='')
    if (grepl('(ffd|dd)',var.name) & !grepl('ETCCDI',var.name) | grepl('pas',var.name))
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/degree_days/',var.name,'/',sep='')    
  }
  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}

plot.climdex <- function(region,scenario,proj.int,
                         var.name,seas,draft) {
  meta <- get.region.names(region)                  
  print('Climdex')
  ##----
  past.int <- '1971-2000'
  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]
  
  proj <- "+init=epsg:4326" ##BC Albers
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  
  print(paste(toupper(var.name),' ',scenario,' ',time.dir,sep=''))
  
  ds.type <- 'bccaq_gcm'
  type <- 'gcm'

  var.title <- get.var.title(var.name)
  grep.name <- var.name
  seas.fx <- mean
  if (grepl('(txx|tnn)',var.name)) {
    seas.fx <- switch(var.name,
                      txxETCCDI=max,
                      tnnETCCDI=min)
  }
      
  ##-------------------------------------------------
  ##Directories
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/shapefiles/',sep='')
  dirs <- climdex.directories(var.name,scenario,time.dir,meta,draft)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir

  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
  ##-------------------------------------------------
  
  leg.loc <- get.leg.loc(region)
  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326"))

  ##Bounding box data for rectangular region
  box.extent <- get.bounding.box(region,shape.dir)

  box.function <- function(x,fac,fx) {
    return(tapply(x,fac,fx))
  }
  
  if (1==1) { ##(!file.exists(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))) {
    #browser()
    ##Prep for the ensemble files
    clim.past.all <- c()
    clim.proj.all <- c()  
    past.box.ens <- c()
    proj.box.ens <- c()
    
    for (gcm in gcm.list) {
      print(gcm)
      clim.file <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=paste('^',grep.name,sep=''),full.name=TRUE)
      all.data <- gcm.region.extract(clim.file,var.name,gcm,rcm='gcm',clip.shp=region.shp)
      clim.atts <- attributes(all.data)
      clim.past.atts <- clim.atts 
      clim.proj.atts <- clim.atts       
      
      time <- as.PCICt(attr(all.data,'time.values'),'standard')
      
      p.st <- head(grep(strsplit(past.int,'-')[[1]][1],time),1)
      p.en <- tail(grep(strsplit(past.int,'-')[[1]][2],time),1)
      f.st <- head(grep(strsplit(proj.int,'-')[[1]][1],time),1)
      f.en <- tail(grep(strsplit(proj.int,'-')[[1]][2],time),1)
      if (grepl('HadGEM2',gcm)  & proj.int == '2071-2100')
        f.en <- length(time)
      
      past.subset <- p.st:p.en
      proj.subset <- f.st:f.en

      ##Outer raster box
      clim.box <- brick(clim.file)      
      
      ##Seasonal subset
      if (seas != 'Annual') {
        past.time <- time[p.st:p.en]
        proj.time <- time[f.st:f.en]
        seas.match <- switch(seas,
                             Winter='(*-01-02|*-02-02|*12-02)',
                             Spring='(*-03-02|*-04-02|*05-02)',
                             Summer='(*-06-02|*-07-02|*08-02)',
                             Fall='(*-09-02|*-10-02|*11-02)')
        past.subset <- past.subset[grepl(seas.match,past.time)]
        proj.subset <- proj.subset[grepl(seas.match,proj.time)]

        past.seas.time <- past.time[grepl(seas.match,past.time)]
        proj.seas.time <- proj.time[grepl(seas.match,proj.time)]
        
        past.seas.fac <- as.factor(format(past.seas.time,'%Y'))
        proj.seas.fac <- as.factor(format(proj.seas.time,'%Y'))
        
        clim.seas.past <- t(apply(all.data[,past.subset],1,function(x,fac,fx){tapply(x,fac,fx)},past.seas.fac,seas.fx))
        clim.seas.proj <- t(apply(all.data[,proj.subset],1,function(x,fac,fx){tapply(x,fac,fx)},proj.seas.fac,seas.fx))
        clim.past <- apply(clim.seas.past,1,mean,na.rm=TRUE)
        clim.proj <- apply(clim.seas.proj,1,mean,na.rm=TRUE)
        
        ##Outer box
        past.box.seas <- calc(subset(clim.box,past.subset),function(x){box.function(x,past.seas.fac,seas.fx)})
        past.box.avg <- calc(past.box.seas,fun=mean,na.rm=T)
        proj.box.seas <- calc(subset(clim.box,proj.subset),function(x){box.function(x,proj.seas.fac,seas.fx)})
        proj.box.avg <- calc(proj.box.seas,fun=mean,na.rm=T)
        
      } else {
        clim.past <- apply(all.data[,past.subset],1,mean,na.rm=T)
        clim.proj <- apply(all.data[,proj.subset],1,mean,na.rm=T)

        past.box.avg <- calc(subset(clim.box,past.subset),fun=mean,na.rm=T)
        proj.box.avg <- calc(subset(clim.box,proj.subset),fun=mean,na.rm=T)
      }

      past.box.crop <- crop(past.box.avg,box.extent)
      proj.box.crop <- crop(proj.box.avg,box.extent)
      box.anoms <- proj.box.crop - past.box.crop        
      
      clim.past.atts$dim <- dim(clim.past)
      clim.past.atts$time.values <- time[p.st]
      attributes(clim.past) <- clim.past.atts

      clim.proj.atts$dim <- dim(clim.proj)
      clim.proj.atts$time.values <- time[p.st]
      attributes(clim.proj) <- clim.proj.atts
      
      ##Use projectRaster to change the box projection to BC Albers
      past.box.project <- past.box.crop ##projectRaster(past.box.crop,crs=proj)
      proj.box.project <- proj.box.crop ##projectRaster(proj.box.crop,crs=proj)
      
      ##----------------------------------------------------------------------
      ##Individual plots of models
      if (1==0) {
        make.single.plots(past.data=clim.past,proj.data=clim.proj,
                          past.box=past.box.project,proj.box=proj.box.project,
                          region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                          var.name=var.name,scenario=scenario,proj=proj,
                          type=type,ds.type=ds.type,gcm=gcm,
                          past.int=past.int,proj.int=proj.int)
      }
      clim.past.all <- rbind(clim.past.all,clim.past)
      clim.proj.all <- rbind(clim.proj.all,clim.proj)
      
      if (is.null(past.box.ens)) {
        past.box.ens <- past.box.crop
        box.counter <- calc(box.anoms,fun=function(x){x*0})      
        proj.box.ens <- proj.box.crop
      } else {
        past.box.ens <- past.box.ens + past.box.crop
        proj.box.ens <- proj.box.ens + proj.box.crop
      }
      box.counter <- box.counter + calc(box.anoms,fun=function(x){rv=x;rv[x>0]=1;rv[x<=0]=0;return(rv)})    
    }
    
    clim.past.ens <- apply(clim.past.all,2,mean,na.rm=TRUE)
    clim.proj.ens <- apply(clim.proj.all,2,mean,na.rm=TRUE)  
    
    clim.past.atts$dim <- dim(clim.past.ens)
    clim.past.atts$gcm <- 'Ensemble'
    attributes(clim.past.ens) <- clim.past.atts
    
    clim.proj.atts$dim <- dim(clim.proj.ens)
    clim.proj.atts$gcm <- 'Ensemble'
    attributes(clim.proj.ens) <- clim.proj.atts
    
    past.box.ens <- past.box.ens/length(gcm.list)
    proj.box.ens <- proj.box.ens/length(gcm.list)
    
    clim.changes <- apply( (clim.proj.all-clim.past.all) , 2, function(x) {sum(x>0,na.rm=T)})
    clim.past.atts$dim <- dim(clim.changes)
    attributes(clim.changes) <- clim.past.atts
    
    ##Use projectRaster to change the box projection to BC Albers
    past.box.ens.project <- past.box.ens ##projectRaster(past.box.ens,crs=proj)
    proj.box.ens.project <- proj.box.ens ##projectRaster(proj.box.ens,crs=proj)

    ##Change to Frost Free days instead
    #if (var.name=='fdETCCDI') {
    #  clim.past.ens <- 365 - clim.past.ens
    #  clim.proj.ens <- 365 - clim.proj.ens
    #  past.box.ens.project <- 365 - past.box.ens.project
    #  proj.box.ens.project <- 365 - proj.box.ens.project
    #}
    ##print(range(past.box.ens.project))
    ##print(range(proj.box.ens.project))
    ##Ensemble plots

    ##Save the ensemble files
    if (!file.exists(paste(read.dir,'data_files/',sep='')))
      dir.create(paste(read.dir,'data_files/',sep=''),recursive=T)
    
    save(clim.past.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    save(clim.proj.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    save(past.box.ens.project,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    save(proj.box.ens.project,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))

  } else {
 
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
  }
  
  make.ensemble.plots(past.data=clim.past.ens,proj.data=clim.proj.ens,
                      past.box=past.box.ens.project,proj.box=proj.box.ens.project,
                      clim.changes=clim.changes,box.counter=box.counter,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int,draft=draft)
}

###***********************************************************************************
###***********************************************************************************

##region.list <- c('cariboo','kootenay','northeast','omineca','skeena','south','thompson','west')
##region.list <- 'inshuck_road_boundary_10'
##region <- 'WhistlerLU_WGS84'
##area <- 'whistler'
##region <- 'metro_van' ##
##area <- 'metro_van' ##
#region <- 'nanaimo'
#area <- 'nanaimo'
region <- 'north_van'
area <- 'north_van'

source(paste('/storage/data/projects/rci/assessments/code/',area,'_map_support.r',sep=''),chdir=T)
##source(paste('/storage/data/projects/rci/assessments/code/van_city_map_support.r',sep=''),chdir=T)

scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
proj.intervals <- c('2041-2070','2071-2100') ##c('2011-2040','2041-2070','2071-2100')
draft <- TRUE
##Single Season
run.season <- function() {
  var.name <- 'tasmin' ##'snowdepth'
  grep.name <- 'tasmin_seas' ##'snowdepth_seas'
  seas.list <- c('Winter','Spring','Summer','Fall','Annual') ##'APRIL1' c('January','February','March','April','May','June','July','August','September','October','November','December')
  for (seas in seas.list) {
    for (scenario in scenario.list) {
      for (proj.int in proj.intervals) {
        print(region)
        plot.single.seasonal(region,scenario,proj.int,
                             var.name,grep.name,seas,draft=FALSE)

      }
    }
  }
}

##Return Periods
run.return.periods <- function() {
  var.name <- 'tasmin'
  for (scenario in scenario.list) {
    for (proj.int in proj.intervals) {        
      print(region)
      plot.return.periods(region,scenario,proj.int,
                          var.name,rp='20',draft=FALSE)
    }
  }
}


##Climdex
run.climdex <- function() {
  ##var.names <- c('cddETCCDI','cwdETCCDI','prcptotETCCDI','rx1dayETCCDI','rx5dayETCCDI','suETCCDI','tn10pETCCDI','tx90pETCCDI','tnnETCCDI','txxETCCDI') 
  var.names <- c('fdETCCDI','gslETCCDI','idETCCDI','suETCCDI','trETCCDI','tnnETCCDI','txxETCCDI')
##  var.names <- c('r95pETCCDI','r99pETCCDI')
  seas.list <- c('Annual') ##,'Spring','Summer','Fall')
  for (seas in seas.list) {
    for (var.name in var.names) {
      for (scenario in scenario.list) {
        for (proj.int in proj.intervals) {                
          print(region)
          plot.climdex(region,scenario,proj.int,
                       var.name,seas,draft=FALSE)
        }
      }
    }
  }
}

##Degree Days
run.dd <- function() {
  var.names <- c('cdd','hdd','fdd','gdd')
  ##var.names <- c('gdd')
  for (var.name in var.names) {
    for (scenario in scenario.list) {
      for (proj.int in proj.intervals) {                
        print(region)
        plot.climdex(region,scenario,proj.int,
                     var.name,seas='Annual',draft=FALSE)
      }
    }
  }
}
