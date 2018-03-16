##Script to plot BC-wide maps of precip, climdex and return period data
##Should add the boundaries of the 8 resource regions to the plot as well.

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)

##source('/storage/data/projects/rci/assessments/code/new.northeast.map.support.r',chdir=T)       
source('/storage/data/projects/rci/assessments/code/resource.region.map.support.r',chdir=T)       
##source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)

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
               van_coastal_health='Vancouver Coastal Health',
               lionsgate_hospital='Lionsgate Hospital Region',
               willow_road='Willow Forestry Road')
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
              nanaimo=list(area='nanaimo',subset='nanaimo',region='nanaimo'),
              van_coastal_health=list(area='van_coastal_health',subset='van_coastal_health',region='van_coastal_health'),
              lionsgate_hospital=list(area='lionsgate_hospital',subset='van_coastal_health',region='lionsgate_hospital'),
              willow_road=list(area='willow_road',subset='willow_road',region='willow_road'))
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
               van_coastal_health=2,
               lionsgate_hospital=2,
               willow_road=2)
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
               van_coastal_health='bottomright',
               lionsgate_hospital='bottomright',
               willow_road='topright')
  return(rv)
}

get.var.title <- function(var.name,rp=NULL) {

  if (!is.null(rp)) {
    rv <- switch(var.name,
                 pr=paste(rp,'-Year Annual Max One Day Precipitation',sep=''),
                 tasmax=paste(rp,'-Year Annual Max Daily Max Temperature',sep=''),
                 tasmin=paste(rp,'-Year Annual Min Daily Min Temperature',sep=''))
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
                 r95daysETCCDI='R95 Precipitation Days',
                 r99pETCCDI='R99 Precipitation',
                 r99daysETCCDI='R95 Precipitation Days',
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
  if (region=='van_coastal_health')
    e <- extent(c(-125.2,-122.0,48.9,51.1))
  if (region=='thompson') {
    e <- extent(c(c((xlim.min - xlim.adj*5), (xlim.max + xlim.adj)),
                  c((ylim.min - ylim.adj), (ylim.max + ylim.adj*5))))
  }
  box.extent <- e
  return(box.extent)
}

get.shp.buffer <- function(region,shape.dir,proj) {

  ##Lat Lon - "+init=epsg:4326"
  ##Ablers  - "+init=epsg:3005"
  shp.buffer <- vector(length=5,mode='list')
    region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:3005"))    
    bc.format <- spTransform(region.shp,CRS("+init=epsg:3005"))
    xdiff <- extent(bc.format)@xmax - extent(bc.format)@xmin
    ydiff <- extent(bc.format)@ymax - extent(bc.format)@ymin    
    shp.buffer[[1]] <- spTransform(gBuffer(bc.format,width=0.0009*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[2]] <- spTransform(gBuffer(bc.format,width=0.002*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[3]] <- spTransform(gBuffer(bc.format,width=0.004*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[4]] <- spTransform(gBuffer(bc.format,width=0.0055*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))
    shp.buffer[[5]] <- spTransform(gBuffer(bc.format,width=0.0065*(ydiff+xdiff)/2,byid=TRUE),CRS(proj))

  return(shp.buffer)
}


##-------------------------------------------------------------------------
make.single.plots <- function(past.box,proj.box,anoms.box,prct.box,
                              region,shape.dir,plot.dir,
                              var.name,scenario,type,ds.type,gcm,proj,
                              seas=NULL,rp.var=NULL,
                              past.int,proj.int,draft=TRUE) {

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS(proj))
  shp.buffer <- get.shp.buffer(region,shape.dir,proj)

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

    box.past.subset <- crop(past.box,extent(region.shp))
    region.past.subset <- mask(box.past.subset,region.shp)

    box.proj.subset <- crop(proj.box,extent(region.shp))
    region.proj.subset <- mask(box.proj.subset,region.shp)

    box.anoms.subset <- crop(anoms.box,extent(region.shp))
    region.anoms.subset <- mask(box.anoms.subset,region.shp)

    box.prct.subset <- crop(prct.box,extent(region.shp))
    region.prct.subset <- mask(box.prct.subset,region.shp)

  region.range <- range(as.matrix(region.past.subset),na.rm=T)
  box.range <-  range(as.matrix(past.box),na.rm=T)

  shared.range <- range(range(as.matrix(region.past.subset),na.rm=T),range(as.matrix(region.proj.subset),na.rm=T))
  shared.box <- range(range(as.matrix(past.box),na.rm=T),range(as.matrix(proj.box),na.rm=T))
  ##Past
  reg.ds.maps(past.box,region,region.range,box.range,
              var.name,type,ds.type,region.shp,shp.buffer,
              past.plot.file,past.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)

  ##Future
  region.range <- range(as.matrix(region.proj.subset),na.rm=T)
  box.range <-  range(as.matrix(proj.box),na.rm=T)

  reg.ds.maps(proj.box,region,region.range,box.range,
              var.name,type,ds.type,region.shp,shp.buffer,
              proj.plot.file,proj.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)

  region.range <- range(as.matrix(region.anoms.subset),na.rm=T)
  box.range <-  range(as.matrix(box.anoms.subset),na.rm=T)


  reg.ds.maps(anoms.box,region,region.range,box.range,
              var.name,type='anomaly',ds.type,region.shp,shp.buffer,
              anoms.plot.file,anoms.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,draft=FALSE)

  if (grepl("(pr|snm|snd|prcptot|rx|r9|snowdepth)", var.name)) {
    ##Percent Change

  region.range <- range(as.matrix(region.prct.subset),na.rm=T)
  box.range <-  range(as.matrix(box.prct.subset),na.rm=T)

    reg.ds.maps(prct.box,region,region.range,box.range,
                var.name,type='percent',ds.type,region.shp,shp.buffer,
                prct.plot.file,prct.plot.title,coords=NULL,proj=proj,
                overlays=overlays,leg.loc=leg.loc,draft=FALSE)
   }



}





make.ensemble.plots <- function(past.box,proj.box,anoms.box,prct.box,
                                region,shape.dir,plot.dir,
                                var.name,scenario,type,ds.type,proj,
                                seas=NULL,rp.var=NULL,
                                past.int,proj.int) {

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS(proj))
  overlays <- NULL ##spTransform(get.region.shape('metro_van_fraser_valley',shape.dir),CRS(proj))
  if (region=='metro_van' | region=='crd') {
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



  region.range <- range(as.matrix(past.box),na.rm=T)
  box.range <-  range(as.matrix(past.box),na.rm=T)

  shared.range <- range(range(as.matrix(past.box),na.rm=T),range(as.matrix(proj.box),na.rm=T))
  shared.box <- range(range(as.matrix(past.box),na.rm=T),range(as.matrix(proj.box),na.rm=T))

if(1==1) {
  reg.ds.maps(past.box,region,region.range,box.range,
              var.name,type,ds.type,region.shp,shp.buffer,
              past.plot.file,past.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)

  ##Future
  region.range <- range(as.matrix(proj.box),na.rm=T)
  box.range <-  range(as.matrix(proj.box),na.rm=T)

  reg.ds.maps(proj.box,region,region.range,box.range,
              var.name,type,ds.type,region.shp,shp.buffer,
              proj.plot.file,proj.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)
}

  region.range <- range(as.matrix(anoms.box),na.rm=T)
  box.range <-  range(as.matrix(anoms.box),na.rm=T)

print(region.range)
print(box.range)
  reg.ds.maps(anoms.box,region,region.range,box.range,
              var.name,type='anomaly',ds.type,region.shp,shp.buffer,
              anoms.plot.file,anoms.plot.title,coords=NULL,proj=proj,
              overlays=overlays,leg.loc=leg.loc,draft=FALSE)

  if (grepl("(pr|snm|snd|prcptot|rx|r9|snowdepth)", var.name)) {
    ##Percent Change

  region.range <- range(as.matrix(prct.box),na.rm=T)
  box.range <-  range(as.matrix(prct.box),na.rm=T)

    reg.ds.maps(prct.box,region,region.range,box.range,
                var.name,type='percent',ds.type,region.shp,shp.buffer,
                prct.plot.file,prct.plot.title,coords=NULL,proj=proj,
                overlays=overlays,leg.loc=leg.loc,draft=FALSE)
   }

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

gcm.list <- c('CNRM-CM5','CanESM2','ACCESS1-0','inmcm4')


seasonal.directories <- function(var.name,region,scenario,seas,time.dir,grep.name,meta) {
  print('Meta')
  print(meta)
  print(grep.name)
  if (grepl('ann',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/annual/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/',var.name,'/',tolower(seas),'/',sep='')
  }

  if (grepl('seas',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/seasonal/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/',var.name,'/',tolower(seas),'/',sep='')
  }
  if (grepl('mon',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/monthly/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/',var.name,'/',tolower(seas),'/',sep='')
  }
  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)  
}
 
plot.single.seasonal <- function(region,scenario,proj.int,
                                 var.name,grep.name,seas) {

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
  dirs <- seasonal.directories(var.name,region,scenario,seas,time.dir,grep.name,meta)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir

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

  
  ##-------------------------------------------------------------------------
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
  if (!file.exists(paste0(plot.dir,'separate/')))
    dir.create(paste0(plot.dir,'separate/'),recursive=TRUE)

  
  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon

  ##Bounding box data for rectangular region
  box.extent <- get.bounding.box(region,shape.dir)

  if (!file.exists(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_past_ensemble_',past.int,'.RData',sep=''))) {


    ##Prep for the ensemble files
    past.box.ens <- c()
    proj.box.ens <- c()
    anoms.box.ens <- c()
    prct.box.ens <- c()

    
    for (gcm in gcm.list) {
      print(gcm)
      clim.files <- list.files(path=paste0(read.dir,gcm),pattern=grep.name,full.name=TRUE)
      past.file <- clim.files[grep(past.int,clim.files)]
      proj.file <- clim.files[grep(proj.int,clim.files)]    

      past.box <- brick(past.file)
      print('Past box')
      proj.box <- brick(proj.file)
      print('Proj box')

        if (seas=='Annual') {
          past.box.seas <- subset(past.box,1)
          proj.box.seas <- subset(proj.box,1)
        } else {

          past.box.seas <- subset(past.box,seas.ix)
          proj.box.seas <- subset(proj.box,seas.ix)            
        }
      
      past.box.crop <- past.box.seas
      proj.box.crop <- proj.box.seas
      box.anoms <- proj.box.crop - past.box.crop
      box.prct <- (proj.box.crop - past.box.crop)/abs(past.box.crop)*100      

###      make.single.plots(past.box=past.box.crop,proj.box=proj.box.crop,
###                        anoms.box=box.anoms,prct.box=box.prct,
###                        region=region,shape.dir=shape.dir,plot.dir=paste0(plot.dir,'separate/'),
###                        var.name=var.name,scenario=scenario,proj=proj,
###                        type=type,ds.type=ds.type,gcm=gcm,seas=seas,
###                        past.int=past.int,proj.int=proj.int)

      ##Use projectRaster to change the box projection to BC Albers
      
      ##Make single GCM plots
      if (is.null(past.box.ens)) {
        past.box.ens <- past.box.crop
        proj.box.ens <- proj.box.crop
        anoms.box.ens <- box.anoms
        prct.box.ens <- box.prct
      } else {
        past.box.ens <- stack(past.box.ens,past.box.crop)
        proj.box.ens <- stack(proj.box.ens,proj.box.crop)
        anoms.box.ens <- stack(anoms.box.ens,box.anoms)
        prct.box.ens <- stack(prct.box.ens,box.prct)
      }
    }##GCM Loop
    
    past.box.ens <- calc(past.box.ens,mean)
    proj.box.ens <- calc(proj.box.ens,mean)
    anoms.box.ens <- calc(anoms.box.ens,mean)
    prct.box.ens <- calc(prct.box.ens,mean)

    ##Save the ensemble files
    if (!file.exists(paste(read.dir,'data_files/',sep='')))
      dir.create(paste(read.dir,'data_files/',sep=''),recursive=T)

    save(past.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_past_ensemble_box_',past.int,'.RData',sep=''))
    save(proj.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_proj_ensemble_box_',proj.int,'.RData',sep=''))
    save(anoms.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_anoms_ensemble_box_',past.int,'.RData',sep=''))
    save(prct.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_prct_ensemble_box_',proj.int,'.RData',sep=''))
    } else {
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_past_ensemble_box_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_proj_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_anoms_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_seasonal_prct_ensemble_box_',proj.int,'.RData',sep=''))
  }

    
    ##Use projectRaster to change the box projection to BC Albers
  print('Plotting')  
  ##Ensemble plots

  make.ensemble.plots(past.box=past.box.ens,proj.box=proj.box.ens,
                      anoms.box=anoms.box.ens,prct.box=prct.box.ens,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int)
}

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Return Periods
return.period.directories <- function(var.name,scenario,time.dir,region,meta) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/return_periods/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/return_periods/',var.name,'/',sep='')

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}


plot.return.periods <- function(region,scenario,proj.int,
                                var.name,rp) {
  print('Return Periods')
  meta <- get.region.names(region)  

  ##Change all of these
  rp.title <- get.var.title('pr',rp=rp)
  past.int <- '1971-2000'
  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]
  
  rp.var <- paste('rp.',rp,sep='')
  proj <- "+init=epsg:4326" ##BC Albers
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  
  ##--------------------------------------------------
  ds.type <- 'bccaq_gcm'
  type <- 'gcm'
  leg.loc <- get.leg.loc(region)  

  ##-------------------------------------------
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/shapefiles/',sep='')  

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon

  ##Bounding box data for rectangular region
  box.extent <- get.bounding.box(region,shape.dir)
  ##-------------------------------------------
  
  dirs <- return.period.directories(var.name,scenario,time.dir,region,meta)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)

  if (!file.exists(paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_past_ensemble_',past.int,'.RData',sep=''))) {
  
    ##Ensemble files
    past.box.ens <- c()
    proj.box.ens <- c()
    anoms.box.ens <- c()
    prct.box.ens <- c()
 
  ##Loop over GCMs
  for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    clim.files <- list.files(path=paste(read.dir,gcm,'/',sep=''),pattern=var.name,full.name=TRUE)
    clim.past.file <- clim.files[grep(past.int,clim.files)]
    clim.proj.file <- clim.files[grep(proj.int,clim.files)]  

    ##------------------------------------------------
    ##Outer raster box
    past.box <- brick(clim.past.file)
    past.box.crop <- subset(past.box,1) 

    proj.box <- brick(clim.proj.file)
    proj.box.crop <- subset(proj.box,1)

    box.anoms <- proj.box.crop - past.box.crop
    box.prct <- (proj.box.crop - past.box.crop)/abs(past.box.crop)*100      

    ##------------------------------------------------
    if (is.null(past.box.ens)) {
      past.box.ens <- past.box.crop
      proj.box.ens <- proj.box.crop
      anoms.box.ens <- box.anoms
      prct.box.ens <- box.prct
    } else {
        past.box.ens <- stack(past.box.ens,past.box.crop)
        proj.box.ens <- stack(proj.box.ens,proj.box.crop)
        anoms.box.ens <- stack(anoms.box.ens,box.anoms)
        prct.box.ens <- stack(prct.box.ens,box.prct)
    }
  }##GCM Loop

    past.box.ens <- calc(past.box.ens,mean)
    proj.box.ens <- calc(proj.box.ens,mean)
    anoms.box.ens <- calc(anoms.box.ens,mean)
    prct.box.ens <- calc(prct.box.ens,mean)

    ##Save the ensemble files
    if (!file.exists(paste(read.dir,'data_files/',sep='')))
      dir.create(paste(read.dir,'data_files/',sep=''),recursive=T)

    save(past.box.ens,file=paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_past_ensemble_box_',past.int,'.RData',sep=''))
    save(proj.box.ens,file=paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_proj_ensemble_box_',proj.int,'.RData',sep=''))
    save(anoms.box.ens,file=paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_anoms_ensemble_box_',past.int,'.RData',sep=''))
    save(prct.box.ens,file=paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_prct_ensemble_box_',proj.int,'.RData',sep=''))
    } else {
    load(paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_past_ensemble_box_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_proj_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_anoms_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_rp',rp,'_',region,'_return_periods_prct_ensemble_box_',proj.int,'.RData',sep=''))
  }


  ##Ensemble plots
  make.ensemble.plots(past.box=past.box.ens,proj.box=proj.box.ens,
                      anoms.box=anoms.box.ens,prct.box=prct.box.ens,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,rp.var=rp.var,
                      past.int=past.int,proj.int=proj.int)

}
##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Climdex 

climdex.directories <- function(var.name,scenario,time.dir,meta,draft) {

  ##Read directories
  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/climdex/',sep='')
  plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/climdex/',var.name,'/',sep='')

  if (grepl('(ffd|dd|s30)',var.name) & !grepl('ETCCDI',var.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/degree_days/',sep='')
    ##plot.dir <- paste('/storage/data/projects/rci/data/forestry/regional_summaries/region_maps/degree_days/',var.name,'/',sep='')
      plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/production/plots/degree_days/',var.name,'/',sep='')
  }

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}

plot.climdex <- function(region,scenario,proj.int,
                         var.name,seas) {
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
      
  ##-------------------------------------------------
  ##Directories

  shape.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/shapefiles/',sep='')
  dirs <- climdex.directories(var.name,scenario,time.dir,meta,draft=FALSE)
  ##dirs <- climdex.directories(var.name,scenario,region,time.dir)
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

  if (!file.exists(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))) {
 
    ##Prep for the ensemble files
    past.box.ens <- c()
    proj.box.ens <- c()
    
    for (gcm in gcm.list) {
      print(gcm)
      clim.files <- list.files(path=paste0(read.dir,gcm),pattern=paste('^',grep.name,sep=''),full.name=TRUE)
      past.file <- clim.files[grep(past.int,clim.files)]
      proj.file <- clim.files[grep(proj.int,clim.files)]

      ##Outer raster box
      past.box <- brick(past.file)
      print('Past box')
      proj.box <- brick(proj.file)
      print('Proj box')
      
      ##Seasonal subset
      if (seas != 'Annual') {
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
        past.box.avg <- subset(past.box,1)
        proj.box.avg <- subset(proj.box,1)
      }

      past.box.crop <- past.box.avg ##crop(past.box.avg,box.extent)
      proj.box.crop <- proj.box.avg ##crop(proj.box.avg,box.extent)
      box.anoms <- proj.box.crop - past.box.crop        
      box.prct <- (proj.box.crop - past.box.crop)/abs(past.box.crop)*100      
      
   
     ## make.single.plots(past.box=past.box.project,proj.box=proj.box.project,
     ##                   region=region,shape.dir=shape.dir,plot.dir=plot.dir,
     ##                   var.name=var.name,scenario=scenario,proj=proj,
     ##                   type=type,ds.type=ds.type,gcm=gcm,
     ##                   past.int=past.int,proj.int=proj.int)

      ##----------------------------------------------------------------------
      if (is.null(past.box.ens)) {
        past.box.ens <- past.box.crop
        proj.box.ens <- proj.box.crop
        anoms.box.ens <- box.anoms
        prct.box.ens <- box.prct
      } else {
        past.box.ens <- stack(past.box.ens,past.box.crop)
        proj.box.ens <- stack(proj.box.ens,proj.box.crop)
        anoms.box.ens <- stack(anoms.box.ens,box.anoms)
        prct.box.ens <- stack(prct.box.ens,box.prct)
      }
    }
    
    past.box.ens <- calc(past.box.ens,mean)
    proj.box.ens <- calc(proj.box.ens,mean)
    anoms.box.ens <- calc(anoms.box.ens,mean)
    prct.box.ens <- calc(prct.box.ens,mean)

    ##Save the ensemble files
    if (!file.exists(paste(read.dir,'data_files/',sep='')))
      dir.create(paste(read.dir,'data_files/',sep=''),recursive=T)

    save(past.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    save(proj.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    save(anoms.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_anoms_ensemble_box_',past.int,'.RData',sep=''))
    save(prct.box.ens,file=paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_prct_ensemble_box_',proj.int,'.RData',sep=''))
    } else {
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_anoms_ensemble_box_',proj.int,'.RData',sep=''))
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_prct_ensemble_box_',proj.int,'.RData',sep=''))
  }

  ##Ensemble plots
  make.ensemble.plots(past.box=past.box.ens,proj.box=proj.box.ens,
                      anoms.box=anoms.box.ens,prct.box=prct.box.ens,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int)
    
}

###***********************************************************************************
###***********************************************************************************

region.list <- 'van_coastal_health' ##'lionsgate_hospital' ####c('cariboo','kootenay','northeast','omineca','skeena','south','thompson','west')
scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
proj.intervals <- c('2041-2070') ##c('2011-2040','2041-2070','2071-2100')

source(paste0('/storage/home/ssobie/code/repos/assessments/van_coastal_health_map_support.r'),chdir=T)       
##source(paste0('/storage/home/ssobie/code/repos/assessments/lionsgate_hospital_map_support.r'),chdir=T)       
##source(paste0('/storage/home/ssobie/code/repos/assessments/willow_road_map_support.r'),chdir=T)       

##Single Season
run.season <- function() {
  var.name <- 'tasmax' ##'snowdepth'
  seas.list <-  c('Winter','Spring','Summer','Fall') ##'Winter' 
                ##c('January','February','March','April','May','June','July','August','September','October','November','December') ##
                ###'January' ##c('Winter','Spring','Summer','Fall','Annual') ##'APRIL1' 
  ##
  for (region in region.list) {
    print(region)
    ##grep.name <- paste0(var.name,'_',region,'_seasonal_average_climatology') ##'snowdepth_seas'
    grep.name <- paste0(var.name,'_van_coastal_health_seasonal_average_climatology') ##'snowdepth_seas'
    for (seas in seas.list) {
      print(seas)
      for (scenario in scenario.list) {
        print(scenario)
        for (proj.int in proj.intervals) {
          print(proj.int)
          print(region)
          plot.single.seasonal(region,scenario,proj.int,
                               var.name,grep.name,seas)
        }
      }
    }
  }
}

##Return Periods
run.return.periods <- function() {
  for (region in region.list) {
    var.list <- c('tasmin')
    for (var.name in var.list) {
      for (scenario in scenario.list) {
        for (proj.int in proj.intervals) {        
          print(region)
          plot.return.periods(region,scenario,proj.int,
                              var.name,rp='20')
        }
      }
    }
  }
}

##Climdex                
 
run.climdex <- function() {
  ##var.names <- c('r95daysETCCDI','r99daysETCCDI') ##  c('r95pETCCDI','r99pETCCDI','prcptotETCCDI')                 
  ##var.names <- c('cddETCCDI','cwdETCCDI','prcptotETCCDI','rx1dayETCCDI','rx5dayETCCDI','suETCCDI','tn10pETCCDI','tx90pETCCDI','tnnETCCDI','txxETCCDI') 
  ##var.names <- c('idETCCDI','suETCCDI','trETCCDI','tnnETCCDI','txxETCCDI','r95pETCCDI','r99pETCCDI')
  ##var.names <- c('r95pETCCDI','r99pETCCDI')
##'gslETCCDI',
##'cwdETCCDI',
  var.names <- c('fdETCCDI','suETCCDI','su30ETCCDI','idETCCDI','trETCCDI','txxETCCDI','txnETCCDI',
                 'tnnETCCDI','tnxETCCDI',
                 'dtrETCCDI','rx1dayETCCDI','rx5dayETCCDI','sdiiETCCDI',                  
                 'r10mmETCCDI','r20mmETCCDI','cddETCCDI','r95pETCCDI','r99pETCCDI',
                 'prcptotETCCDI')
##  var.names <- c('r95daysETCCDI','r99daysETCCDI')
  var.names <- 'sdiiETCCDI'
  seas.list <- c('Annual') ##,'Spring','Summer','Fall')
  for (region in region.list) {
    for (seas in seas.list) {
      for (var.name in var.names) {
        for (scenario in scenario.list) {
          for (proj.int in proj.intervals) {                
            print(region)
            plot.climdex(region,scenario,proj.int,
                         var.name,seas)
          }
        }
      }
    }
  }
}

##Degree Days
run.dd <- function() {
  var.names <- c('cdd','hdd','fdd','gdd')
  for (region in region.list) {
    for (var.name in var.names) {
      for (scenario in scenario.list) {
        for (proj.int in proj.intervals) {                
          print(region)
          plot.climdex(region,scenario,proj.int,
                     var.name,seas='Annual')
 
        }
      }
    }
  }
}