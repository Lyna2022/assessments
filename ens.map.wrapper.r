##Script to plot BC-wide maps of precip, climdex and return period data
##Should add the boundaries of the 8 resource regions to the plot as well.

library(sp)
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)

source('/storage/home/ssobie/code/repos/assessments/resource.region.map.support.r',chdir=T)       
source('/storage/home/ssobie/code/repos/assessments/bc_albers_map_support.r',chdir=T)

##-------------------------------------------------------------------------

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
                 tas='Average Temperature',
                 tmin='Min Temperature',
                 cdd='Cooling Degree Days',
                 hdd='Heating Degree Days',
                 gdd='Growing Degree Days',
                 fdd='Freezing Degree Days',
                 ffd='Frost Free Days',
                 s30='Summer Hot Days',
                 gslETCCDI='Growing Season Length',
                 cddETCCDI='Consecutive Dry Days',
                 cdd90ETCCDI='Consecutive Dry Days 90th%',
                 cddmaxETCCDI='Consecutive Dry Days Max',
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
                 tnnETCCDI='Coldest Nights',
                 tnxETCCDI='Max Tmin',         
                 txxETCCDI='Hottest Days',
                 txnETCCDI='Min Tmax',
                 pas='Precipitation as Snow',
                 snowdepth='Snowpack',
                 tasmax_annual_quantile_975='Warm Month Design Temperature 97.5%',
                 tasmax_annual_quantile_990='Warm Month Design Temperature 99.0%',
                 tasmax_annual_quantile_996='Warm Month Design Temperature 99.6%',
                 tasmin_annual_quantile_004='Cold Month Design Temperature 0.4%',
                 tasmin_annual_quantile_010='Cold Month Design Temperature 1.0%',
                 tasmin_annual_quantile_025='Cold Month Design Temperature 2.5%',
                 pr_maximum='Maximum Annual Total Precipitation',
                 pr_minimum='Minimum Annual Total Precipitation',
                 pr_standard_deviation='Standard Deviation of Total Precipitation') 

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

get.plot.ranges <- function(region,
                            past.box,proj.boxes) {

  box.extent <- get.crop.box(region)
  past.crop <- crop(past.box,box.extent)
  all.boxes <- c(past.box,proj.boxes)
  proj.box <- range(unlist(lapply(lapply(all.boxes,as.matrix),range,na.rm=T)))

  all.crops <- lapply(all.boxes,crop,box.extent)
  proj.range <- range(unlist(lapply(lapply(all.crops,as.matrix),range,na.rm=T)))

  proj.crops <- lapply(proj.boxes,crop,box.extent)
  proj.crops.anoms <- lapply(proj.crops,function(x,y){x-y},past.crop)
  crop.anoms <- range(unlist(lapply(lapply(proj.crops.anoms,as.matrix),range,na.rm=T)))
  proj.box.anoms <- lapply(proj.boxes,function(x,y){x-y},past.box)
  box.anoms <- range(unlist(lapply(lapply(proj.box.anoms,as.matrix),range,na.rm=T)))

  proj.crops.prct <- lapply(proj.crops,function(x,y){(x-y)/y*100},past.crop)
  crop.prct <- range(unlist(lapply(lapply(proj.crops.prct,as.matrix),range,na.rm=T)))

  proj.box.prct <- lapply(proj.boxes,function(x,y){(x-y)/y*100},past.box)
  box.prct <- range(unlist(lapply(lapply(proj.box.prct,as.matrix),range,na.rm=T)))

  rv <- list(crop.range=proj.range,box.range=proj.box,
             crop.anoms=crop.anoms,box.anoms=box.anoms,
             crop.prct=crop.prct,box.prct=box.prct)
  return(rv)
}


make.ensemble.plots <- function(past.box,proj.box,anoms.box,prct.box,all.ranges,
                                region,shape.dir,plot.dir,
                                var.name,scenario,proj,
                                ds.type,seas=NULL,rp.var=NULL,
                                past.int,proj.int) {

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS(proj))
  shp.buffer <- get.shp.buffer(region,shape.dir,proj)

  var.title <- get.var.title(var.name)                            
  leg.loc <- get.leg.loc(region)                              
  file.type <- get.file.type(region)

                              
  ##Individual models
  if (!is.null(seas)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',seas,'.',past.int,file.type,sep='')
    past.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',seas,'.',proj.int,file.type,sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',seas,'.abs.anoms.',past.int,'.',proj.int,file.type,sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',seas,'.prc.anoms.',past.int,'.',proj.int,file.type,sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', seas,' ',var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
  }
  ##Return Periods
  if (!is.null(rp.var)) {
    rp <- strsplit(rp.var,'\\.')[[1]][2]
    var.title <- get.var.title(var.name,rp)                            
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n ', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n Projected Change in ',var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n Projected Change in ', var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
  }  
##Climdex
  if (is.null(seas) & is.null(rp.var)) {
    past.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',past.int,'.png',sep='')
    past.plot.title <- paste(get.region.title(region),' \n',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')      
    proj.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste(get.region.title(region),' \n', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste(get.region.title(region),' \n',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,region,'.',var.name,'.',ds.type,'.ensemble.',scenario,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste(get.region.title(region),' \n', var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
  }

  box.extent <- get.crop.box(region)
  past.crop <- crop(past.box,box.extent)
  proj.crop <- crop(proj.box,box.extent)

  region.range <- range(as.matrix(past.crop),na.rm=T)
  box.range <-  range(as.matrix(past.box),na.rm=T)

  ##shared.range <- range(range(as.matrix(past.crop),na.rm=T),range(as.matrix(proj.crop),na.rm=T))
  ##shared.box <- range(range(as.matrix(past.box),na.rm=T),range(as.matrix(proj.box),na.rm=T))

  shared.range <- all.ranges$crop.range
  shared.box <- all.ranges$box.range

if(1==1) {
  print('Past')
  reg.ds.maps(past.box,region,region.range,box.range,
              var.name,type='past',ds.type,region.shp,
              past.plot.file,past.plot.title,
              add.overlays=add.overlays,
              add.cities=add.cities,add.districts=add.districts,
              add.graticules=add.graticules,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)
##browser()
  ##Future
  region.range <- range(as.matrix(proj.crop),na.rm=T)
  box.range <-  range(as.matrix(proj.box),na.rm=T)

  print('Future')
  reg.ds.maps(proj.box,region,region.range,box.range,
              var.name,type='proj',ds.type,region.shp,
              proj.plot.file,proj.plot.title,
              add.overlays=add.overlays,
              add.cities=add.cities,add.districts=add.districts,
              add.graticules=add.graticules,leg.loc=leg.loc,
              shared.range=shared.range,shared.box=shared.box,draft=FALSE)

}
if (1==1) {
  region.range <- range(as.matrix(crop(anoms.box,box.extent)),na.rm=T)
  box.range <-  range(as.matrix(anoms.box),na.rm=T)

print(region.range)
print(box.range)
  print('Anomalies')
  reg.ds.maps(anoms.box,region,region.range,box.range,
              var.name,type='anomaly',ds.type,region.shp,
             anoms.plot.file,anoms.plot.title,
              add.overlays=add.overlays,
              add.cities=add.cities,add.districts=add.districts,
              add.graticules=add.graticules,leg.loc=leg.loc,
              shared.range=all.ranges$crop.anoms,shared.box=all.ranges$box.anoms,
              draft=FALSE)


  if (grepl("(pr|snm|snd|prcptot|rx|r9|snowdepth)", var.name)) {
    ##Percent Change

  region.range <- range(as.matrix(crop(prct.box,box.extent)),na.rm=T)
  box.range <-  range(as.matrix(prct.box),na.rm=T)
  print('Percent')
    reg.ds.maps(prct.box,region,region.range,box.range,
                var.name,type='percent',ds.type,region.shp,
                prct.plot.file,prct.plot.title,
                add.overlays=add.overlays,
                add.cities=add.cities,add.districts=add.districts,
                add.graticules=add.graticules,leg.loc=leg.loc,
                shared.range=all.ranges$crop.prct,shared.box=all.ranges$box.prct,
                draft=FALSE)
   }
}
##browser()
}##Ensemble plot function

##-------------------------------------------------------------------------
##-------------------------------------------------------------------------

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


seasonal.directories <- function(var.name,region,scenario,seas,time.dir,grep.name,meta) {
  print('Meta')
  print(meta)
  print(grep.name)
  if (grepl('ann',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/annual/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',var.name,'/',tolower(seas),'/',sep='')
  }

  if (grepl('(winter|spring|summer|fall)',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/seasonal/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',var.name,'/',tolower(seas),'/',sep='')
  }
  if (grepl('mon',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/monthly/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/',var.name,'/',tolower(seas),'/',sep='')
  }
  if (grepl('(maximum|minimum|standard)',grep.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/annual/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/build_code/pr/',sep='')
  }

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)  
}


 
plot.single.seasonal <- function(region,scenario,proj.int,proj.intervals,
                                 var.name,grep.name,seas) {

  meta <- get.region.names(region)  
  print('Single Season')
  scenario <- 'rcp85'
  past.int <- '1971-2000'
  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]

  ds.type <- 'bccaq2.gcm'
  proj <- "+init=epsg:4326"
  seas.fx <- mean

  leg.loc <- get.leg.loc(region)
  var.title <- get.var.title(var.name)
  print(paste(toupper(var.name),' ',scenario,' ',time.dir,' ',seas,sep=''))
  
  ##Directories
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/shapefiles/',meta$subset,'/',sep='')

  dirs <- seasonal.directories(var.name,region,scenario,seas,time.dir,grep.name,meta)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir

  ##-------------------------------------------------------------------------
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon

  clim.files <- list.files(path=paste0(read.dir,'ENSEMBLE'),pattern=grep.name,full.name=TRUE)
  past.file <- clim.files[grep(past.int,clim.files)]
  proj.file <- clim.files[grep(proj.int,clim.files)]    
  proj.files <- clim.files[grepl(paste0('(',paste0(proj.intervals,collapse='|'),')'),clim.files)]

  print(clim.files)

  past.box <- brick(past.file)
  print('Past box')
  proj.box <- brick(proj.file)
  proj.boxes <- lapply(proj.files,brick)
  print('Proj box')

  past.box.seas <- subset(past.box,1)
  proj.box.seas <- subset(proj.box,1)
  proj.boxes.seas <- lapply(proj.boxes,subset,1)

  all.ranges <- get.plot.ranges(region,past.box.seas,proj.boxes.seas)
        
  box.anoms <- proj.box.seas - past.box.seas
  box.prct <- (proj.box.seas - past.box.seas)/abs(past.box.seas)*100      

  ##Use projectRaster to change the box projection to BC Albers
  print('Plotting')  



  ##Ensemble plots
  make.ensemble.plots(past.box=past.box.seas,proj.box=proj.box.seas,
                      anoms.box=box.anoms,prct.box=box.prct,all.ranges=all.ranges,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int)
}

##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Return Periods
return.period.directories <- function(var.name,scenario,time.dir,region,meta) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/return_periods/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/return_periods/',var.name,'/',sep='')

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}


plot.return.periods <- function(region,scenario,proj.int,proj.intervals,
                                var.name,rp) {
  print('Return Periods')
  meta <- get.region.names(region)  
  scenario <- 'rcp85'
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
  ds.type <- 'bccaq2.gcm'
  leg.loc <- get.leg.loc(region)  

  ##-------------------------------------------
  shape.dir <- paste('/storage/data/projects/rci/data/assessments/shapefiles/',meta$subset,'/',sep='')  

  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326")) ##Keep this projection to extract the data from lat/lon
  ##-------------------------------------------
  
  dirs <- return.period.directories(var.name,scenario,time.dir,region,meta)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir
  
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
    
    all.files <- list.files(path=paste(read.dir,'ENSEMBLE/',sep=''),pattern=var.name,full.name=TRUE)
    clim.files <- all.files[grep(paste0('RP',rp,'_'),all.files)]
    print(clim.files)

    past.file <- clim.files[grep(past.int,clim.files)]
    proj.file <- clim.files[grep(proj.int,clim.files)]  
    proj.files <- clim.files[grepl(paste0('(',paste0(proj.intervals,collapse='|'),')'),clim.files)]   
    past.box <- brick(past.file)
    print('Past box')
    proj.box <- brick(proj.file)
    proj.boxes <- lapply(proj.files,brick)
    print('Proj box')

    past.box.seas <- subset(past.box,1)
    proj.box.seas <- subset(proj.box,1)
    proj.boxes.seas <- lapply(proj.boxes,subset,1)

    all.ranges <- get.plot.ranges(region,past.box.seas,proj.boxes.seas)
      
    box.anoms <- proj.box.seas - past.box.seas
    box.prct <- (proj.box.seas - past.box.seas)/abs(past.box.seas)*100      

    ##Use projectRaster to change the box projection to BC Albers
    print('Plotting')  

  ##Ensemble plots
  make.ensemble.plots(past.box=past.box.seas,proj.box=proj.box.seas,
                      anoms.box=box.anoms,prct.box=box.prct,all.ranges=all.ranges,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      ds.type=ds.type,rp.var=rp.var,
                      past.int=past.int,proj.int=proj.int)
}
##------------------------------------------------------------------------------------------
##------------------------------------------------------------------------------------------
##Climdex 

climdex.directories <- function(var.name,scenario,time.dir,meta,draft) {

  ##Read directories
  read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/climdex/',sep='')
  plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/climdex/',var.name,'/',sep='')

  if (grepl('(ffd|dd)',var.name) & !grepl('ETCCDI',var.name)) {
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/degree_days/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/degree_days/',var.name,'/',sep='')
  }

  if (grepl('quantile',var.name) & !grepl('ETCCDI',var.name)) {
    var.sub <- strsplit(var.name,'_')[[1]][1]
    read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',meta$subset,'/',scenario,'/annual_quantiles/',sep='')
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$area,'/plots/build_code/',var.sub,'/',sep='')
  }

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)
}

plot.climdex <- function(region,scenario,proj.int,proj.intervals,
                         var.name,seas) {
  meta <- get.region.names(region)

  print('Climdex')
  ##----
  scenario <- 'rcp85'
  past.int <- '1971-2000'
  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]
  
  proj <- "+init=epsg:4326" ##BC Albers
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  
  print(paste(toupper(var.name),' ',scenario,' ',time.dir,sep=''))
  
  ds.type <- 'bccaq2.gcm'

  var.title <- get.var.title(var.name)
  grep.name <- var.name
      
  ##-------------------------------------------------
  ##Directories

  shape.dir <- paste('/storage/data/projects/rci/data/assessments/shapefiles/',meta$subset,'/',sep='')
  dirs <- climdex.directories(var.name,scenario,time.dir,meta,draft=FALSE)
  read.dir <- dirs$read.dir
  plot.dir <- dirs$plot.dir
  print(read.dir)
  print(plot.dir)
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
  ##-------------------------------------------------
    
  leg.loc <- get.leg.loc(region)
  print(region)
  print(shape.dir)
  region.shp <- spTransform(get.region.shape(region,shape.dir),CRS("+init=epsg:4326"))
  box.function <- function(x,fac,fx) {
    return(tapply(x,fac,fx))
  }

  ##Seasonal subset
  if (seas != 'Annual') {
     all.files <- list.files(path=paste0(read.dir,'ENSEMBLE'),pattern=paste('^',grep.name,sep=''),full.name=TRUE)
     clim.files <- all.files[grep(tolower(seas),all.files)]
     past.file <- clim.files[grep(past.int,clim.files)]
     proj.file <- clim.files[grep(proj.int,clim.files)]
     proj.files <- clim.files[grepl(paste0('(',paste0(proj.intervals,collapse='|'),')'),clim.files)]
     print(clim.files)

     ##Outer raster box
     past.box <- brick(past.file)
     print('Past box')
     proj.box <- brick(proj.file)
     proj.boxes <- lapply(proj.files,brick)
     print('Proj box') 
  } else {
     all.files <- list.files(path=paste0(read.dir,'ENSEMBLE'),pattern=paste('^',grep.name,sep=''),full.name=TRUE)
     clim.files <- all.files[grep('annual',all.files)]
     past.file <- clim.files[grep(past.int,clim.files)]
     proj.file <- clim.files[grep(proj.int,clim.files)]
     proj.files <- clim.files[grepl(paste0('(',paste0(proj.intervals,collapse='|'),')'),clim.files)]   
     print(clim.files)

     ##Outer raster box
     past.box <- brick(past.file)
     print('Past box')
     proj.box <- brick(proj.file)
     proj.boxes <- lapply(proj.files,brick)
     print('Proj box')
  }
  past.box.avg <- subset(past.box,1)
  proj.box.avg <- subset(proj.box,1)
  proj.boxes.seas <- lapply(proj.boxes,subset,1)
  box.anoms <- proj.box.avg - past.box.avg
  box.prct <- (proj.box.avg - past.box.avg)/abs(past.box.avg)*100      

  all.ranges <- get.plot.ranges(region,past.box.avg,proj.boxes.seas)

  ##Ensemble plots
  make.ensemble.plots(past.box=past.box.avg,proj.box=proj.box.avg,
                      anoms.box=box.anoms,prct.box=box.prct,all.ranges=all.ranges,
                      region=region,shape.dir=shape.dir,plot.dir=plot.dir,
                      var.name=var.name,scenario=scenario,proj=proj,
                      ds.type=ds.type,seas=seas,
                      past.int=past.int,proj.int=proj.int)
    
}

###***********************************************************************************
###***********************************************************************************

region <- 'vancouver_island'
readloc <- 'vancouver_island'
type <- 'return_periods'

##args <- commandArgs(trailingOnly=TRUE)
##for(i in 1:length(args)){
##    eval(parse(text=args[[i]]))
##}

print(region)
print(readloc)
print(type)

source(paste0('/storage/home/ssobie/code/repos/assessments/',region,'_map_support.r'),chdir=T)       
##source(paste0('/storage/home/ssobie/code/repos/assessments/okanagan_map_support.r'),chdir=T)       
##source(paste0('/storage/home/ssobie/code/repos/assessments/vancouver_island_map_support.r'),chdir=T)       
scenario <- 'rcp85'

proj.intervals <- '2041-2070' ##c('2041-2070','2071-2100') ##c('2011-2040','2041-2070','2071-2100')

##Single Season
run.season <- function(region) {

  ##Annual Maps
  var.list <- c('pr','tasmax','tasmin','tas')
  ##var.list <- 'tas'
  for (var.name in var.list) {

     grep.name <- paste0(var.name,'_',readloc,'_annual_average_climatology')
     ##grep.name <- paste0(var.name,'_average_annual_climatology')
     for (proj.int in proj.intervals) {
        print(proj.int)
        print(region)
        plot.single.seasonal(region,scenario,proj.int,proj.intervals,
                             var.name,grep.name,'Annual')

     }
  }

  ##Seasonal
  ##c('January','February','March','April','May','June','July','August','September','October','November','December') ##
  seas.list <-  c('Winter','Spring','Summer','Fall')

  for (var.name in var.list) {
    for (seas in seas.list) {
      grep.name <- paste0(var.name,'_',readloc,'_',tolower(seas),'_average_climatology')  
      print(seas)
      for (proj.int in proj.intervals) {
         print(proj.int)
         plot.single.seasonal(region,scenario,proj.int,proj.intervals,
                               var.name,grep.name,seas)
      }
    }
  }
}

##Return Periods
run.return.periods <- function(region) {
  var.list <- 'pr' ##c('pr','tasmax','tasmin')
  for (var.name in var.list) {
     for (proj.int in proj.intervals) {        
       print(region)
       plot.return.periods(region,scenario,proj.int,proj.intervals,
                           var.name,rp='20')
     }
  }
}

##Climdex                
run.climdex <- function(region) {

##Annual Climdex Maps
  var.names <- c('fdETCCDI','suETCCDI','su30ETCCDI','idETCCDI','trETCCDI','gslETCCDI',
                 'txxETCCDI','txnETCCDI','tnnETCCDI','tnxETCCDI','dtrETCCDI',
                 'rx1dayETCCDI','rx5dayETCCDI',
                 'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI',
                 'cwdETCCDI','cddETCCDI', ##'cdd90ETCCDI','cddmaxETCCDI',
                 'prcptotETCCDI','r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI')
   ##var.names <- c('trETCCDI','dtrETCCDI')
   ##var.names <- c('rx1dayETCCDI','suETCCDI','fdETCCDI')
   ##var.names <- c('suETCCDI','su30ETCCDI','tnnETCCDI','idETCCDI','rx5dayETCCDI','gslETCCDI')
   for (var.name in var.names) {
      for (proj.int in proj.intervals) {                
         plot.climdex(region,scenario,proj.int,proj.intervals,
                         var.name,seas='Annual')        
      }
   }
##browser()
##Seasonal Climdex Maps
  var.names <- c('txxETCCDI','txnETCCDI','tnnETCCDI','tnxETCCDI','dtrETCCDI',
                 'rx1dayETCCDI','rx5dayETCCDI')                
##  var.names <- 'dtrETCCDI'
  seas.list <-  c('Winter','Spring','Summer','Fall')
  for (var.name in var.names) {
     for (seas in seas.list) {
        for (proj.int in proj.intervals) {                
           plot.climdex(region,scenario,proj.int,proj.intervals,
                         var.name,seas)
        }
     }
  }
}

##Degree Days
run.dd <- function(region) {
  var.names <- c('cdd','hdd','fdd','gdd')
    for (var.name in var.names) {
        for (proj.int in proj.intervals) {                
          plot.climdex(region,scenario,proj.int,proj.intervals,
                     var.name,seas='Annual') 
        }
    }
}

##Building Code
run.quantiles <- function(region) {
  var.names <- c('tasmax_annual_quantile_975','tasmax_annual_quantile_990','tasmax_annual_quantile_996',
                 'tasmin_annual_quantile_004','tasmin_annual_quantile_010','tasmin_annual_quantile_025')
    for (var.name in var.names) {
        for (proj.int in proj.intervals) {                
          plot.climdex(region,scenario,proj.int,proj.intervals,
                     var.name,seas='Annual')
        }
    }
}

run.pr.vars <- function(region) {
  ##Annual Maps
  var.list <- c('pr_maximum','pr_minimum','pr_standard_deviation')
  for (var.name in var.list) {      
     grep.name <- var.name
     for (proj.int in proj.intervals) {
        print(proj.int)
        print(region)
        plot.single.seasonal(region,scenario,proj.int,proj.intervals,
                             var.name,grep.name,'Annual')
     }
  }
}

##-----------------------------------------------------------
##-----------------------------------------------------------

print(type)

if (type=='season') {
  print('Running Seasons')
  run.season(region)
}

if (type=='climdex') {
  print('Running CLIMDEX')
  run.climdex(region)
}

if (type=='degree_days') {
  print('Running Degree Days')
  run.dd(region)
}
if (type=='return_periods') {
  print('Running RPs')
  run.return.periods(region)
}

if (type=='quantiles') {
  print('Running Quantiles')
  run.quantiles(region)
}

if (type=='pr.vars') {
  print('Running PR Vars')
  run.pr.vars(region)
}

