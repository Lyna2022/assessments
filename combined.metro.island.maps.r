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


##-------------------------------------------------------------------------

make.lm.plots <- function(metro.past,metro.proj,
                          metro.past.box,metro.proj.box,
                          cvrd.past,cvrd.proj,
                          cvrd.past.box,cvrd.proj.box,
                          crd.past,crd.proj,
                          crd.past.box,crd.proj.box,                
                          bc.past,bc.proj,
                          bc.past.box,bc.proj.box,                
                          plot.dir,
                          var.name,scenario,type,ds.type,proj,
                          seas=NULL,rp.var=NULL,
                          past.int,proj.int,draft) {

  shape.dir <- paste('/storage/data/projects/rci/data/assessments/metro_van/shapefiles/',sep='')
  metro.shp <- spTransform(get.region.shape('metro_van',shape.dir),CRS(proj))

  shape.dir <- paste('/storage/data/projects/rci/data/assessments/cvrd/shapefiles/',sep='')
  cvrd.shp <- spTransform(get.region.shape('cvrd',shape.dir),CRS(proj))

  shape.dir <- paste('/storage/data/projects/rci/data/assessments/crd/shapefiles/',sep='')
  crd.shp <- spTransform(get.region.shape('crd',shape.dir),CRS(proj))

  var.title <- get.var.title(var.name)                            
  leg.loc <- 'bottomright'                              
                              
  ##Individual models
  if (!is.null(seas)) {
    past.plot.file <- paste(plot.dir,'lower_mainland','.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.',past.int,'.png',sep='')
    past.plot.title <- paste('Lower Mainland',' \n', seas,' ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,'lower_mainland','.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste('Lower Mainland',' \n', seas,' ',var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,'lower_mainland','.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste('Lower Mainland',' \n', seas,' ',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,'lower_mainland','.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',seas,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste('Lower Mainland',' \n', seas,' ',var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,'lower_mainland','.',var.name,'.',ds.type,'.ensemble.',type,'.',seas,'.increases.png',sep='')
    num.plot.title <- paste('Lower Mainland',' ', seas,' \n Number of GCMs with increasing ',var.title,
                        '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
  }
  ##Return Periods
  if (!is.null(rp.var)) {
    var.title <- get.var.title(var.name,rp=20)                            
    past.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste('Lower Mainland',' \n ',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')
    proj.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste('Lower Mainland',' \n ', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste('Lower Mainland',' \n Projected Change in ',var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste('Lower Mainland',' \n Projected Change in ', var.title,' \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',rp.var,'.',ds.type,'.ensemble.',type,'.increases.png',sep='')
    num.plot.title <- paste('Lower Mainland',' \n Number of GCMs with increasing 20-Year ',var.title,
                            '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')        
  }  
##Climdex
  if (is.null(seas) & is.null(rp.var)) {
    past.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',past.int,'.png',sep='')
    past.plot.title <- paste('Lower Mainland',' \n',var.title,' Past \n CMIP5 Ensemble ',toupper(scenario),' (',past.int,')',sep='')      
    proj.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.',proj.int,'.png',sep='')
    proj.plot.title <- paste('Lower Mainland',' \n', var.title,' Projections \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    anoms.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.abs.anoms.',past.int,'.',proj.int,'.png',sep='')
    anoms.plot.title <- paste('Lower Mainland',' \n',var.title,' Anomalies \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    prct.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.prc.anoms.',past.int,'.',proj.int,'.png',sep='')
    prct.plot.title <- paste('Lower Mainland',' \n', var.title,' Percent Change \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')
    num.plot.file <- paste(plot.dir,'lower_mainland.',var.name,'.',ds.type,'.ensemble.',scenario,'.',type,'.increases.png',sep='')
    num.plot.title <- paste('Lower Mainland',' \n Number of GCMs with increasing ',var.title,
                            '  \n CMIP5 Ensemble ',toupper(scenario),' (',proj.int,')',sep='')    

  }

  shared.range <- range(c(range(metro.past,na.rm=T),range(metro.proj,na.rm=T),
                          range(cvrd.past,na.rm=T),range(cvrd.proj,na.rm=T),
                          range(crd.past,na.rm=T),range(crd.proj,na.rm=T),
                          range(bc.past,na.rm=T),range(bc.proj,na.rm=T)),na.rm=T)

  shared.box <- range(c(metro.past.box@data@min,metro.past.box@data@max,
                        metro.proj.box@data@min,metro.proj.box@data@max,
                        cvrd.past.box@data@min,cvrd.past.box@data@max,
                        cvrd.proj.box@data@min,cvrd.proj.box@data@max,
                        crd.past.box@data@min,crd.past.box@data@max,
                        crd.proj.box@data@min,crd.proj.box@data@max,
                        bc.past.box@data@min,bc.past.box@data@max,
                        bc.proj.box@data@min,bc.proj.box@data@max),na.rm=T)
if (1==1) {
  lm.ds.maps(metro.past,metro.past.box,
             cvrd.past,cvrd.past.box,
             crd.past,crd.past.box,
             bc.past,bc.past.box,
             type,ds.type,
             metro.shp,cvrd.shp,crd.shp,
             past.plot.file,past.plot.title,proj=proj,
             overlays=overlays,leg.loc=leg.loc,
             shared.range=shared.range,shared.box=shared.box)

  ##Future
  lm.ds.maps(metro.proj,metro.proj.box,
             cvrd.proj,cvrd.proj.box,
             crd.proj,crd.proj.box,
             bc.proj,bc.proj.box,
             type,ds.type,
             metro.shp,cvrd.shp,crd.shp,
             proj.plot.file,proj.plot.title,proj=proj,
             overlays=overlays,leg.loc=leg.loc,
             shared.range=shared.range,shared.box=shared.box)

  metro.anoms <- metro.proj - metro.past
  cvrd.anoms <- cvrd.proj - cvrd.past
  crd.anoms <- crd.proj - crd.past
  bc.anoms <- bc.proj - bc.past

  metro.anoms.box <- metro.proj.box - metro.past.box
  cvrd.anoms.box <- cvrd.proj.box - cvrd.past.box
  crd.anoms.box <- crd.proj.box - crd.past.box
  bc.anoms.box <- bc.proj.box - bc.past.box

  lm.ds.maps(metro.anoms,metro.anoms.box,
             cvrd.anoms,cvrd.anoms.box,
             crd.anoms,crd.anoms.box,
             bc.anoms,bc.anoms.box,
             type='anomaly',ds.type,
             metro.shp,cvrd.shp,crd.shp,
             anoms.plot.file,anoms.plot.title,proj=proj,
             overlays=overlays,leg.loc=leg.loc,
             shared.range=NULL,shared.box=NULL)
}
 if (grepl("(pr|snm|snd|prcptot|rx|r9|snowdepth)", var.name)) {
    ##Percent Change

      metro.prct <- (metro.proj - metro.past)/abs(metro.past)*100
      cvrd.prct <- (cvrd.proj - cvrd.past)/abs(cvrd.past)*100
      crd.prct <- (crd.proj - crd.past)/abs(crd.past)*100
      bc.prct <- (bc.proj - bc.past)/abs(bc.past)*100

      metro.prct.box <- (metro.proj.box - metro.past.box)/abs(metro.past.box)*100
      cvrd.prct.box <- (cvrd.proj.box - cvrd.past.box)/abs(cvrd.past.box)*100
      crd.prct.box <- (crd.proj.box - crd.past.box)/abs(crd.past.box)*100
      bc.prct.box <- (bc.proj.box - bc.past.box)/abs(bc.past.box)*100

  lm.ds.maps(metro.prct,metro.prct.box,
             cvrd.prct,cvrd.prct.box,
             crd.prct,crd.prct.box,
             bc.prct,bc.prct.box,
             type='percent',ds.type,
             metro.shp,cvrd.shp,crd.shp,
             prct.plot.file,prct.plot.title,proj=proj,
             overlays=overlays,leg.loc=leg.loc,
             shared.range=NULL,shared.box=NULL)
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


seasonal.directories <- function(var.name,scenario,seas,time.dir,grep.name,meta,draft) {
  
  if (grepl('seas',grep.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/',scenario,'/seasonal/',sep='')
  if (grepl('mon',grep.name))
    read.dir <- paste('/storage/data/scratch/ssobie/bccaq_gcm_',meta$subset,'_subset/',scenario,'/monthly/',sep='')

  if (grepl('(seas|apr1|may1)',grep.name))
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/',var.name,'/',tolower(seas),'/',sep='')  
  if (grepl('mon',grep.name))
    plot.dir <- paste('/storage/data/projects/rci/data/assessments/',meta$region,'/production/plots/',var.name,'/monthly/',sep='')    

  rv <- list(read.dir=read.dir,
             plot.dir=plot.dir)
  return(rv)  
}
 
plot.single.seasonal <- function(scenario,proj.int,
                                 var.name,grep.name,seas,draft) {
  
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
  leg.loc <- 'bottomright'
  var.title <- get.var.title(var.name)
  print(paste(toupper(var.name),' ',scenario,' ',time.dir,' ',seas,sep=''))
  
  ##Directories

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
  

  ##Metro Van
  region <- 'metro_van'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/rcp85/seasonal/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    metro.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    metro.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    metro.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    metro.proj.box <- proj.box.ens.project    

  ##CVRD
  region <- 'cvrd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/seasonal/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    cvrd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    cvrd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    cvrd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    cvrd.proj.box <- proj.box.ens.project    

  ##CRD
  region <- 'crd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/seasonal/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    crd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    crd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    crd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    crd.proj.box <- proj.box.ens.project    

  plot.dir <- paste('/storage/data/projects/rci/data/assessments/lower_mainland/production/plots/',var.name,'/seasonal/',sep='')      
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
    
  print('Plotting')

  ##Ensemble plots
  make.lm.plots(metro.past=metro.past,metro.proj=metro.proj,
                      metro.past.box=metro.past.box,metro.proj.box=metro.proj.box,
                      cvrd.past=cvrd.past,cvrd.proj=cvrd.proj,
                      cvrd.past.box=cvrd.past.box,cvrd.proj.box=cvrd.proj.box,
                      crd.past=crd.past,crd.proj=crd.proj,
                      crd.past.box=crd.past.box,crd.proj.box=crd.proj.box,
                      plot.dir=plot.dir,
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

plot.return.periods <- function(scenario,proj.int,
                                var.name,rp,draft) {
  
  print('Return Periods')
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  past.int <- '1971-2000'
  rp.title <- get.var.title('pr',rp=rp)
  time.intervals <- '2041-2070' ##c('2011-2040','2041-2070','2071-2100')
  time.dirs <- '2050s' ##c('2020s','2050s','2080s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]

  rp.var <- paste('rp.',rp,sep='')

  proj <- "+init=epsg:4326"
  seas.fx <- mean
  if (var.name=='pr')
    seas.fx <- sum

  ds.type <- 'bccaq_gcm'
  type <- 'gcm'
  leg.loc <- 'bottomright'
  var.title <- get.var.title(var.name)

  
  ##-------------------------------------------------------------------------
  
  ##Metro Van
  region <- 'metro_van'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/rcp85/return_periods/'
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    metro.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    metro.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    metro.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    metro.proj.box <- proj.box.ens.project    

  ##CVRD
  region <- 'cvrd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/return_periods/'
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    cvrd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    cvrd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    cvrd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    cvrd.proj.box <- proj.box.ens.project    

  ##CRD
  region <- 'crd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/return_periods/'
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    crd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    crd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    crd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    crd.proj.box <- proj.box.ens.project    

  ##BC
  region <- 'bc'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/rcp85/return_periods/'
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_',past.int,'.RData',sep=''))
    bc.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_',proj.int,'.RData',sep=''))
    bc.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_past_ensemble_box_',past.int,'.RData',sep=''))
    bc.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_RP20_',region,'_proj_ensemble_box_',proj.int,'.RData',sep=''))
    bc.proj.box <- proj.box.ens.project    

  plot.dir <- paste('/storage/data/projects/rci/data/assessments/lower_mainland/production/plots/',var.name,'/return_periods/',sep='')      
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
    
  print('Plotting')

  ##Ensemble plots
  make.lm.plots(metro.past=metro.past,metro.proj=metro.proj,
                      metro.past.box=metro.past.box,metro.proj.box=metro.proj.box,
                      cvrd.past=cvrd.past,cvrd.proj=cvrd.proj,
                      cvrd.past.box=cvrd.past.box,cvrd.proj.box=cvrd.proj.box,
                      crd.past=crd.past,crd.proj=crd.proj,
                      crd.past.box=crd.past.box,crd.proj.box=crd.proj.box,
                      bc.past=bc.past,bc.proj=bc.proj,
                      bc.past.box=bc.past.box,bc.proj.box=bc.proj.box,
                      plot.dir=plot.dir,
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

plot.climdex <- function(scenario,proj.int,
                         var.name,seas) {

  print('Climdex')
  ##----
  past.int <- '1971-2000'
  time.intervals <- c('2011-2040','2041-2070','2071-2100')
  time.dirs <- c('2050s')
  time.dir <- time.dirs[grep(proj.int,time.intervals)]
  
  proj <- "+init=epsg:4326" ##BC Albers
  if (scenario=='rcp26')
    gcm.list <- rcp26.list
  
  ds.type <- 'bccaq_gcm'
  type <- 'gcm'

  ##-------------------------------------------------------------------------
  ##Metro Van
  region <- 'metro_van'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/rcp85/climdex/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    metro.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    metro.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    metro.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    metro.proj.box <- proj.box.ens.project    

  ##CVRD
  region <- 'cvrd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/climdex/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    cvrd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    cvrd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    cvrd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    cvrd.proj.box <- proj.box.ens.project    

  ##CRD
  region <- 'crd'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_south_island_subset/rcp85/climdex/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    crd.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    crd.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    crd.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    crd.proj.box <- proj.box.ens.project    

  ##BC
  region <- 'bc'
  read.dir <-'/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/rcp85/climdex/'
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_',past.int,'.RData',sep=''))
    bc.past <- clim.past.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_',proj.int,'.RData',sep=''))
    bc.proj <- clim.proj.ens
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_past_ensemble_box_',past.int,'.RData',sep=''))
    bc.past.box <- past.box.ens.project
    load(paste(read.dir,'data_files/',var.name,'_',seas,'_',region,'_climdex_proj_ensemble_box_',proj.int,'.RData',sep=''))
    bc.proj.box <- proj.box.ens.project    

  plot.dir <- paste('/storage/data/projects/rci/data/assessments/lower_mainland/production/plots/',var.name,'/climdex/',sep='')      
  if (!file.exists(plot.dir))
    dir.create(plot.dir,recursive=TRUE)
    
  print('Plotting')

  ##Ensemble plots
  make.lm.plots(metro.past=metro.past,metro.proj=metro.proj,
                      metro.past.box=metro.past.box,metro.proj.box=metro.proj.box,
                      cvrd.past=cvrd.past,cvrd.proj=cvrd.proj,
                      cvrd.past.box=cvrd.past.box,cvrd.proj.box=cvrd.proj.box,
                      crd.past=crd.past,crd.proj=crd.proj,
                      crd.past.box=crd.past.box,crd.proj.box=crd.proj.box,
                      bc.past=bc.past,bc.proj=bc.proj,
                      bc.past.box=bc.past.box,bc.proj.box=bc.proj.box,
                      plot.dir=plot.dir,seas=NULL,
                      var.name=var.name,scenario=scenario,proj=proj,
                      type=type,ds.type=ds.type,
                      past.int=past.int,proj.int=proj.int,draft=draft)
}

###***********************************************************************************
###***********************************************************************************

source(paste('~/code/repos/assessments/lm_map_support.r',sep=''),chdir=T)

scenario.list <- 'rcp85' ##c('rcp26','rcp45','rcp85')
proj.intervals <- c('2041-2070') ##,'2071-2100') ##c('2011-2040','2041-2070','2071-2100')
draft <- TRUE
##Single Season
run.season <- function() {
  var.name <- 'pr' ##'snowdepth'
  grep.name <- 'pr_seas' ##'snowdepth_seas'
  seas.list <- c('Winter') ##,'Spring','Summer','Fall','Annual') ##'APRIL1' c('January','February','March','April','May','June','July','August','September','October','November','December')
  for (seas in seas.list) {
    for (scenario in scenario.list) {
      for (proj.int in proj.intervals) {
        plot.single.seasonal(scenario,proj.int,
                             var.name,grep.name,seas,draft=FALSE)

      }
    }
  }
}

##Return Periods
run.return.periods <- function() {
  var.name <- 'pr'
  for (scenario in scenario.list) {
    for (proj.int in proj.intervals) {        
      plot.return.periods(scenario,proj.int,
                          var.name,rp='20',draft=FALSE)
    }
  }
}


##Climdex
run.climdex <- function() {
  ##var.names <- c('cddETCCDI','cwdETCCDI','prcptotETCCDI','rx1dayETCCDI','rx5dayETCCDI','suETCCDI','tn10pETCCDI','tx90pETCCDI','tnnETCCDI','txxETCCDI') 
  ##var.names <- c('fdETCCDI','gslETCCDI','idETCCDI','suETCCDI','trETCCDI','tnnETCCDI','txxETCCDI')
  var.names <- c('suETCCDI') ##,'r95daysETCCDI') ##,'suETCCDI')
  seas.list <- c('Annual') ##,'Spring','Summer','Fall')
  for (seas in seas.list) {
    for (var.name in var.names) {
      for (scenario in scenario.list) {
        for (proj.int in proj.intervals) {                

          plot.climdex(scenario,proj.int,
                       var.name,seas)
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
