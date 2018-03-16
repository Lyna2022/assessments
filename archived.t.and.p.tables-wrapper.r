##Script to produce tables of projected temperature and precipitation changes
source('/storage/data/projects/rci/bcgov/moti/nrcan-precip_case_studies/code/moti.climdex.robjects.r',chdir=TRUE)
source('/storage/home/ssobie/code/hg/rci-lib/ds.t.and.p.tables.r',chdir=TRUE)

##-----------------------------------------------------------------------------------------------

var.list <- c('pr','tasmax','tasmin')
##var.list <- 'snowdepth'
ds.type <- 'bccaq'  ## 'rcm'
type <- 'gcm'
##reg.list <-  c('skeena','northeast','kootenay','omineca','west','cariboo','thompson','south')
##reg.titles <- c('Skeena','Northeast','Kootenay','Omineca','West','Cariboo','Thompson','South') 
##reg.list <-  c('GVRD','JDF_Electoral','victoria_water','southern_gulf_islands')
##reg.titles <- c('Greater Victoria','Juan de Fuca Electoral','Victoria Water Supply','Southern Gulf Islands') 

##reg.list <-  c('cvrd_developed_watersheds','cvrd_water_supply_watersheds','cvrd_west_coast_watersheds')
##reg.titles <- c('Developed Area Watersheds','Water Supply Watersheds','West Coast Watersheds') 

##reg.list <- 'fraser_municipal' ##'upper_adams'
##reg.titles <- 'Fraser District Municipalities' ##'Upper Adams Forestry Road'



reg.list <- c('mission','kent','abbotsford','chilliwack','FVRDelectoralG','FVRDelectoralH')
reg.titles <- c('Mission','Kent','Abbotsford','Chilliwack','FVRD Electoral G','FVRD Electoral H')

##reg.list <- 'bc'
##reg.titles <- 'Province of BC'

proj.dir <- '/storage/data/projects/rci/data/assessments/fraser_municipal/'

##proj.dir <- '/storage/data/projects/rci/data/forestry/regional_summaries/'

past.int <- '1971-2000'

pctl <- TRUE ##Controls whether the percentiles (10,50,90) are added to the ensemble


gcm.list <- c('ACCESS1-0',              
              'CanESM2',
              'CCSM4',
              'CNRM-CM5',
              'CSIRO-Mk3-6-0',
              'GFDL-ESM2G',
              'HadGEM2-CC',
              'inmcm4',                                                
              'HadGEM2-ES',
              'MIROC5',
              'MPI-ESM-LR',
              'MRI-CGCM3')

  rcp26.list <- c('CanESM2',
                  'CCSM4',
                  'CNRM-CM5',
                  'CSIRO-Mk3-6-0',
                  'GFDL-ESM2G',
                  'HadGEM2-ES',
                  'MIROC5',
                  'MPI-ESM-LR',
                  'MRI-CGCM3')

scen.list <- 'rcp85' ###c('rcp26','rcp45','rcp85')
proj.list <- c('2011-2040','2041-2070','2071-2100')

for (i in seq_along(reg.list)) {        
   region <- reg.list[i]
   print(region)
   region.title <- reg.titles[i]
   shape.dir <- '/storage/data/projects/rci/data/assessments/fraser_district/shapefiles/sub_regions/'
   shape.name <- region

   clip.shp <- readOGR(shape.dir,shape.name, stringsAsFactors=F)  

   for (scenario in scen.list) {
     print(scenario)
     ##read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/',scenario,'/',sep='')
       read.dir <- paste('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_van_whistler_subset/',scenario,'/',sep='')
     model.list <- gcm.list
     if (scenario=='rcp26') {
        model.list <- rcp26.list
     }
     for (proj.int in proj.list) {
         print(proj.int)
         make.tables(var.list,model.list,
                       ds.type,region,region.title,clip.shp,type,scenario,
                       proj.dir,read.dir,write.dir=proj.dir,
                       past.int=past.int,proj.int=proj.int,pctl=pctl)   
     }
   }
}
  
