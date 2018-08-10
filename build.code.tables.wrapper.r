##Wrapper script for ds.climdex.tables.r in the main RCI code directory.

source('/storage/home/ssobie/code/repos/assessments/calc.build.code.tables.r',chdir=TRUE)
##-----------------------------------------------------------------------------------------------

args <- commandArgs(trailingOnly=TRUE)
for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
}

##region <- 'vancouver_general_hospital_site'
##title <- "VanGeneral"
##readloc <- 'van_coastal_health'
##writeloc <- 'van_coastal_health/vancouver_general_hospital_site'

ds.type <- 'bccaq2' 
scenario <- 'rcp85'
past.int <- '1971-2000'
proj.list <- c('2011-2040','2041-2070','2071-2100')
pctl <- TRUE ##Controls whether the percentiles (10,50,90) are added to the ensemble

shape.dir <- paste0('/storage/data/projects/rci/data/assessments/shapefiles/',region,'/')
read.dir <- paste0('/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/assessment_subsets/',readloc,'/',scenario,'/')
write.dir <-  paste0('/storage/data/projects/rci/data/assessments/',writeloc,'/')

gcm.list <- c('ACCESS1-0','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0','GFDL-ESM2G',
              'HadGEM2-CC','inmcm4','HadGEM2-ES','MIROC5','MPI-ESM-LR','MRI-CGCM3')

clip.shp <- spTransform(readOGR(shape.dir,region, stringsAsFactors=F),CRS("+init=epsg:4326"))

for (proj.int in proj.list) {
   print(proj.int)
   make.build.tables(gcm.list,ds.type,region,title,scenario,clip.shp,
                     past.int=past.int,proj.int=proj.int,
                     read.dir=read.dir,write.dir=write.dir,pctl=pctl)

}




