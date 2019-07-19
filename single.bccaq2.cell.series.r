##Script to plot a single cell of data as a time series

library(ncdf4)
source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)


get_cell <- function(file.name,var.name,lonc,latc) {

   nc <- nc_open(file.name)
   lon <- ncvar_get(nc,'lon')
   lon <- ((lon + 180) %% 360) - 180
   lat <- ncvar_get(nc,'lat')
   time <- netcdf.calendar(nc)

   lon.ix <- which.min(abs(lonc-lon))
   lat.ix <- which.min(abs(latc-lat))

   data <- ncvar_get(nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
   nc_close(nc)
   rv <- list(data=data,time=time)
   return(rv)
}


read.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/high_res_downscaling/bccaq_gcm_bc_subset/'
var.name <- 'pr'

gcm.list <- list(list(gcm='ACCESS1-0',
                       file='_day_BCCAQ2_ACCESS1-0_rcp85_r1i1p1_2001-2100.nc',
                       lonc=-122.21875,latc=49.34375,dst='2099-11-01',den='2099-11-30',dct='2099-11-14'),
                  list(gcm='CanESM2',
                       file='_day_BCCAQ2_CanESM2_rcp85_r1i1p1_2001-2100.nc',
                       lonc=-122.40625,latc=49.59375,dst='2065-10-05',den='2065-11-05',dct='2065-10-20'),
                  list(gcm='CCSM4',
                       file='_day_BCCAQ2_CCSM4_rcp85_r2i1p1_1951-2000.nc',
                       lonc=-122.34375,latc=49.65625,dst='1956-09-02',den='1956-10-02',dct='1956-09-17'),
                  list(gcm='CNRM-CM5',
                       file='_day_BCCAQ2_CNRM-CM5_rcp85_r1i1p1_2001-2100.nc',
                       lonc=-122.28125,latc=49.59375,dst='2002-09-20',den='2002-10-20',dct='2002-10-02'),
                  list(gcm='HadGEM2-ES',
                       file='_day_BCCAQ2_HadGEM2-ES_rcp85_r1i1p1_2001-2100.nc',
                       lonc=-122.53125,latc=49.65625,dst='2006-09-15',den='2006-10-15',dct='2006-09-30'),
                  list(gcm='MPI-ESM-LR',
                       file='_day_BCCAQ2_MPI-ESM-LR_rcp85_r3i1p1_2001-2100.nc',
                       lonc=-122.15625,latc=49.46875,dst='2095-11-01',den='2095-11-30',dct='2095-11-15'))

##pr.series <- vector(mode='list',length=6)
##tasmax.series <- vector(mode='list',length=6)

for (i in 6:6) {
  gcm.info <- gcm.list[[i]]
  print(gcm.info$gcm)
  pr <- get_cell(paste0(read.dir,gcm.info$gcm,'/pr',gcm.info$file),       
                               var.name='pr',
                               gcm.info$lonc,gcm.info$latc)      
  tasmax <- get_cell(paste0(read.dir,gcm.info$gcm,'/tasmax',gcm.info$file),       
                               var.name='tasmax',
                               gcm.info$lonc,gcm.info$latc)      

  char.dates <- format(as.Date(as.character(pr$time)),'%Y-%m-%d')

  s.ix <- which(char.dates==gcm.info$dst)
  e.ix <- which(char.dates==gcm.info$den)

  time.sub <- char.dates[s.ix:e.ix]
  pr.sub <- pr$data[s.ix:e.ix]
  tasmax.sub <- tasmax$data[s.ix:e.ix]

  pr.series[[i]] <- list(data=pr.sub,time=time.sub)
  tasmax.series[[i]] <- list(data=tasmax.sub,time=time.sub)

}
pdf(file='/storage/home/ssobie/general/plots/bccaq2.pr.tx.series.fraser.sites.pdf',width=5,height=8)
par(mfrow=c(6,2))
par(mar=c(2,4,2,1))
for (j in 1:6) {
    plot(as.Date(pr.series[[j]]$time),pr.series[[j]]$data,type='l',lwd=2,col='blue',
         xlab='',ylab='Pr (mm)',main=paste0('BCCAQ2 ',gcm.list[[j]]$gcm),
         cex.main=0.75,cex.lab=0.75,cex.axis=0.75)
    abline(v=as.Date(gcm.list[[j]]$dct))
    plot(as.Date(tasmax.series[[j]]$time),tasmax.series[[j]]$data,type='l',lwd=2,col='red',
         xlab='Date',ylab='Tasmax (degC)',main=paste0('BCCAQ2 ',gcm.list[[j]]$gcm),
         cex.main=0.75,cex.lab=0.75,cex.axis=0.75)
    abline(v=as.Date(gcm.list[[j]]$dct))
}
dev.off()

#----------------------------------------------------

prism.list <- list(list(gcm='ACCESS1-0',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_ACCESS1-0_rcp85_r1i1p1_1951-2100.nc'),
                       lonc=-122.21875,latc=49.34375),
                  list(gcm='CanESM2',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_CanESM2_rcp85_r1i1p1_1951-2100.nc'),
                       lonc=-122.40625,latc=49.59375),
                  list(gcm='CCSM4',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_CCSM4_rcp85_r2i1p1_1951-2100.nc'),
                       lonc=-122.34375,latc=49.65625),
                  list(gcm='CNRM-CM5',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_CNRM-CM5_rcp85_r1i1p1_1951-2100.nc'),
                       lonc=-122.28125,latc=49.59375),
                  list(gcm='HadGEM2-ES',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_HadGEM2-ES_rcp85_r1i1p1_1951-2100.nc'),
                       lonc=-122.53125,latc=49.65625),
                  list(gcm='MPI-ESM-LR',
                       file=paste0(var.name,'_annual_maximum_BCCAQ2_PRISM_MPI-ESM-LR_rcp85_r3i1p1_1951-2100.nc'),
                       lonc=-122.15625,latc=49.46875))

data.series <- vector(mode='list',length=6)
year.series <- vector(mode='list',length=6)
for (i in 1:6) {
  prism.info <- prism.list[[i]]
  print(prism.info$gcm)
  year.series[[i]] <- get_cell(paste0(read.dir,prism.info$gcm,'/rcp85/annual_extremes/',prism.info$file),       
                               var.name,
                               prism.info$lonc,prism.info$latc)      
}
pdf(file='/storage/home/ssobie/general/plots/bccaq2-prism.pr.series.fraser.sites.pdf',width=5,height=8)
par(mar=c(2,4,2,1))
par(mfrow=c(6,1))
for (j in 1:6) {
    plot(year.series[[j]]$time,year.series[[j]]$data,type='l',lwd=3,col='green',
         xlab='Year',ylab='Pr (mm)',main=paste0('BCCAQ2-PRISM ',prism.list[[j]]$gcm))
}
dev.off()