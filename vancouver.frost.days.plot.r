##Script to plot time series of frost free days for Vancouver Intl.

library(ncdf4)
library(PCICt)
library(climdex.pcic)

source('/storage/data/projects/rci/stat.downscaling/bccaq2/code/new.netcdf.calendar.R',chdir=T)

read.dir <- '/storage/data/scratch/ssobie/bccaq_gcm_van_whistler_subset/rcp85/climdex/'
plot.dir <- '/storage/data/projects/rci/data/assessments/van_intl/'

coords <- c(-123.1812,49.1945)
var.name <- 'fdETCCDI'

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

fd.matrix <- matrix(NA,nrow=12,ncol=150)

for (i in seq_along(gcm.list)) {
    gcm <- gcm.list[i]
    print(gcm)
    file.name <- list.files(path=paste0(read.dir,gcm),pattern=var.name,full.name=TRUE)
    file.nc <- nc_open(file.name)
    lon <- ncvar_get(file.nc,'lon')
    lon.ix <- which.min(abs(lon-coords[1]))
    lat <- ncvar_get(file.nc,'lat')
    lat.ix <- which.min(abs(lat-coords[2]))
    data.sub <- ncvar_get(file.nc,var.name,start=c(lon.ix,lat.ix,1),count=c(1,1,-1))
    if (grepl('HadGEM2',gcm)) {
       data.sub <- c(data.sub,NA)
    }
    fd.matrix[i,] <- data.sub
}


fd.mean <- apply(fd.matrix,2,mean)

mean.1980s <- mean(fd.mean[21:50])
mean.2050s <- mean(fd.mean[91:120])

##png('/storage/data/projects/rci/data/assessments/metro_van/plots/frost_days_van_intl2.png',width=1200,height=600)
plot(fd.mean,type='l',col='blue',lwd=5,ylim=c(0,90),axes=FALSE,
        main='Vancouver International Airport\nAnnual Frost Days',
        xlab='Year',ylab='Number of Frost Days',cex.main=2,cex.lab=1.5)
apply(fd.matrix,1,lines,lwd=3,col='gray')
lines(fd.mean,col='blue',lwd=5)
lines(x=c(21,50),y=c(mean.1980s,mean.1980s),lwd=6,col='black')
lines(x=c(91,120),y=c(mean.2050s,mean.2050s),lwd=6,col='black')
axis(1,seq(1950,2100,10),at=seq(0,150,10),cex=2,cex.lab=2,cex.axis=1.5)
axis(2,seq(0,100,20),at=seq(0,100,20),cex=2,cex.lab=2,cex.axis=1.5)
abline(h=seq(0,100,20),lty=2,col='gray',lwd=0.5)
box(which='plot')
##dev.off()