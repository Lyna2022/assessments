##Testing script

library(ncdf4)
library(PCICt)

source('/storage/home/ssobie/code/repos/monthlyDS/R/calculate.anomalies.r',chdir=T)

var.name <- 'tasmax'
read.dir <- '/storage/data/climate/downscale/BCCAQ2+PRISM/mvbc/'
write.dir <- read.dir
series.file <- 'tasmax_sub.nc'
anomaly.file <- 'anomaly_tasmax_sub.nc'
yst <- 1971
yen <- 2000
calculate_anomalies(var.name,series.file,anomaly.file,yst,yen)

source('/storage/home/ssobie/code/repos/monthlyDS/R/interpolate.anomalies.r',chdir=T)

anomaly.file <- 'anomaly_tasmax_sub_1951-2100.nc'

grid.file <- 'mbc.test.grid.txt'
grid.dir <- '/storage/home/ssobie/grid_files/'
write.dir <- paste0(read.dir,'interp/')
interpolate_anomalies(var.name,anomaly.file,read.dir,
                     grid.file,grid.dir,
                      write.dir)

interp.dir <- paste0(read.dir,'interp/')
downscale.dir <- paste0(read.dir,'hires/')

source('/storage/home/ssobie/code/repos/monthlyDS/R/make.daily.scale.r',chdir=T)

years <- 1950:2100
target.dir <- read.dir
target.file <- 'tasmax.mon.tps.nc'

for (year in years) {
  print(year)
  interp.file <- paste0('interpolated_anomaly_tasmax_sub_',year,'.nc')
  daily_target_scale(var.name,
                     interp.file,interp.dir,
                     target.file,target.dir,
                     downscale.dir)
}
