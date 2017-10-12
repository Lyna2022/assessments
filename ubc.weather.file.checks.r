##Script to look at the UBC weather file data

get.data <- function(data) {

   pst <- grep('1971-01-01',data$DATE)
   pen <- grep('2000-12-31',data$DATE)
   fst <- grep('2041-01-01',data$DATE)
   fen <- grep('2070-12-31',data$DATE)

   tas <- (data$TASMAX+data$TASMIN)/2
   tas.past <- tas[pst:pen]
   tas.proj <- tas[fst:fen]
   time.past <- data$DATE[pst:pen]
   time.proj <- data$DATE[fst:fen]
   p.jfac <- as.factor(format(as.Date(time.past),'%j'))
   f.jfac <- as.factor(format(as.Date(time.proj),'%j'))
   p.jday <- tapply(tas.past,p.jfac,mean)
   f.jday <- tapply(tas.proj,f.jfac,mean)
   browser()
   jday.diff <- f.jday-p.jday
   return(jday.diff)
}

read.dir <- '/storage/data/projects/rci/data/assessments/ubc/'
data.800 <- read.csv(paste0(read.dir,'CanESM2_BCCAQ-PRISM_800m_TASMAX_TASMIN_PR_UBC_EOS_1951-2100.csv'),header=T,as.is=T)
data.10 <- read.csv(paste0(read.dir,'CanESM2_BCCAQ_10km_TASMAX_TASMIN_PR_UBC_EOS_1951-2100.csv'),header=T,as.is=T)

series.800 <- get.data(data.800)
series.10 <- get.data(data.10)

plot(series.800,type='l',col='red',lwd=3)
lines(series.10,col='blue',lwd=3)