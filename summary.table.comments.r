##Script to contain the comments for the openxlsx summary tables


variable.comment <- function(var.name) {

   var.list <- list(pr=c('Precipitation\n','Average seasonal and annual precipitation totals'),
                    rx1dayETCCDI=c('RX1Day\n','Average seasonal and annual maximum 1-day precipitation'),
                    rx5dayETCCDI=c('RX5Day\n','Average seasonal and annual maximum 5-day precipitation'),
                    r95pETCCDI=c('R95p\n','Average Annual total precipitation when PR > 95th percentile of daily precipitation'),
                    r95daysETCCDI=c('R95days\n','Average number of days when PR > 95th percentile of daily precipitation'),
                    r99pETCCDI=c('R99p\n','Average Annual total precipitation when PR > 99th percentile of daily precipitation'),
                    r99daysETCCDI=c('R99days\n','Average number of days when PR > 99th percentile of daily precipitation'),
                    pr.maximum=c('Max Ann Pr\n','Maximum annual precipitation amount'),
                    pr.minimum=c('Min Ann Pr\n','Minimum annual precipitation amount'),
                    pr.standard_deviation=c('SD Pr\n','Standard deviation of annual precipitation amount'),
                    pr_rp5=c('RP5 Precipitation\n','5-Year annual maximum one day precipitation amount'),
                    pr_rp20=c('RP20 Precipitation\n','20-Year annual maximum one day precipitation amount'),
                    pr_rp50=c('RP50 Precipitation\n','50-Year annual maximum one day precipitation amount'),
                    cddETCCDI=c('CDD\n','Average annual maximum length of consecutive dry days'),
                    cdd90ETCCDI=c('CDD90\n','90th percentile of annual maximum length of consecutive dry days'),
                    cddmaxETCCDI=c('CDDMAX\n','Maximum annual maximum length of consecutive dry days'),
                    cwdETCCDI=c('CDD\n','Average annual maximum length of consecutive wet days'),
                    tasmax=c('Maximum Temperature\n','Seasonally and annually averaged daily maximum temperature'),
                    tas=c('Average Temperature\n','Seasonally and annually averaged daily average temperature'),
                    tasmin=c('Minimum Temperature\n','Seasonally and annually averaged daily minimum temperature'),
                    txxETCCDI=c('TXX\n','Average seasonal and annual maximum of daily maximum temperature'),
                    tnnETCCDI=c('TNN\n','Average seasonal and annual minimum of daily minimum temperature'),
                    txnETCCDI=c('TXN\n','Average seasonal and annual minimum of daily maximum temperature'),
                    tnxETCCDI=c('TNX\n','Average seasonal and annual maximum of daily minimum temperature'),
                    dtrETCCDI=c('DTR\n','Averege seasonal and annual diurnal temperature range'),
                    suETCCDI=c('SU\n','Average number of summer days (daily maximum temperature > 25\u00B0C)'),
                    su30ETCCDI=c('SU30\n','Average number of summer days (daily maximum temperature > 30\u00B0C)'),                    
                    trETCCDI=c('TR\n','Average number of tropical nights (daily minimum temperature > 20\u00B0C)'),
                    idETCCDI=c('ID\n','Average number of icing days (daily maximum temperature < 0\u00B0C)'),
                    fdETCCDI=c('FD\n','Average number of frost days (daily minimum temperature < 0\u00B0C)'),
                    gslETCCDI=c('GSL\n','Growing season length (number of days between first span of at least 6 days with daily mean temperature > 5\u00B0C and first span after July 1st of 6 days with daily mean temperature < 5\u00B0C)'),
                    cdd=c('CDD\n','Cooling Degree Days (Threshold: 18\u00B0C)'),
                    gdd=c('GDD\n','Growing Degree Days (Threshold: 5\u00B0C)'),
                    hdd=c('HDD\n','Heating Degree Days (Threshold: 18\u00B0C)'),
                    fdd=c('FDD\n','Freezing Degree Days (Threshold: 0\u00B0C)'),
                    tasmax_rp5=c('RP5 Tasmax\n','5-Year annual maximum daily maximum temperature'),
                    tasmax_rp20=c('RP20 Tasmax\n','20-Year annual maximum daily maximum temperature'),
                    tasmin_rp5=c('RP5 Tasmin\n','20-Year annual minimum daily minimum temperature'),
                    tasmin_rp20=c('RP20 Tasmin\n','20-Year annual minimum daily minimum temperature'),
                    tasmax.annual_quantile_975=c('Tasmax 97.5%\n','Warm Month Design Temperature 97.5%'),
                    tasmax.annual_quantile_990=c('Tasmax 99.0%\n','Warm Month Design Temperature 99.0%'),
                    tasmax.annual_quantile_996=c('Tasmax 99.6%\n','Warm Month Design Temperature 99.6%'),
                    tasmin.annual_quantile_004=c('Tasmin 0.4%\n','Cold Month Design Temperature 0.4%'),
                    tasmin.annual_quantile_010=c('Tasmin 1.0%\n','Cold Month Design Temperature 1.0%'),
                    tasmin.annual_quantile_025=c('Tasmin 2.5%\n','Cold Month Design Temperature 2.5%'))



  rv <- var.list[[var.name]]
  return(rv)                    
}