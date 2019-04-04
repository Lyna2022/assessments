#!/bin/bash

declare -a regions=("abbotsford_regional_hospital_site" "burnaby_hospital_site" "chilliwack_general_hospital_site" "delta_hospital_site" "fraser_health" "langley_memorial_hospital_site" "mission_memorial_hospital_site" "peace_arch_hospital_site" "ridge_meadows_hospital_site" "riverview_hospital_site" "royal_columbian_hospital_site" "surrey_memorial_hospital_site")
declare -a titles=("BellaBellaHospital" "Richmond" "LionsgateHospital" "RoyalColumbianHospital" "VancouverGeneralHospital" "BellaHealth" "Lionsgate" "\
VancouverHealth")
declare -a readlocs=("bella_health" "van_coastal_health" "van_coastal_health" "van_coastal_health" "van_coastal_health" "bella_health" "van_coastal_he\
alth" "van_coastal_health")
declare -a writelocs=("van_coastal_health/bella_hospital_site" "van_coastal_health/richmond" "van_coastal_health/lionsgate_hospital_site" "van_coastal\
_health/royal_columbian_hospital_site" "van_coastal_health/vancouver_general_hospital_site" "van_coastal_health/bella_health" "van_coastal_health/lion\
sgate_hospital" "van_coastal_health/van_coastal_health")

# get length of an array                                                                                                                               
arraylength=${#regions[@]}

echo $arraylength

# use for loop to read all values and indexes                                                                                                          
for (( i=0; i<${arraylength}; i++ ));
do
  echo $i " / " ${arraylength} " : " ${regions[$i]}
  qsub -N "${regions[$i]}.rp50" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]},rp="50" run.rp.wrapper.\
pbs
done


region='fraser_canyon_hospital_site' ##'delta_hospital_site'
title='FraserCanyon'
readloc='interior_health'
writeloc='van_coastal_health/vch_fraser/fraser_canyon_hospital_site' ##'agriculture/'$region ##delta_hospital_site'
echo $writeloc

##qsub -N "${region}.tp" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.t.p.wrapper.pbs
##qsub -N "${region}.dd" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.dd.wrapper.pbs
##qsub -N "${region}.rp5" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc,rp="5" run.rp.wrapper.pbs
##qsub -N "${region}.rp20" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc,rp="20" run.rp.wrapper.pbs
##qsub -N "${region}.rp50" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc,rp="50" run.rp.wrapper.pbs
qsub -N "${region}.bc" -v region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.build.code.wrapper.pbs

###  climdex.list <- c('idETCCDI','trETCCDI','fdETCCDI',
###                     'txxETCCDI','tnxETCCDI','txnETCCDI','tnnETCCDI','dtrETCCDI',
###                    'rx1dayETCCDI','rx2dayETCCDI','rx5dayETCCDI',
###                    'suETCCDI','su30ETCCDI','gslETCCDI',
###                    'sdiiETCCDI','r10mmETCCDI','r20mmETCCDI','cddETCCDI','cwdETCCDI',
###                    'r95pETCCDI','r99pETCCDI','r95daysETCCDI','r99daysETCCDI',
###                    'prcptotETCCDI')

##qsub -N "cdd90" -v varname='cdd90ETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "cddmax" -v varname='cddmaxETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs

##qsub -N "id" -v varname='idETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "tr" -v varname='trETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "fd" -v varname='fdETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "txx" -v varname='txxETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "tnn" -v varname='tnnETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "tnx" -v varname='tnxETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "txn" -v varname='txnETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "dtr" -v varname='dtrETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "rx1day" -v varname='rx1dayETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "rx2day" -v varname='rx2dayETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "rx5day" -v varname='rx5dayETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "su" -v varname='suETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "su30" -v varname='su30ETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "gsl" -v varname='gslETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "sdii" -v varname='sdiiETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r10mm" -v varname='r10mmETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r20mm" -v varname='r20mmETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "cdd" -v varname='cddETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "cwd" -v varname='cwdETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r95p" -v varname='r95pETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r99p" -v varname='r99pETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r95days" -v varname='r95daysETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "r99days" -v varname='r99daysETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
##qsub -N "prcptot" -v varname='prcptotETCCDI',region=$region,title=$title,readloc=$readloc,writeloc=$writeloc run.climdex.wrapper.pbs
