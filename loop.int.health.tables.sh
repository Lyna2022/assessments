#!/bin/bash

declare -a regions=("cariboo_memorial_hospital" "helmcken_memorial_hospital" "hundred_mile_general_hospital" "lillooet_hospital" "royal_inland_hospital" "shuswap_lake_hospital" "nicola_valley_health" "queen_victoria_hospital" "vernon_jubilee_hospital" "kelowna_general_hospital" "penticton_regional_hospital" "princeton_general_hospital" "south_okanagan_hospital" "arrow_lakes_hospital" "kootenay_lake_hospital" "kootenay_boundary_hospital" "boundary_hospital" "golden_districts_hospital" "invermere_district_hospital" "east_kootenay_hospital" "elk_valley_hospital" "creston_valley_hospital")
declare -a titles=("CaribooMemorialHospital" "HelmckenMemorialHospital" "HundredMileGeneralHospital" "LillooetHospital" "RoyalInlandHospital" "ShuswapLakeHospital" "NicolaValleyHealth" "QueenVictoriaHospital" "VernonJubileeHospital" "KelownaGeneralHospital" "PentictonRegionalHospital" "PrincetonGeneralHospital" "SouthOkanaganHospital" "ArrowLakesHospital" "KootenayLakeHospital" "KootenayBoundaryHospital" "BoundaryHospital" "GoldenDistrictsHospital" "InvermereDistrictHospital" "EastKootenayHospital" "ElkValleyHospital" "CrestonValleyHospital")
declare -a readlocs=("interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health" "interior_health")
declare -a writelocs=("interior_health/cariboo_memorial_hospital" "interior_health/helmcken_memorial_hospital" "interior_health/hundred_mile_general_hospital" "interior_health/lillooet_hospital" "interior_health/royal_inland_hospital" "interior_health/shuswap_lake_hospital" "interior_health/nicola_valley_health" "interior_health/queen_victoria_hospital" "interior_health/vernon_jubilee_hospital" "interior_health/kelowna_general_hospital" "interior_health/penticton_regional_hospital" "interior_health/princeton_general_hospital" "interior_health/south_okanagan_hospital" "interior_health/arrow_lakes_hospital" "interior_health/kootenay_lake_hospital" "interior_health/kootenay_boundary_hospital" "interior_health/boundary_hospital" "interior_health/golden_districts_hospital" "interior_health/invermere_district_hospital" "interior_health/east_kootenay_hospital" "interior_health/elk_valley_hospital" "interior_health/creston_valley_hospital")

# get length of an array                                                                                                                               
arraylength=${#regions[@]}

echo $arraylength

# use for loop to read all values and indexes                                                                                                          
for (( i=0; i<${arraylength}; i++ ));
do
  echo $i " / " ${arraylength} " : " ${regions[$i]}
  #qsub -N "${regions[$i]}.rp5" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]},rp="5" run.rp.wrapper.pbs
  #qsub -N "${regions[$i]}.rp20" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]},rp="20" run.rp.wrapper.pbs
  qsub -N "${regions[$i]}.tp" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.t.p.wrapper.pbs
  #qsub -N "${regions[$i]}.dd" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.dd.wrapper.pbs
  qsub -N "${regions[$i]}.bc" -v region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.build.code.wrapper.pbs

#  qsub -N "${regions[$i]}.id" -v varname='idETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.tr" -v varname='trETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.fd" -v varname='fdETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.txx" -v varname='txxETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.tnn" -v varname='tnnETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.tnx" -v varname='tnxETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.txn" -v varname='txnETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.dtr" -v varname='dtrETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.rx1day" -v varname='rx1dayETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.rx2day" -v varname='rx2dayETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.rx5day" -v varname='rx5dayETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.su" -v varname='suETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.su30" -v varname='su30ETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.gsl" -v varname='gslETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.sdii" -v varname='sdiiETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r10mm" -v varname='r10mmETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r20mm" -v varname='r20mmETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.cdd" -v varname='cddETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.cwd" -v varname='cwdETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r95p" -v varname='r95pETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r99p" -v varname='r99pETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r95days" -v varname='r95daysETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.r99days" -v varname='r99daysETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
#  qsub -N "${regions[$i]}.prcptot" -v varname='prcptotETCCDI',region=${regions[$i]},title=${titles[$i]},readloc=${readlocs[$i]},writeloc=${writelocs[$i]} run.climdex.wrapper.pbs
done

