
*** 1) Testing remittances/real exchange rate appreciation hypothesis ***
clear
set more off
capture log close
cd "C:\Users\erik.vardanyan\Desktop\Paper"
insheet using Data.csv
egen countrynum = group(country)
xtset countrynum year
egen incomegroup_num = group(incomegroup)
gen remit_reer=reer*remittances
gen lngdp_pc=ln(gdp_pc)

*** droping advanced economies from the data sample 

drop if countrynum==6 | countrynum==7 |  countrynum==12 | countrynum==26
drop if countrynum==35  |  countrynum==45 | countrynum==48 | countrynum==50
drop if countrynum==56 | countrynum==60 |  countrynum==62 | countrynum==64
drop if countrynum==86 | countrynum==87 |  countrynum==91 | countrynum==99 | countrynum==100
drop if countrynum==101 | countrynum==111 | countrynum==119 | countrynum==122 | countrynum==123
drop if countrynum==131 | countrynum==134 


xtabond2 reer l.reer gdpcons_lcu remittances i.year , gmm (reer  remittances, lag (1 2)) iv( l.gdpcons_lcu , equation(diff)) iv( l2.gdpcons_lcu i.year , equation (level)) twostep robust small cluster (countrynum)
est store REER
xtabond2 reer l.reer gdpcons_lcu remittances tot i.year , gmm (reer remittances , lag (1 2))  iv( l.gdpcons_lcu l.tot, equation(diff)) iv( l2.gdpcons_lcu l2.tot i.year , equation (level)) twostep robust small cluster (countrynum) 
est store REER1
xtabond2 reer l.reer gdpcons_lcu remittances tot trade_open i.year , gmm (reer remittances , lag (1 2))  iv( l.gdpcons_lcu l.tot l.trade_open, equation(diff)) iv(l2.gdpcons_lcu l2.tot l2.trade_open i.year , equation (level)) twostep robust small cluster (countrynum) 
est store REER2
xtabond2 reer l.reer gdpcons_lcu remittances tot trade_open lngdp_pc i.year , gmm (reer remittances , lag (1 2))  iv( l.gdpcons_lcu l.tot l.trade_open l.lngdp_pc, equation(diff)) iv( l2.gdpcons_lcu l2.tot l2.trade_open l2.lngdp_pc i.year , equation (level)) twostep robust small cluster (countrynum) 
est store REER3
xtabond2 reer l.reer gdpcons_lcu remittances tot trade_open lngdp_pc fdi i.year , gmm (reer remittances , lag (1 2))  iv( l.gdpcons_lcu l.tot l.trade_open l.lngdp_pc l.fdi, equation(diff)) iv( l2.gdpcons_lcu l2.tot l2.trade_open l2.lngdp_pc l2.fdi i.year, equation (level)) twostep robust small cluster (countrynum) 
est store REER4
xtabond2 reer l.reer gdpcons_lcu remittances tot trade_open lngdp_pc m2 i.year , gmm (reer remittances , lag (1 2))  iv( l.gdpcons_lcu l.tot l.trade_open l.lngdp_pc l.m2 , equation(diff)) iv( l2.gdpcons_lcu l2.tot l2.trade_open l2.lngdp_pc l2.m2 i.year, equation (level)) twostep robust small cluster (countrynum) 
est store REER5
xtabond2 reer l.reer gdpcons_lcu remittances tot trade_open lngdp_pc  hardpeg i.year , gmm (reer remittances, lag (1 2))  iv(l.gdpcons_lcu l.tot l.trade_open l.lngdp_pc , equation(diff)) iv( l2.gdpcons_lcu l2.tot l2.trade_open l2.lngdp_pc hardpeg i.year, equation (level)) twostep robust small cluster (countrynum)
est store REER6


 xml_tab REER REER1 REER2 REER3 REER4 REER5 REER6 , /*
*/ title("REER - Remittances") /*
*/ below stats(N, N_g, j, ar1p, ar2p, hansenp)  stars(.01 .05 .1) /*
*/ replace save("C:\Users\erik.vardanyan\Desktop\Paper\REER")

*** 2) Testing the main hypothesis of the paper ***
clear
set more off
capture log close
cd "C:\Users\erik.vardanyan\Desktop\Paper"
insheet using Data.csv
egen countrynum = group(country)
xtset countrynum year
egen incomegroup_num = group(incomegroup)
gen remit_reer=reer*remittances
gen lngdp_pc=ln(gdp_pc)
drop if countrynum==3 | countrynum==16 |  countrynum==36 | countrynum==106 | countrynum==114

xtabond2 hhi l.hhi lngdp_pc reer_vol remittances reer remit_reer tot gov_effect  population_growth i.year i.incomegroup_num, gmm (hhi lngdp_pc  , lag(1 3)) iv(l.reer  l.remittances l.gov_effect l.reer_vol l.tot l.population_growth l.remit_reer l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store HHI1
xtabond2 gini l.gini lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (gini  lngdp_pc , lag(1 3)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store GINI1
xtabond2 theil l.theil lngdp_pc  reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (theil lngdp_pc , lag(1 3)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)     
est store Theil1
xtabond2 theil_within l.theil_within lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year  i.incomegroup_num, gmm (theil_within  lngdp_pc , lag(1 3)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)     
est store Theil_within1
xtabond2 theil_between l.theil_between lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (theil_between lngdp_pc , lag(1 3)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd )   twostep robust small cluster (countrynum)    
est store Theil_between1
xtabond2 active_lines4 l.active_lines4 lngdp_pc reer_vol remittances reer remit_reer tot gov_effect  population_growth i.year i.incomegroup_num, gmm (active_lines4 lngdp_pc, lag(1 3)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store active_lines41



xml_tab GINI1 HHI1 Theil1 Theil_within1 Theil_between1 active_lines41 , /*
*/ title("Export Concentration") /*
*/ below stats(N, N_g, j, ar1p, ar2p, hansenp)  stars(.01 .05 .1) /*
*/ replace save("C:\Users\erik.vardanyan\Desktop\Paper\hypothesis1")



***** 3) Robustness check ***
clear
set more off
capture log close
cd "C:\Users\erik.vardanyan\Desktop\Paper"
insheet using Data.csv
egen countrynum = group(country)
xtset countrynum year
egen incomegroup_num = group(incomegroup)
gen remit_reer=reer*remittances
gen lngdp_pc=ln(gdp_pc)
drop if countrynum==3 | countrynum==10 | countrynum==16 | countrynum==23 | countrynum==30 | countrynum==36
drop if countrynum==43 | countrynum==80 | countrynum==106 | countrynum==107 | countrynum==108 | countrynum==109
drop if countrynum==110 | countrynum==114 | countrynum==117 | countrynum==127 | countrynum==128

xtabond2 hhi l.hhi lngdp_pc reer_vol remittances reer remit_reer tot gov_effect  population_growth i.year i.incomegroup_num, gmm (hhi lngdp_pc  , lag(1 2)) iv(l.reer  l.remittances l.gov_effect l.reer_vol l.tot l.population_growth l.remit_reer l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store HHI13
xtabond2 gini l.gini lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (gini  lngdp_pc , lag(1 2)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store GINI13
xtabond2 theil l.theil lngdp_pc  reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (theil lngdp_pc , lag(1 2)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)     
est store Theil13
xtabond2 theil_within l.theil_within lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year  i.incomegroup_num, gmm (theil_within  lngdp_pc , lag(1 2)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)     
est store Theil_within13
xtabond2 theil_between l.theil_between lngdp_pc reer_vol remittances reer remit_reer tot gov_effect   population_growth i.year i.incomegroup_num, gmm (theil_between lngdp_pc , lag(1 2)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd )   twostep robust small cluster (countrynum)    
est store Theil_between13
xtabond2 active_lines4 l.active_lines4 lngdp_pc reer_vol remittances reer remit_reer tot gov_effect  population_growth i.year i.incomegroup_num, gmm (active_lines4 lngdp_pc, lag(1 2)) iv(l.reer l.remittances l.reer_vol l.tot l.population_growth l.remit_reer l.gov_effect l.inflation l.unemployment l.prim_enrol l.exch_loc_usd ) twostep robust small cluster (countrynum)  
est store active_lines413

xml_tab GINI13 HHI13 Theil13 Theil_within13 Theil_between13 active_lines413 , /*
*/ title("Export Concentration") /*
*/ below stats(N, N_g, j, ar1p, ar2p, hansenp)  stars(.01 .05 .1) /*
*/ replace save("C:\Users\erik.vardanyan\Desktop\Paper\hypothesis2")





