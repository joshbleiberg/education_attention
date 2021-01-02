********************************************************************************
********************************************************************************
/*
An exploration of spillover effects: evidence from threat-induced education reform
by Joshua Bleiberg
Journal of Education Policy  2019
https://doi.org/10.1080/02680939.2019.1704065

Paper published December 2019 and replication code posted January 2021

Note: code requires packages putexcel and cpigen
*/
********************************************************************************
********************************************************************************
//Clearing data and previous logs
clear all
log close _all
cls
//Set Working Directory to Folder Containing "ed_attention_master.dta"
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

**cd "CHANGE TO WORKING DIRECTORY HERE"

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Creating Log File
log using "ed_attention_log", replace
//Reading in Master Data File
use "ed_attention_master_01_02_2021.dta", clear
********************************************************************************
********************************************************************************
//Analysis
********************************************************************************
********************************************************************************
*Figure 1. Changes in deployed troops over time
graph twoway line (troop_dif year), ///
	ytitle("Troop Changes since Previous Year in 100Ks") ///
	xlabel(1947[5]2015) ///
	xscale(r (1947 2015) noextend) ///
	xtitle("Year")  ///
	scheme( s1mono )
graph export "figure1.png", replace

*Table 1. Descriptive Statistics
local descrip_vars agenda plaw_passed troop_dif ///
nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1
	 
foreach var of local descrip_vars{
	tabstat `var', statistics(mean sd min max count) c(s) save
	matrix descrip_`var'=r(StatTotal)'
	matrix colnames descrip_`var'="Mean" "SD" "Min" "Max" "N"
}
matrix descrip_all=descrip_agenda \ descrip_plaw_passed \ descrip_troop_dif \ descrip_nyt_def_std \ descrip_ed_nga_bin \ descrip_state_rev \ descrip_gallup_mip_ed \ descrip_sat \ descrip_control_house \ descrip_control_senate \ descrip_pres_party \ descrip_cong_polar \ descrip_conserv \ descrip_hearing_days_t1

putexcel set "table_1.xlsx", modify
putexcel B2="Table 1. Descriptive statistics"
putexcel C4=matrix(descrip_all)
putexcel C3="Mean"
putexcel D3="SD"
putexcel E3="Min"
putexcel F3="Max"
putexcel G3="N"
putexcel B4="Education on Agenda"
putexcel B5="Education Law Passed"
putexcel B6="Troop Difference"
putexcel B7="National Security Issue"
putexcel B8="Education Governors"
putexcel B9="State Education Revenues"
putexcel B10="Education MIP"
putexcel B11="SAT Composite"
putexcel B12="Control House"
putexcel B13="Control Senate"
putexcel B14="President"
putexcel B15="Polarization"
putexcel B16="Congressional Ideology"
putexcel B17="Hearing Days Lagged"

*Table 2. Means conditional on presence on agenda and law passage
estpost ttest plaw_passed, by(agenda)
	esttab using "table_2.csv", cells(mu_1 mu_2 t p) wide nonumber star(* 0.1 ** 0.05 *** 0.01) ///
		mtitle("Table 2. Means conditional on presence on agenda and law passage") ///
		replace
estpost ttest troop_dif, by(agenda)
	esttab using "table_2.csv", cells(mu_1 mu_2 t p) wide nonumber star(* 0.1 ** 0.05 *** 0.01) ///
		mtitle("Table 2. Means conditional on presence on agenda and law passage") ///
		append
estpost ttest agenda, by(plaw_passed)
	esttab using "table_2.csv", cells(mu_1 mu_2 t p) wide nonumber star(* 0.1 ** 0.05 *** 0.01) ///
		mtitle("Table 2. Means conditional on presence on agenda and law passage") ///
		append
estpost ttest troop_dif, by(plaw_passed)
	esttab using "table_2.csv", cells(mu_1 mu_2 t p) wide nonumber star(* 0.1 ** 0.05 *** 0.01) ///
		mtitle("Table 2. Means conditional on presence on agenda and law passage") ///
		append
	
*Table 3. Linear probability models: education agenda on troop changes
//Defining locals
local ed ed_nga_bin state_rev gallup_mip_ed sat
local poli control_house control_senate pres_party cong_polar conserv hearing_days_t1
local covar ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar conserv hearing_days_t1

//No Covariates
	reg agenda troop_dif nyt_def_std year_run, robust
		estimates store ed_agenda_1
//Education Covariates
	reg agenda troop_dif nyt_def_std `ed' year_run, robust
		estimates store ed_agenda_2
//Political Covariates
	reg agenda troop_dif nyt_def_std `poli' year_run, robust
		estimates store ed_agenda_3
//All Covariates
	reg agenda troop_dif nyt_def_std `covar' year_run, robust
		estimates store ed_agenda_4

esttab ed_agenda_1 ed_agenda_2 ed_agenda_3 ed_agenda_4 ///
	using "table_3.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	t(2) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Table 3. Linear probability models: education agenda on troop changes") ///
	se(2) ///
	nomtitle ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(troop_dif) ///
	nonote ///
	nogap	
		
*Table 4. Linear probability models: education law passage on troop changes
//Defining locals
local ed ed_nga_bin state_rev gallup_mip_ed sat
local poli control_house control_senate pres_party cong_polar conserv hearing_days_t1
local covar ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar conserv hearing_days_t1

//No covariates
	reg plaw_passed troop_dif nyt_def_std year_run, robust
		estimates store ed_law_1
//Education Covariates
	reg plaw_passed troop_dif nyt_def_std `ed' year_run, robust
		estimates store ed_law_2
//Political Covariates
	reg plaw_passed troop_dif nyt_def_std `poli' year_run, robust
		estimates store ed_law_3
//All Covariates
	reg plaw_passed troop_dif nyt_def_std `covar' year_run, robust
		estimates store ed_law_4

esttab  ed_law_1 ed_law_2 ed_law_3 ed_law_4 ///
	using "table_4.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	t(2) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Table 4. Linear probability models: education law passage on troop changes") ///
	se(2) ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(troop_dif) ///
	nogap	


*Table 5. Linear probability models: mediation analysis
//Locals
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
	
//Education law on troop difference
	reg plaw_passed troop_dif `covar', robust
		estimates store mediation_1
//Agenda on troop difference
	reg agenda troop_dif `covar', robust
		estimates store mediation_2
//Education laws on agenda
	reg plaw_passed agenda `covar', robust
		estimates store mediation_3
//Education laws on agenda and troop difference
	reg plaw_passed troop_dif agenda `covar', robust
		estimates store mediation_4

esttab mediation_1 mediation_2 mediation_3 mediation_4 ///
	using "table_5.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	t(2) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Table 5. Linear probability models: mediation analysis") ///
	se(2) ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(agenda troop_dif) ///
	nogap		
	

*Appendix Figure A1. Predicted Probability of Presence of Education on Congressional Agenda by Changes in Troop Levels
//Locals
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
	
	reg agenda troop_dif `covar', robust
	margins, at(troop_dif=(-3.5(0.25)3.5)) atmeans
	marginsplot, ///
		title(" ") ///
		xtitle("Changes in Troops (in 100s of Ks)") ///
		ytitle("Predicted Probability of Congressional Attention to Education", size(small)) ///
		xlabel(-3.5 -3 -2 -1 0 1 2 3 3.5) ///
		recast(line) recastci(rarea)  scheme( s1mono )
	graph export "appendix_figure_a1.png", replace

*Appendix Figure A2. Predicted Probability of Education Law Passage on Congressional by Changes in Troop Levels
//Locals
	reg plaw_passed troop_dif `covar', robust
	margins, at(troop_dif=(-3.5(0.25)3.5)) atmeans
	marginsplot, ///
	title(" ") ///
	xtitle("Changes in Troops (in 100s of Ks)") ///
		ytitle("Predicted Probability of Education Law Passage", size(small)) ///
		xlabel(-3.5 -3 -2 -1 0 1 2 3 3.5) ///
		recast(line) recastci(rarea)   scheme( s1mono )
	graph export "appendix_figure_a2.png", replace

//Appendix Table A1. Linear probability models: education agenda on economic covariates
//Locals
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
//GDP
reg agenda gdp troop_dif `covar', robust
		estimates store agenda_alt_econ_1
//Unemployment
reg agenda unemp troop_dif `covar', robust
		estimates store agenda_alt_econ_2
//Gallup MIP Econ
reg agenda gallup_mip_econ troop_dif `covar', robust
		estimates store agenda_alt_econ_3

esttab agenda_alt_econ_1 agenda_alt_econ_2 agenda_alt_econ_3 ///
	using "appendix_table_a1.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	se(3) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Appendix Table A1. Linear probability models: education agenda on economic covariates") ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(gdp unemp gallup_mip_econ) ///
	nogap
	
//Appendix Table A2. Linear probability models: education law on economic covariates
//Locals
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
//GDP
reg plaw_passed agenda gdp troop_dif `covar', robust
		estimates store law_alt_econ_1
//Unemployment
reg plaw_passed agenda unemp troop_dif `covar', robust
		estimates store law_alt_econ_2
//Gallup MIP Econ
reg plaw_passed agenda gallup_mip_econ troop_dif `covar', robust
		estimates store law_alt_econ_3

esttab law_alt_econ_1 law_alt_econ_2 law_alt_econ_3 ///
	using "appendix_table_a2.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	se(3) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Appendix Table A2. Linear probability models: education law on economic covariates") ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(gdp unemp gallup_mip_econ) ///
	nogap
	
*Appendix Table A3. Linear probability models: education agenda on political covariates
//Local
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1
//SOTU
reg agenda sotu troop_dif `covar', robust
		estimates store agenda_alt_poli_1
//President
reg agenda ed_pres troop_dif `covar', robust
		estimates store agenda_alt_poli_2
//Congressional Election
reg agenda elec troop_dif `covar', robust
		estimates store agenda_alt_poli_3
//ESEA Passage (5 Years)
reg agenda esea_5 troop_dif `covar', robust
		estimates store agenda_alt_poli_4
//Education Eras
reg agenda  i.ed_era troop_dif `covar', robust
		estimates store agenda_alt_poli_5
	
esttab agenda_alt_poli_1 agenda_alt_poli_2 agenda_alt_poli_3 agenda_alt_poli_4 agenda_alt_poli_5 ///
	using "appendix_table_a3.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	se(3) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Appendix Table A3. Linear probability models: education agenda on political covariates") ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(troop_dif sotu ed_pres elec  esea_5 1.ed_era 2.ed_era 3.ed_era) ///
	nogap


//Appendix Table A4. Linear probability models: education law on political covariates
//Local
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1
//SOTU
reg plaw_passed agenda sotu troop_dif `covar', robust
		estimates store law_alt_poli_1
//President
reg plaw_passed agenda i.ed_pres troop_dif `covar', robust
		estimates store law_alt_poli_2
//Congressional Election
reg plaw_passed agenda elec troop_dif `covar', robust
		estimates store law_alt_poli_3
//ESEA Passage (5 Years)
reg plaw_passed agenda troop_dif esea_5 `covar', robust
		estimates store law_alt_poli_4
//Education Eras
reg plaw_passed agenda troop_dif i.ed_era  `covar', robust
		estimates store law_alt_poli_5
		
esttab law_alt_poli_1 law_alt_poli_2 law_alt_poli_3 law_alt_poli_4 law_alt_poli_5 ///
	using "appendix_table_a4.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	se(3) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("ppendix Table A4. Linear probability models: education law on political covariates") ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(sotu elec 1.ed_pres esea_5 1.ed_era 2.ed_era 3.ed_era) ///
	nogap

//Appendix Table A5. Ordinary least squares: congressional productivity on troop changes
//Local
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
//Issue count on troop difference
reg issues troop_dif `covar', robust
		estimates store productivity_1
//Laws passed on troop difference
reg laws_passed troop_dif `covar', robust
		estimates store productivity_2
//Hearings on troop difference
reg hearing_tot troop_dif `covar', robust
		estimates store productivity_3	
//Model B-Total laws on troop difference
reg total_laws troop_dif `covar', robust
		estimates store productivity_4	
		
esttab productivity_* ///
	using "appendix_table_a5.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	t(2) ///
	r2 (2) ///
	ar2 (2) ///
	scalar(F  "df_m DF Model"  "df_r DF Residual" N) ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Appendix Table A5. Ordinary least squares: congressional productivity on troop changes") ///
	se(2) ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(agenda troop_dif) ///
	nogap		

//Appendix Table B1. Logistic regression: congressional agenda size and laws passed on troop changes
//Locals
local covar nyt_def_std ed_nga_bin state_rev gallup_mip_ed sat control_house control_senate pres_party cong_polar  ///
	 conserv hearing_days_t1 year_run
//Education law on troop difference
logit plaw_passed troop_dif `covar', robust or
		estimates store log_model_1
//Agenda on troop difference
logit agenda troop_dif `covar', robust or
		estimates store log_model_2
//Education laws on agenda
logit plaw_passed agenda `covar', robust or
		estimates store log_model_3
//Education laws on agenda and troop difference
logit plaw_passed troop_dif agenda `covar', robust or
		estimates store log_model_4

esttab log_model_1 log_model_2 log_model_3 log_model_4 ///
	using "logit_mediation.csv", ///
	label ///
	nodepvars ///
	b(3) ///
	t(2) ///
	pr2(2) ///
	scalar(chi2  "Wald Chi2") ///
	sfmt (2 0 0 0) ///
	replace  ///
	title("Appendix Table B1. Logistic regression: congressional agenda size and laws passed on troop changes") ///
	se(2) ///
	nomtitle ///
	nonote ///
	star(† 0.1 * 0.05  ** 0.01  *** 0.001) ///
	order(agenda troop_dif) ///
	nogap	

//Closing Log
log close _all
log using "ed_attention_log", replace
