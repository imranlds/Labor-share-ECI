
set more off
set matsize 11000
clear all
cd "C:\Users\Imran\Google Drive\WIP\Growth inequality and economic complexity of nations\Data"
use "Extended_QOG_03262020.dta" 

xtset ocode year	
		
program define _crcslbl	/* varname varname */
	version 6
	args dst src
	local w : variable label `src'
	if `"`w'"' == "" {
		local w "`src'"
	}
	label variable `dst' `"`w'"'
end

		
* Take log of variables
foreach v of varlist wdi_popden  gbd_all_nr- gbd_res_nr {
g LN_`v' = log(`v') 
_crcslbl LN_`v' `v'
	}
	
g LN_rgdp_pc = log(pwt91_rgdpna/pwt91_pop)
label var LN_rgdp_pc "ln(Real GDP p.c.)"
	
global j 1
global y LN_gbd_all_nr- LN_gbd_res_nr
global x LN_gbd_all_nr-LN_gbd_res_nr
global xlist LN_rgdp_pc wdi_gdpcapgr pwt91_hc LN_wdi_popden  dr_ig wdi_trade pwt91_csh_g ifpri_gdphealth_ppp wdi_gdpagr

drop if !(year==1996|year==2000|year==2005|year==2010|year==2015)
replace year=1995 if year==1996

xi: quietly xtreg $y $xlist, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", " ")

foreach y of varlist $y {
foreach x of varlist $x {
xi: quietly xtreg `y' L(5).`y' `x' $xlist i.year, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x L(5).`y' $xlist) ///	
	sortvar($x L(5).`y' $xlist) ///
	addtext("Dep. var.", "`y'")

	} 
		} 
		
		
xi: quietly xtreg $y $xlist, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", " ")

foreach y of varlist $y {
foreach x of varlist $x {
xi: quietly xtreg D.(`y' `x' $xlist) if $sample, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep(D.$x L(1).`y' D.$xlist) ///	
	sortvar(D.$x L(1).`y' D.$xlist) ///
	addtext("Dep. var.", "`y'")

	} 
		} 
		
twoway scatter $ylist $x if $sample || lfit $ylist $x if $sample



global ylist LN_rgdp_pc bl_asymf bl_lsmf pwt91_hc 
