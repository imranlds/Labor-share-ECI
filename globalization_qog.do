

clear all
cd "C:\Users\Imran\Google Drive\WIP\Growth inequality and economic complexity of nations\Data"

use "Extended_QOG.dta" 
xtset ocode year

g LN_wdi_gdpcappppcur = log(wdi_gdpcappppcur)

global j 1
global y wbgi_cce- wbgi_vas
global x dr_eg-dr_sg
global xlist LN_wdi_gdpcappppcur pwt91_hc polity2 

xi: quietly xtreg $y, fe 
	outreg2 using "Tables\T$j.xls" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", " ")

foreach y of varlist $y {
foreach x of varlist $x {
xi: quietly xtreg `y' `x' $xlist i.year, fe robust
	outreg2 using "Tables\T$j.xls" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $xlist) ///	
	sortvar($x $xlist) ///
	addtext("Dep. var.", "`y'")

	} 
		} 
