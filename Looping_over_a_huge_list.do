

clear all
cd "C:\Users\Imran\Google Drive\WIP\Growth inequality and economic complexity of nations\Data"

use "Extended_QOG.dta" 
xtset ocode year

global j 1
global y eci_plus
global x wbgi_cce-wdi_wofm15

xi: quietly xtreg $y, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", " ")

foreach y in $y {
foreach x of varlist $x {
xi: quietly xtreg `y' `x' i.year, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", "`y'")

	} 
		} 
