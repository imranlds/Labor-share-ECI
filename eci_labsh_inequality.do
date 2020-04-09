********************************************************************************	
* Economic complexity, labor share, and inequality of nations                  * 
* Author: Imran Arif                                                           *
* Version: April 08, 2020													   *
********************************************************************************

clear all
cd "C:\Users\Imran\Google Drive\WIP\Data_analysis"

use "Extended_QOG_03262020.dta" 
xtset ocode year

g LN_GDPpc = log(pwt91_rgdpna/pwt91_pop)
g LN_GDPpc_sqr = log(pwt91_rgdpna/pwt91_pop)^2
g LN_pwt91_pop = log(pwt91_pop) 

global j 1
global y ehii_gini
global x eci_plus
global xlist LN_GDPpc LN_GDPpc_sqr LN_pwt91_pop pwt91_hc polity2

global sample year==1965|year==1970|year==1975|year==1980|year==1985|year==1990|year==1995|year==2000|year==2005|year==2010|year==2015

xi: quietly xtreg ehii_gini, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x) ///	
	sortvar($x) ///
	addtext("Dep. var.", " ")

foreach y of varlist $y1  {
foreach x of varlist $x {
xi: quietly xtreg `y' `x' $xlist i.year, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $xlist) ///	
	sortvar($x $xlist) ///
	addtext("Dep. var.", "`y'")

	} 
		} 
		
		
	* Model with polity IV index
	quietly xtreg ehii_gini c.eci_plus##c.pwt91_labsh $xlist i.year if $sample, fe robust
	quietly margins, dydx(eci_plus) at(pwt91_labsh=(0(.05)1)) vsquish
	marginsplot, yline(0) recast(line) draw name(a, replace) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Polity IV", size(small))

	quietly xtreg ehii_gini c.eci_plus##c.pwt91_labsh $xlist i.year if $sample, fe robust
	quietly margins, dydx(pwt91_labsh) at(eci_plus=(0(1)10)) vsquish
	marginsplot, yline(0) recast(line) draw name(a, replace) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Polity IV", size(small))
