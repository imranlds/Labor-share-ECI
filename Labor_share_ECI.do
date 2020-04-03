********************************************************************************	
* Economic complexity and the rise in labor share: Evidence from a panel data 
* Author: Imran Arif
* Version: April 02, 2020
********************************************************************************
********************************************************************************
* Backgroud work
********************************************************************************
********************************************************************************

set more off
set matsize 11000
clear all
cd "C:\Users\Imran\Google Drive\WIP\Growth inequality and economic complexity of nations\Data"
use "Extended_QOG_03262020.dta" 

xtset ocode year	
********************************************************************************
* Taking logs and copying lables
********************************************************************************

program define _crcslbl	/* varname varname */
	version 6
	args dst src
	local w : variable label `src'
	if `"`w'"' == "" {
		local w "`src'"
	}
	label variable `dst' `"`w'"'
end


* Calcualte five-year averages
	foreach v in  pwt91_labsh pwt91_rnna eci_plus pwt91_rgdpna {
g `v'_5y = (`v'[_n-2] + `v'[_n-1] + `v' + `v'[_n+1] + `v'[_n+2])/5

_crcslbl `v'_5y `v'
	}
	
	* Take log of variables
foreach v in  wdi_popden pwt91_labsh_5y {
g LN_`v' = log(`v') 
_crcslbl LN_`v' `v'
	}
	
g LN_pwt_cs = log(pwt91_rnna_5y/pwt91_rgdpna_5y)	
label var LN_pwt_cs "ln(Capital output ratio)"	

g invest_price = pwt91_pl_i/pwt91_pl_c
label var invest_price "Investment price"

sort ocode year
by ocode: g pop_gr = (pwt91_pop - pwt91_pop[_n-1])/pwt91_pop[_n-1]
label var pop_gr "Population growth"

g LN_rgdp_pc = log(pwt91_rgdpna/pwt91_pop)
label var LN_rgdp_pc "ln(Real GDP p.c.)"


* get the values from the middle of the period i.e. from 2010-2015 = 2013 value
g LN_pwt_cs_ma = LN_pwt_cs[_n-2]
g eci_plus_ma = eci_plus_5y[_n-2]
g LN_pwt91_labsh_ma = LN_pwt91_labsh_5y[_n-2]

drop if !(year==1970|year==1975|year==1980|year==1985|year==1990|year==1995|year==2000|year==2005|year==2010|year==2015)

********************************************************************************
* Generate standarized values 
********************************************************************************

xtset ocode year	
	
global ylist LN_pwt91_labsh_ma
global x eci_plus_ma    
global x1 LN_pwt_cs_ma polity2 LN_rgdp_pc 
global xlist wdi_gdpcapgr invest_price pwt91_hc wdi_gdpagr wdi_gdpind LN_wdi_popden  pop_gr wdi_inflation fi_reg dr_ig wdi_unempne ciri_assn wdi_fdiin

global z1 wdi_fossil 
global z2 wdi_enerenew
global fe i.year

* Drop variables
drop if $ylist ==.| $x ==. | LN_pwt_cs==.| polity2==.| LN_rgdp_pc ==.
*drop if  eci_plus<2 
drop if pwt91_labsh_5y>0.75
keep ocode ccodealp year region pwt91_labsh_5y income_group OECD_nonOECD $ylist $x $x1 $xlist $z1 $z2 

. label variable dr_ig "Globalization"

. label variable fi_reg "Regulation"

* label variable wdi_co2 "CO2 emissions"

. label variable wdi_gdpagr "Agriculture value added"

. label variable wdi_gdpcapgr "GDP p.c. growth"

. label variable wdi_gdpind "Industry value added"

. label variable wdi_inflation "Inflation"

. label variable pwt91_hc "Human capital index"

. label variable LN_wdi_popden "ln(Population density)"

. label variable LN_pwt91_labsh "ln(Share of labor)"

. label variable pwt91_labsh "Share of labor"


	
********************************************************************************
* Table 1: Summary stats
********************************************************************************

	set more off
	outreg2 using "Tables\t_1.tex",  ///
	replace sum(log) ///	
	keep(pwt91_labsh $ylist $x $x1 $xlist $z1) ///	
	sortvar(pwt91_labsh $ylist $x $x1 $xlist $z1) ///	
	dec(2) label 
********************************************************************************
* Table 2: Partial correlaitons
********************************************************************************

set more off
estpost corr $ylist $x $x1, matrix
esttab . using "Tables\t_2.tex", not unstack compress noobs replace booktabs page label star b(2)

twoway scatter $ylist $x || lfit $ylist $x 


graph twoway ///
	(scatter pwt91_labsh_5y $x if OECD_nonOECD=="OECD", msymbol(oh)) ///
	(scatter pwt91_labsh_5y $x if OECD_nonOECD!="OECD", msymbol(x)) ///
	|| lfit pwt91_labsh_5y $x, ///
	legend(label(1 OECD) label(2 Non-OECD) pos(5)  ring(0) col(1)) ///
	xtitle("Economic complexity", size(small)) ///
	ytitle("Labor share", size(small))

********************************************************************************
* Table 3: Corruption economic complexity index
********************************************************************************

 /* H0: Economic complexity increases labor bargaining power relative to capital,
 increasing labor share relative to capital. This happen becuase complex products 
 require more technological knowledge embedded in labor. */
   
global j 3

* Pooled OLS estimator
xi: quietly xtreg $ylist $x
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "No")
	
xi: quietly xtreg $ylist $x $x1
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "No")
	
xi: quietly xtreg $ylist $x $x1 $fe
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "No")

xi: quietly xtreg $ylist $x $x1 $fe, fe 
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
xi: quietly xtreg $ylist $x $x1 $fe, re 
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("RE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addstat(R-squared, e(r2_o)) ///
	addtext("Period effects", "Yes", "Chi-squared (Hausman test)", 49.04)	

********************************************************************************
* Hausman test for fixed versus random effects model
xi: quietly xtreg $ylist $x $x1 $fe, fe 
estimates store fixed

xi: quietly xtreg $ylist $x $x1 $fe, re 
estimates store random

hausman fixed random

* if Prob>chi2 < 0.05 (i.e. significant) use fixed effects

********************************************************************************
* A test for heteroskedasticiy is avalable for the fixed- effects model.
ssc install xtest3

xttest3 

*Prob>chi2 <0.05 indicates presence of heteroskedasticity
********************************************************************************
* To see if time fixed effects are needed when running a FE model.
xi: quietly xtreg $ylist $x $x1 i.year, fe 
testparm _Iyear_1990- _Iyear_2015

* H0: _Iyear_1990- _Iyear_2015=0

* Prob>F < 0.05 indicates we reject the null hypothesis, time FE are needed.


********************************************************************************

********************************************************************************
* Table 4 : Additional Control variables 
********************************************************************************	

global j 4
	
	quietly xtreg $ylist $x $x1 $xlist $fe, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")

foreach v in $xlist { 

xi: quietly xtreg $ylist $x $x1 `v' $fe, fe 
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
		}		
		
********************************************************************************
* Table 5 : Sub-sample estimation 
********************************************************************************

*************Sub-sample OECD****************
global j 5

quietly xtreg $ylist $x $x1 $fe if OECD_nonOECD=="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("OECD") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

	
foreach v in $xlist { 
	
quietly xtreg $ylist $x $x1 `v' $fe if OECD_nonOECD=="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("OECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	}
	
quietly xtreg $ylist $x $x1 $xlist $fe if OECD_nonOECD=="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("OECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

*************Sub-sample nonOECD****************

global j 6

quietly xtreg $ylist $x $x1 $fe if OECD_nonOECD!="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("nonOECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
foreach v in $xlist { 
	
quietly xtreg $ylist $x $x1 `v' $fe if OECD_nonOECD!="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("nonOECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	}
	
quietly xtreg $ylist $x $x1 $xlist $fe if OECD_nonOECD!="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("nonOECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")	
	
********************************************************************************	
* Table 7: * IV regression
********************************************************************************
 /* Other potential IV wdi_refori wdi_expmil wdi_elprodhyd pwt91_cwtfp 
 wdi_import wdi_powcon pwt91_pl_x wdi_co2 pwt91_xr 
  epi_co2kwh epi_eh epi_ehair epi_epi epi_ev */ 
  
 
global x eci_plus_ma LN_pwt_cs_ma    
global x1  polity2 LN_rgdp_pc 
global z1  wdi_fossil L(5).eci_plus_ma L(5).LN_pwt_cs_ma  

************* Full sample*************
global j 7

quietly ivreg2 $ylist  ($x =  $z1) $x1 i.year i.ocode, ffirst
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")		
foreach s in $xlist { 	
	quietly ivreg2 $ylist  ($x =  $z1) $x1 `s' i.year i.ocode, ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")	
		}
quietly ivreg2 $ylist  ($x =  $z1) $x1 $xlist i.year i.ocode, ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
	
*************Sub-sample OECD****************		
global j 7A			
local s_oecd OECD_nonOECD=="OECD"		

quietly ivreg2 $ylist  ($x =  $z1) $x1 i.year i.ocode if `s_oecd', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")		
foreach s in $xlist { 	
	quietly ivreg2 $ylist  ($x =  $z1) $x1 `s' i.year i.ocode if `s_oecd' , ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")	
		}
quietly ivreg2 $ylist  ($x =  $z1) $x1 $xlist i.year i.ocode if `s_oecd', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
		
*************Sub-sample non-OECD****************
		
global j 7B
local s_nonoecd OECD_nonOECD!="OECD"		

quietly ivreg2 $ylist  ($x =  $z1) $x1 i.year i.ocode if `s_nonoecd', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
		
foreach s in $xlist { 	
	quietly ivreg2 $ylist  ($x =  $z1) $x1 `s' i.year i.ocode if `s_nonoecd' , ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")	
		}	
quietly ivreg2 $ylist  ($x =  $z1) $x1 $xlist i.year i.ocode if `s_nonoecd', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
	
		
********************************************************************************
* Table 3: * Differencing
********************************************************************************

global j 7

sort ocode year
by ocode: g LN_pwt91_labsh_d5 = LN_pwt91_labsh - LN_pwt91_labsh[_n-1]
by ocode: g LN_pwt91_labsh_d10 = LN_pwt91_labsh - LN_pwt91_labsh[_n-2]
by ocode: g LN_pwt91_labsh_d30 = LN_pwt91_labsh - LN_pwt91_labsh[_n-6]

by ocode: g eci_plus_d5 = eci_plus - eci_plus[_n-1]
by ocode: g eci_plus_d10 = eci_plus - eci_plus[_n-2]
by ocode: g eci_plus_d30 = eci_plus - eci_plus[_n-6]

by ocode: g LN_pwt_cs_d10 = LN_pwt_cs - LN_pwt_cs[_n-2]
by ocode: g LN_pwt_cs_d30 = LN_pwt_cs - LN_pwt_cs[_n-6]

global ylist  LN_pwt91_labsh
global x eci_plus_d5 

* First differences
quietly reg $ylist $x
outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("First-differences") ///
	keep($x) ///
	sortvar($x) ///
	addtext("Period effects", "No", "Country effects", "No")
	
quietly reg $ylist $x i.year
outreg2 using "Tables\T$j.tex" , ///
append bdec(3) rdec(3) aster nocons label ctitle("First-differences") ///
	keep($x) ///
	sortvar($x)	///
	addtext("Period effects", "Yes", "Country effects", "No")

quietly reg $ylist $x i.year i.ocode
outreg2 using "Tables\T$j.tex" , ///
append bdec(3) rdec(3) aster nocons label ctitle("First-differences") ///
	keep($x) ///
	sortvar($x) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

* Long-run differences
global ylist  LN_pwt91_labsh_d30
global x eci_plus_d30 LN_pwt_cs_d30


quietly reg $ylist $x if OECD_nonOECD=="OECD"
outreg2 using "Tables\T$j.tex" , ///
replace bdec(3) rdec(3) aster nocons label ctitle("Long-run differences") ///
	keep($x) ///
	sortvar($x) ///
	addtext("Period effects", "No", "Country effects", "No")
	
quietly reg $ylist $x if OECD_nonOECD!="OECD"
outreg2 using "Tables\T$j.tex" , ///
append bdec(3) rdec(3) aster nocons label ctitle("Long-run differences") ///
	keep($x) ///
	sortvar($x) ///
	addtext("Period effects", "No", "Country effects", "No")
	
********************************************************************************
* Table 8: Conditional Effects. 
********************************************************************************

global ylist LN_pwt91_labsh_ma 
global x eci_plus_ma    
global x1 LN_pwt_cs_ma LN_rgdp_pc 


global j 10

global int c.eci_plus_ma##c.pwt91_hc

	* Democracy
	quietly xtreg $ylist $int $x1 $xlist $fe, fe
	outreg2 using "Tables\t_$j.tex",  ///
	replace nocons ctitle(Full-sample)  dec(3) label  ///
	keep($x $int $x1 $xlist $z1 ) ///	
	sortvar($x $int $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")

	quietly xtreg $ylist $int $x1 $xlist $fe if OECD_nonOECD=="OECD", fe
	outreg2 using "Tables\t_$j.tex",  ///
	append nocons ctitle(OECD)  dec(3) label  ///
	keep($x $int $x1 $xlist $z1 ) ///	
	sortvar($x $int $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")	

	quietly xtreg $ylist $int $x1 $xlist $fe if OECD_nonOECD!="OECD", fe
	outreg2 using "Tables\t_$j.tex",  ///
	append nocons ctitle(non-OECD)  dec(3) label  ///
	keep($x $int $x1 $xlist $z1 ) ///	
	sortvar($x $int $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
* Marginal effect graphs

	quietly xtreg $ylist $int $x1 $xlist $fe, fe
	quietly margins, dydx(eci_plus_ma) at( pwt91_hc=(0(0.1)5)) vsquish
	marginsplot, yline(0) recast(line) nodraw name(a, replace) ///
	title("Full-sample", size(small)) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Human development index", size(small))
	
	quietly xtreg $ylist $int $x1 $xlist $fe if OECD_nonOECD!="OECD", fe
	quietly margins, dydx(eci_plus_ma) at( pwt91_hc=(0(0.1)5)) vsquish
	marginsplot, yline(0) recast(line) nodraw name(b, replace) ///
	title("Sub-sample non-OECD", size(small)) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Human development index", size(small))
	
	quietly xtreg $ylist $int $x1 $xlist $fe if OECD_nonOECD=="OECD", fe
	quietly margins, dydx(eci_plus_ma) at( pwt91_hc=(0(0.1)5)) vsquish
	marginsplot, yline(0) recast(line) nodraw name(c, replace) ///
	title("Sub-sample OECD", size(small)) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Human development index", size(small))
				
	*graph combine name1 name2 name 3, ycommon will draw all these graphs together with y axis common
	graph combine a b c, col(3)

********************************************************************************
* Table 8: Graph by sub_sample
********************************************************************************	
collapse (mean) pwt91_labsh,  by(OECD_nonOECD year) cw

encode OECD_nonOECD, generate(oecd)

xtset oecd year
 
xtline pwt91_labsh if OECD_nonOECD=="OECD", ///
	nodraw name(a, replace) ///
	subtitle("OECD", size(small)) ///
	xtitle("Year", size(small)) ///
	ytitle("Group average labor share", size(small))
	
xtline pwt91_labsh if OECD_nonOECD!="OECD", ///
	nodraw name(b, replace) ///
	subtitle("Non-OECD", size(small)) ///
	xtitle("Year", size(small)) ///
	ytitle("Group average labor share", size(small))
	
		*graph combine name1 name2 name 3, ycommon will draw all these graphs together with y axis common
	graph combine a b
	
********************************************************************************
* Appendix Table 4A : Additional Control variables 
********************************************************************************	

global x1list wdi_trade bl_asymf wdi_unempne ciri_assn 
global j 4A
	
	quietly xtreg $ylist $x $x1 $xlist $x1list $fe, fe 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $x1list $z1 ) ///	
	sortvar($x $x1 $xlist $z1 $x1list ) ///
	addtext("Period effects", "Yes")

foreach v in $x1list { 

xi: quietly xtreg $ylist $x $x1 `v' $fe, fe 
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $x1list $xlist $z1 ) ///	
	sortvar($x $x1 $x1list $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
		}		




	
