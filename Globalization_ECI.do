********************************************************************************	
* Economic complexity and the rise in labor share: Evidence from a panel data 
* Author: Imran Arif
* Version: March 3, 2020
********************************************************************************
********************************************************************************
* Backgroud work
********************************************************************************
********************************************************************************

clear all
cd "C:\Users\Imran\Google Drive\WIP\Globalization and ECI"

use "C:\Users\Imran\Google Drive\WIP\Growth inequality and economic complexity of nations\Data\Extended_QOG.dta" 

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

	}
	
	* Take log of variables
foreach v in  wdi_popden pwt91_labsh_5y {
g LN_`v' = log(`v') 
_crcslbl LN_`v' `v'
	}
	
g LN_pwt_cs_5y = log(pwt91_rnna_5y/pwt91_rgdpna_5y)	
label var LN_pwt_cs_5y "ln(Capital output ratio)"	

g invest_price = pwt91_pl_i/pwt91_pl_c
label var invest_price "Investment price"

sort ocode year
by ocode: g pop_gr = (pwt91_pop - pwt91_pop[_n-1])/pwt91_pop[_n-1]
label var pop_gr "Population growth"

g LN_rgdp_pc = log(pwt91_rgdpna/pwt91_pop)
label var LN_rgdp_pc "ln(Real GDP p.c.)"

drop if !(year==1985|year==1990|year==1995|year==2000|year==2005|year==2010|year==2015)

********************************************************************************
* Generate standarized values 
********************************************************************************
	
global ylist eci_plus 
global x  dr_ig dr_eg dr_pg dr_sg  
global x1 LN_pwt_cs polity2 LN_rgdp_pc 
global xlist wdi_gdpcapgr invest_price pwt91_hc wdi_gdpagr wdi_gdpind LN_wdi_popden  pop_gr wdi_inflation fi_reg 

global z1 wdi_co2 
global z2 wdi_enerenew
global fe i.year

* Drop variables
keep ocode ccodealp year region pwt91_labsh income_group OECD_nonOECD $ylist $x $x1 $xlist $z1 $z2
drop if $ylist ==.| $x ==.
drop if  eci_plus<2 


. label variable dr_ig "Globalization"

. label variable fi_reg "Regulation"

. label variable wdi_co2 "CO2 emissions"

. label variable wdi_gdpagr "Agriculture value added"

. label variable wdi_gdpcapgr "GDP p.c. growth"

. label variable wdi_gdpind "Industry value added"

. label variable wdi_inflation "Inflation"

. label variable pwt91_hc "Human capital index"

. label variable LN_wdi_popden "ln(Population density)"

. label variable LN_pwt91_labsh_5y "ln(Share of labor)"

. label variable pwt91_labsh_5y "Share of labor"

. label variable LN_pwt_cs_5y "ln(Capital-output ratio)"

. label variable eci_plus_5y "Economic complexity"
	
********************************************************************************
* Table 1: Summary stats
********************************************************************************

	set more off
	outreg2 using "Tables\t_1.tex",  ///
	replace sum(log) ///	
	keep(pwt91_labsh_5y $ylist $x $x1 $xlist $z1) ///	
	sortvar(pwt91_labsh $ylist $x $x1 $xlist $z1) ///	
	dec(2) label 
********************************************************************************
* Table 2: Partial correlaitons
********************************************************************************

set more off
estpost corr $ylist $x $x1, matrix
esttab . using "Tables\t_2.tex", not unstack compress noobs replace booktabs page label star b(2)

twoway scatter $ylist $x || lfit $ylist $x 
twoway scatter $ylist $x  if OECD_nonOECD=="OECD" || lfit $ylist $x  if OECD_nonOECD=="OECD"
twoway scatter $ylist $x if OECD_nonOECD!="OECD" || lfit $ylist $x if OECD_nonOECD!="OECD"


********************************************************************************
* Table 3: Corruption economic complexity index
********************************************************************************

 /* H0: Economic complexity increases labor bargaining power relative to capital,
 increasing labor share relative to capital. This happen becuase complex products 
 require more technological knowledge embedded in labor. */
  
  
 local x1  dr_ig dr_eg dr_pg dr_sg 
 
global j 3

* Pooled OLS estimator
xi: quietly xtreg $ylist `x1' L(5).$ylist, robust
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 L(5).$ylist) ///	
	sortvar($x $x1 $xlist $z1 L(5).$ylist) ///
	addtext("Period effects", "No")
	
xi: quietly xtreg $ylist `x1' $x1 L(5).$ylist, robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 L(5).$ylist) ///	
	sortvar($x $x1 $xlist $z1 L(5).$ylist) ///
	addtext("Period effects", "No")
	
xi: quietly xtreg $ylist `x1' $x1 L(5).$ylist $fe, robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Pooled") ///
	addstat(R-squared, e(r2_o), "Number of ocode", e(N_g)) ///
	keep($x $x1 $xlist $z1 L(5).$ylist) ///	
	sortvar($x $x1 $xlist $z1 L(5).$ylist) ///
	addtext("Period effects", "Yes")

xi: quietly xtreg $ylist `x1' $x1 L(5).$ylist $fe, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 L(5).$ylist) ///	
	sortvar($x $x1 $xlist $z1 L(5).$ylist) ///
	addtext("Period effects", "Yes")
	
xi: quietly xtreg $ylist `x1' $x1 L(5).$ylist $fe, re robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("RE") ///
	keep($x $x1 $xlist $z1 L(5).$ylist) ///	
	sortvar($x $x1 $xlist $z1 L(5).$ylist) ///
	addstat(R-squared, e(r2_o)) ///
	addtext("Period effects", "Yes")	

********************************************************************************
* Hausman test for fixed versus random effects model
xi: quietly xtreg $ylist dr_ig $x1  $fe, fe 
estimates store fixed

xi: quietly xtreg $ylist dr_ig $x1  $fe, re 
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
* Table 4 : 
********************************************************************************	

global j 4
	
	quietly xtreg $ylist $x $fe, fe robust 
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")

foreach v in $x { 

xi: quietly xtreg $ylist `v' $fe, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
		}	
		
foreach v in $x { 

xi: quietly xtreg $ylist `v' $x1 $fe, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
		}
		
foreach v in $x { 

xi: quietly xtreg $ylist `v' $x1 $xlist $fe, fe robust
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("FE") ///
	keep($x $x1 $xlist $z1 ) ///	
	sortvar($x $x1 $xlist $z1 ) ///
	addtext("Period effects", "Yes")
	
		}
		
********************************************************************************
* Table : Sub-sample estimation 
********************************************************************************
global j 5

quietly xtreg $ylist $x $x1 $fe if OECD_nonOECD=="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("OECD") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
quietly xtreg $ylist $x $x1 $fe if OECD_nonOECD!="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Non_OECD") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

quietly xtreg $ylist $x $x1 $fe if income_group=="High income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("High income") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
quietly xtreg $ylist $x $x1 $fe if income_group=="Upper middle income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Middle income") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

quietly xtreg $ylist $x $x1 $fe if income_group=="Lower middle income"|income_group=="Low income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Low income") ///
	keep($x $x1) ///
	sortvar($x $x1) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	

********************************************************************************
* Table : Sub-sample estimation with additional control variables
********************************************************************************
	
global j 6

quietly xtreg $ylist $x $x1 $xlist $fe if OECD_nonOECD=="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("OECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
quietly xtreg $ylist $x $x1 $xlist $fe if OECD_nonOECD!="OECD", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Non_OECD") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

quietly xtreg $ylist $x $x1 $xlist $fe if income_group=="High income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("High income") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
quietly xtreg $ylist $x $x1 $xlist $fe if income_group=="Upper middle income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Middle income") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")

quietly xtreg $ylist $x $x1 $xlist $fe if income_group=="Lower middle income"|income_group=="Low income", fe
outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("Low income") ///
	keep($x $x1 $xlist) ///
	sortvar($x $x1 $xlist) ///
	addtext("Period effects", "Yes", "Country effects", "Yes")
	
	
********************************************************************************	
* Table 3: * IV regression
********************************************************************************
 wdi_refori wdi_expmil wdi_elprodhyd pwt91_cwtfp wdi_import wdi_powcon pwt91_pl_x wdi_co2 pwt91_xr
 
 
global x eci_plus_5y LN_pwt_cs_5y    
global x1  polity2 LN_rgdp_pc 
global z1  wdi_fossil L(5).eci_plus_5y L(5).LN_pwt_cs_5y 


global j 7


global sample OECD_nonOECD=="OECD" OECD_nonOECD!="OECD" 

	quietly ivreg2 $ylist  ($x =  $z1) $x1 i.year i.ocode, ffirst
	outreg2 using "Tables\T$j.tex" , ///
	replace bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 ) ///
	sortvar($x $x1 $z1 ) ///
	addtext("Period effects", "Yes")

foreach s in $sample { 

quietly ivreg2 $ylist  ($x =  $z1) $x1 i.year i.ocode if `s', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 ) ///
	sortvar($x $x1 $z1 ) ///
	addtext("Period effects", "Yes")
	
		}
		
// Additional control variables (xlist)
		
	quietly ivreg2 $ylist  ($x =  $z1) $x1 $xlist i.year i.ocode, ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
		
foreach s in $sample { 
	
	quietly ivreg2 $ylist  ($x =  $z1) $x1 $xlist i.year i.ocode if `s', ffirst
	outreg2 using "Tables\T$j.tex" , ///
	append bdec(3) rdec(3) aster nocons label ctitle("IV") ///
	addstat(1st stage F test, e(widstat), Over-id (p-value), e(jp)) ///
	keep($x $x1 $z1 $xlist) ///
	sortvar($x $x1 $z1 $xlist) ///
	addtext("Period effects", "Yes")
	
		}	
		
********************************************************************************
* Table 3: * Differencing
********************************************************************************

global j 7

sort ocode year
by ocode: g LN_pwt91_labsh_d5 = LN_pwt91_labsh - LN_pwt91_labsh[_n-1]
by ocode: g LN_pwt91_labsh_d10 = LN_pwt91_labsh - LN_pwt91_labsh[_n-2]
by ocode: g LN_pwt91_labsh_d30 = LN_pwt91_labsh - LN_pwt91_labsh[_n-6]

by ocode: g std_eci_plus_d5 = std_eci_plus - std_eci_plus[_n-1]
by ocode: g std_eci_plus_d10 = std_eci_plus - std_eci_plus[_n-2]
by ocode: g std_eci_plus_d30 = std_eci_plus - std_eci_plus[_n-6]

global ylist  LN_pwt91_labsh_d10
global x std_eci_plus_d10 

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
global x std_eci_plus_d30


quietly reg $ylist $x
outreg2 using "Tables\T$j.tex" , ///
append bdec(3) rdec(3) aster nocons label ctitle("Long-run differences") ///
	keep($x) ///
	sortvar($x) ///
	addtext("Period effects", "No", "Country effects", "No")	
	
********************************************************************************
* Table 8: Conditional Effects. Dependent
********************************************************************************

global ylist LN_pwt91_labsh 
global x std_eci_plus    
global x1alt LN_pwt_cs LN_rgdp_pc 


global j 8

global int c.std_eci_plus##c.polity2

	* Democracy
	quietly xtreg $ylist $int $fe, fe
	outreg2 using "Tables\t_$j.tex",  ///
	replace nocons ctitle(Levels)  dec(3) label  ///
	keep($x $int $x1alt $xlist $z1 ) ///	
	sortvar($x $int $x1alt $xlist $z1 ) ///
	addtext("Period effects", "Yes")

	quietly xtreg $ylist $int $x1alt $fe, fe
	outreg2 using "Tables\t_$j.tex",  ///
	append nocons ctitle(Levels)  dec(3) label  ///
	keep($x $int $x1alt $xlist $z1 ) ///	
	sortvar($x $int $x1alt $xlist $z1 ) ///
	addtext("Period effects", "Yes")	

* Marginal effect graphs

	* Model with polity IV index
	quietly xtreg $ylist $int $fe, fe
	quietly margins, dydx(std_eci_plus) at( polity2=(-10(1)10)) vsquish
	marginsplot, yline(0) recast(line) nodraw name(a, replace) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Polity IV", size(small))

	quietly xtreg $ylist $int $x1alt $fe, fe
	quietly margins, dydx(std_eci_plus) at( polity2=(-10(1)10)) vsquish
	marginsplot, yline(0) recast(line) nodraw name(b, replace) ///
	ytitle ("Effects on Linear Prediction",size(small)) ///
	xtitle ("Polity IV", size(small))
	
				
	*graph combine name1 name2 name 3, ycommon will draw all these graphs together with y axis common
	graph combine a b 
	
	
