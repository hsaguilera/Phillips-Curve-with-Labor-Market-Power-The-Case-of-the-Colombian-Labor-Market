clear all
set more off
cls

* set working directory
cap cd "/.../WagePCunderLMP/"

	
**********************
**  F I G U R E  2  **
**********************

* Wage Phillips Curve by Labor Market Power *

{
use "C:\Investigation\wage phillips curve with labor market power\2_data\monthly_model_data.dta", clear

* clean data
gen year=yofd(dofq(datequat))
collapse (mean)  unrate meansalary* hhi_n_vac, by(year cz)

tsset dpto period
gen dln_meansalary_1=ln(inglabo)-ln(l1.inglabo)
** gen dln_meansalary_college=ln(meansalary_college1)-ln(l1.meansalary_college1)
** gen dln_meansalary_nocollege=ln(meansalary_college0)-ln(l1.meansalary_college0)


cap drop high_mkp
su ihh,d
gen high_mkp=(ihh>r(p50) & ihh!=.)
gen low_mkp=1-high_mkp

eststo clear
eststo: reghdfe dln_meansalary unem, a(dpto period)
*eststo: reghdfe dln_meansalary_college unrate, a(cz year)
*eststo: reghdfe dln_meansalary_nocollege unrate, a(cz year)
eststo: reghdfe dln_meansalary unem if high_mkp==0, a(dpto period)
*eststo: reghdfe dln_meansalary_college unrate if high_mkp==0, a(cz year)
*eststo: reghdfe dln_meansalary_nocollege unrate if high_mkp==0, a(cz year)
eststo: reghdfe dln_meansalary c.unem##c.high_mkp, a(dpto period)
*eststo: reghdfe dln_meansalary_college c.unrate##c.high_mkp, a(cz year)
*eststo: reghdfe dln_meansalary_nocollege c.unrate##c.high_mkp, a(cz year)
esttab, varwidth(40)

preserve

* (fig. 2) *
replace dln_meansalary=dln_meansalary*100
replace unem=unem*100
binscatter dln_meansalary unem, by(high_mkp) ytitle(Wage Inflation (%)) xtitle(Unemployment Rate  (%))  msymbols(diamond oh ) mcolor(blue%50 magenta%50 ) lcolor(blue%50 magenta%50 ) legend(lab(1 Low Labor Market Power)           lab(2 High Labor Market Power) region(lcolor(white))) 
graph export "3.Output/Figure2_wage_philips_curve_by_labor_market_power.png", as(png) name("Graph") replace

restore
}


********************
**  T A B L E  1  **
********************

* Wage Phillips Curve Depending on the Extent of Regional Labor Market Power *

*(uses the same data as fig. 2, run in sequence)

{

label var dln_meansalary " $  \text{Wage Growth}_{c,t} $  "
label var unem " $ \text{Unemployment Rate}_{c,t} $ "
label var high_mkp " $\mathbbm{1}\: \text{LMP}_{c,t}$ "

eststo clear
eststo: reghdfe dln_meansalary c.unem##c.high_mkp, cluster(dpto) noabsorb
estadd local time_fe ""
estadd local dpto_fe ""

eststo: reghdfe dln_meansalary c.unem##c.high_mkp, a(period)  cluster(dpto)
estadd local time_fe " $\checkmark$ "
estadd local dpto_fe ""

eststo: reghdfe dln_meansalary c.unem##c.high_mkp, a( dpto)  cluster(dpto)
estadd local time_fe ""
estadd local dpto_fe "  $\checkmark$  "

eststo: reghdfe dln_meansalary c.unem##c.high_mkp, a(dpto period)  cluster(dpto)
estadd local time_fe " $\checkmark$ "
estadd local dpto_fe  " $\checkmark$ "

esttab


* (table 1.) *
esttab , se star(* 0.1 ** 0.05 *** 0.01) nogap label  ///
	s(N  time_fe cz_fe , label("Obs."  "Time FE" "CZ FE" ) fmt(%16.0fc)) replace  noomit /// 
	 nonotes substitute(_ _) ///
	mgroups("Wage Growth_{c,t}", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	nomtitles b(%16.3f)
	
esttab using "3.Output/Table1_Phillips_Curve_CZ_with_LMP", tex se star(* 0.1 ** 0.05 *** 0.01) nogap label  ///
	s(N  time_fe cz_fe , label("Obs."  "Time FE" "CZ FE" ) fmt(%16.0fc)) replace  noomit /// 
	 nonotes substitute(_ _) noconstant ///
	mgroups("Wage Growth_{c,t}", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
	nomtitles b(%16.3f)
}
