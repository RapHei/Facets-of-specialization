
*** Define models
global UV_1 Elite Female White Insider log_Pub_Cum

global UV_2 Elite Female White Insider log_Pub_Cum ///
HHI Novelty Consistency

global UV_3 Elite Female White Insider log_Pub_Cum ///
HHI Novelty Consistency ///
T1-T46 T48-T60

global UV_4 Elite Female White Insider log_Pub_Cum ///
HHI Novelty Consistency Consistency_Square ///
T1-T46 T48-T60

global UV_5 Elite Female White Insider log_Pub_Cum ///
HHI Novelty Consistency Consistency_Square ///
c.HHI#c.log_Pub_Cum c.Novelty#c.log_Pub_Cum /// 
c.Consistency#c.log_Pub_Cum c.Consistency_Square#c.log_Pub_Cum ///
T1-T46 T48-T60


* Setup
set more off
cd "C:\Users\ac135138\Documents\Github\Facets-of-specialization"


*********
**** Load file for Model 1
*********
use "Output\Stata\Repo_Event_DF_Final_M1.dta", clear
gen log_Pub_Cum = log(Pub_Total + 1)

stset Event_Time [pw=Weight], failure(Advisor==1) id(ID) 

*** Model 1
streg $UV_1, dist(weib) vce(robust)
estimates store m1


*********
**** Load file for Models 2-5
*********
use "Output\Stata\Repo_Event_DF_Final_0.9.dta", clear
* Z-standardize
egen HHI = std(hhi)
egen Consistency = std(Consistency_Mean)
gen log_Pub_Cum = log(Pub_Total + 1)
egen Novelty = std(novel_inte)
gen Consistency_Square = Consistency^2

stset Event_Time [pw=Weight], failure(Advisor==1) id(ID) 


*** Models 2-5
quiet streg $UV_2, dist(weib) vce(robust)
estimates store m2

quiet streg $UV_3, dist(weib) vce(robust)
estimates store m3

quiet streg $UV_4, dist(weib) vce(robust)
estimates store m4

quiet streg $UV_5, dist(weib) vce(robust)
estimates store m5


esttab m1 m2 m3 m4 m5 using "Output/Stata/Tab3_MainResults.html", ///
 b(%9.0g ) eform not se star varwidth(25) aic replace

 
*********
* FIGURES
*********
 
* Fig4

** export to R (cf. Repo_Fig4_TargetSpec.R; might use alternative baselines, cf. Repo_TargetSpec.R)
*** SE
esttab m5 using "Output/Stata/SE_Single_Base09.csv", ///
  eform not se replace wide plain
*** Pvalues
esttab m5 using "Output/Stata/Pval_Single_Base09.csv", ///
  eform not p replace wide plain

  

* Fig5
*** Plot to compare effect size
quiet streg $UV_3, dist(weib) vce(robust)


* T50, in color
stcurve, cumhaz at1(White=1 T50=1) at2(White=1 T50=0) at3(White=0 T50=1) at5(White=0 T50=0) ///
	ytitle("Cumulative Hazard", size(small)) ///
	xtitle("Years", size(small)) ///	
	ylabel(,labsize(small)) ///
	xlabel(0(5)35,labsize(small)) ///
	title("") ///
	scheme(s1mono) ///
	legend(label(1 White=1, T50=1) label(2 White=1, T50=0) label(3 White=0, T50=1) label(4 White=0, T50=0) nobox region(style(none)) size(vsmall)) ///
	clcolor(blue blue cyan cyan) ///
	lpattern(solid dash solid dash) ///
	saving(Output/Figures/Fig5_White_Stcurve_T50, replace)
graph export Output/Figures/Fig5_White_Stcurve_T50.pdf, replace

stcurve, cumhaz at1(Female=1 T50=1) at2(Female=1 T50=0) at3(Female=0 T50=1) at5(Female=0 T50=0) ///
	ytitle("Cumulative Hazard", size(small)) ///
	xtitle("Years", size(small)) ///
	ylabel(,labsize(small)) ///
	xlabel(,labsize(small)) ///
	xlabel(0(5)35,labsize(small)) ///
	title("") ///
	scheme(s1mono) ///
	legend(label(1 Female=1, T50=1) label(2 Female=1, T50=0) label(3 Female=0, T50=1) label(4 Female=0, T50=0) nobox region(style(none)) size(vsmall)) ///
	clcolor(blue blue cyan cyan) ///
	lpattern(solid dash solid dash) ///
	saving(Output/Figures/Fig5_Female_Stcurve_T50, replace)
graph export Output/Figures/Fig5_Female_Stcurve_T50.pdf, replace

* Save multiple graphs as one
gr combine Output/Figures/Fig5_White_Stcurve_T50.gph ///
	Output/Figures/Fig5_Female_Stcurve_T50.gph, col(2) ycommon xcommon
graph export Output/Figures/Fig5_WhiteFemale_T50_Base09.pdf, replace


* T50, black&white, print version
stcurve, cumhaz at1(White=1 T50=1) at2(White=1 T50=0) at3(White=0 T50=1) at5(White=0 T50=0) ///
	ytitle("Cumulative Hazard", size(small)) ///
	xtitle("Years", size(small)) ///	
	ylabel(,labsize(small)) ///
	xlabel(0(5)35,labsize(small)) ///
	title("") ///
	scheme(s1mono) ///
	legend(label(1 White=1, T50=1) label(2 White=1, T50=0) label(3 White=0, T50=1) label(4 White=0, T50=0) nobox region(style(none)) size(vsmall)) ///
	clcolor(black black gs12 gs12) ///
	lpattern(solid dash solid dash) ///
	saving(Output/Figures/Fig5_White_Stcurve_T50_BW, replace)
graph export Output/Figures/Fig5_White_Stcurve_T50_BW.pdf, replace

stcurve, cumhaz at1(Female=1 T50=1) at2(Female=1 T50=0) at3(Female=0 T50=1) at5(Female=0 T50=0) ///
	ytitle("Cumulative Hazard", size(small)) ///
	xtitle("Years", size(small)) ///
	ylabel(,labsize(small)) ///
	xlabel(,labsize(small)) ///
	xlabel(0(5)35,labsize(small)) ///
	title("") ///
	scheme(s1mono) ///
	legend(label(1 Female=1, T50=1) label(2 Female=1, T50=0) label(3 Female=0, T50=1) label(4 Female=0, T50=0) nobox region(style(none)) size(vsmall)) ///
	clcolor(black black gs12 gs12) ///
	lpattern(solid dash solid dash) ///
	saving(Output/Figures/Fig5_Female_Stcurve_T50_BW, replace)
graph export Output/Figures/Fig5_Female_Stcurve_T50_BW.pdf, replace

* Save multiple graphs as one
gr combine Output/Figures/Fig5_White_Stcurve_T50_BW.gph ///
	Output/Figures/Fig5_Female_Stcurve_T50_BW.gph, col(2) ycommon xcommon
graph export Output/Figures/Fig5__WhiteFemale_T50_Base09_BW.pdf, replace


* FIG6
*** Margins plot 
quiet streg $UV_5, dist(weib) vce(robust) 

margins [pw=Weight], predict(hr) at(Consistency = (-0.5 0 0.5 1 1.5) log_Pub_Cum = (0 1 2 3) )  vce(unconditional) ///
	saving(Output/Stata/Margins_Base09_HR, replace) 

use Output/Stata/Margins_Base09_HR, clear
* nicer colnames
rename _at5 log_Pub_Total
rename _at8 Z_Identity	

* just to smooth figure for extreme value of consistency = 1.5:
gen margin_scale = _margin
replace margin_scale = 9 if margin_scale > 8

* colors
twoway (contour margin_scale Z_Identity log_Pub_Total, ///
	ccuts(0.0(0.5)2 2(1)8) ), ///
	ytitle("Standardized values of {it:consistency} metric")  ///
	xtitle("Log({it:Cumulated publications})") ///
	ztitle("Hazard ratio for {it:first time advisorship}") ///
	graphregion(fcolor(white))
graph export Output/Figures/Fig6_Twoway_Base09_HR.pdf, replace

* black and white
twoway (contour margin_scale Z_Identity log_Pub_Total, ///
	ccuts(0.0(0.5)2 2(1)8) ///
	ccolors(gs1 gs2 gs3 gs4 gs5 gs6 gs7 gs8 gs9 gs10 gs11 gs12 gs13) ), ///
	ytitle("Standardized values of {it:consistency} metric")  ///
	xtitle("Log({it:Cumulated publications})") ///
	ztitle("Hazard ratio for {it:first time advisorship}") ///
	graphregion(fcolor(white)) 
graph export Output/Figures/Fig6_Twoway_Base09_HR_BW.pdf, replace
