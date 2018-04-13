clear all
cls
set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Multilevel Modeling using Stata II.log", text replace

/********************************************************************/
/*	Name: Multilevel Modeling using Stata II.do						*/
/*	Date: April 13, 2018											*/
/*	Author:	Desmond D. Wallace										*/
/*	Purpose:	Estimating basic random intercept and random slope	*/
/*					multilevel models via the 'mixed' command.	 	*/
/*	Input Files:	Data\HSB All.xlsx								*/
/*	Output File:	Multilevel Modeling using Stata I.log			*/
/********************************************************************/


	/************************************/
	/*	Import the HSB All.xlsx dataset	*/
	/************************************/
	
import excel "Data\HSB All.xlsx", sheet("HSB All") firstrow clear

	rename id idGrp

	destring idGrp, replace /*	Originally treated as type-string variable	*/
	
	bysort idGrp: gen idInd = _n
	
	gen id = _n
	
	order id idInd idGrp

	describe
	
	bro
	
	
	/************************************************/
	/*	Visually explore the 'mathach' variable.	*/
	/************************************************/
	
twoway scatter mathach ses if idGrp==2305

twoway scatter mathach ses if idGrp==4523

twoway scatter mathach ses if idGrp==6816
	
	
	/****************************/
	/*	Random Intercept model	*/
	/****************************/
	
	/*	Estimate a random intercept model with level-1 variables	*/
	
mixed mathach ses i.minority i.female, || idGrp:

	estat icc

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict idGrpEffect, reffects /*	If relevel(levelvar) is not	*/
							  /*	specified, random effects	*/
							  /*	are calculated for all		*/
							  /*	levels.						*/

gen idGrpMean = GrandMean + idGrpEffect

predict idGrpMean2, fitted /*	Same result as lines 96-105	*/

	sum idGrpMean idGrpMean2
	
	codebook idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==6816, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	
	/****************************/
	/*	Random Slope model I	*/
	/****************************/
	
	/*	Estimate a random slope model with level-1 variables	*/
	
mixed mathach ses i.minority i.female, || idGrp: ses

	estat icc
	
	drop GrandMean sesEffect idGrpEffect idGrpMean

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict sesEffect idGrpEffect, reffects

predict idGrpMean, fitted

	codebook idGrpMean sesEffect idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==6816, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	
	/****************************/
	/*	Random Slope model II	*/
	/****************************/
	
	/*	Estimate a random slope model with level-1 and level-2 variables	*/
	
mixed mathach c.ses##c.size minority female i.sector, || idGrp: ses

	estat icc
	
	drop GrandMean sesEffect idGrpEffect idGrpMean
	
	/*	Use margins and marginsplot to examine the interaction	*/
	
	qui margins, dydx(ses) at(size=(100(100)2800)) atmeans
	
marginsplot, recast(line) recastci(rarea) ciopts(color(%30))

	qui margins, dydx(size) at(ses=(-3.758(0.1)2.692)) atmeans
	
marginsplot, recast(line) recastci(rarea) ciopts(color(%30)) yline(0)

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict sesEffect idGrpEffect, reffects

predict idGrpMean, fitted

	codebook idGrpMean sesEffect idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==6816, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	
	/************/
	/*	Cleanup	*/
	/************/
	
log close
