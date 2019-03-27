clear all
cls
set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Multilevel Modeling using Stata.log", text replace

/************************************************************************/
/*	Name: Multilevel Modeling using Stata.do							*/
/*	Date: March 27, 2019												*/
/*	Author:	Scott LaCombe												*/
/*	Purpose:	Estimating multilevel models via the 'mixed' command.	*/
/*	Input Files:	Data\HSB All.xlsx									*/
/*	Output File:	Multilevel Modeling using Stata.log					*/
/************************************************************************/


	/************************************/
	/*	Example I - Math Achievement	*/
	/************************************/
	
	/*	Import the HSB All.xlsx dataset	*/
	
import excel "Data\HSB All.xlsx", sheet("HSB All") firstrow clear

	rename id idGrp

	destring idGrp, replace /*	Originally treated as type-string variable	*/
	/// create individual identifier within group
	bysort idGrp: gen idInd = _n
	/// create identifier for entire sample
	gen id = _n
	
	order id idInd idGrp

	describe
	
	bro
	
	/*	Visually explore the 'mathach' variable.	*/
	
twoway scatter mathach ses if idGrp==1224

twoway scatter mathach ses if idGrp==1288

twoway scatter mathach ses if idGrp==1296
	
	/*	Estimate the null model with only individual-level variation	*/
	
mixed mathach 			
		  
mixed mathach, stddev /*	Shows random-effects and residual-error parameter	*/
	/*	Calculate and plot the grand mean	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1224, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1288, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1296, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	/*	Estimate the model with individual- and group-level variation	*/
	
mixed mathach, || idGrp:

	/*	Calculate and plot the group means	*/
	
predict GrandMean2, xb

	label var GrandMean2 "GrandMean2"
	
predict idGrpEffect, reffects relevel(idGrp) /*	If relevel(levelvar) is not	*/
											 /*	specified, random effects	*/
											 /*	are calculated for all		*/
											 /*	levels.						*/

gen idGrpMean = GrandMean2 + idGrpEffect

predict idGrpMean2, fitted relevel(idGrp) /*	Same result as lines 96-105	*/

	sum idGrpMean idGrpMean2

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1224, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1288, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line idGrpMean ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if idGrp==1296, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	/********************************/
	/*	Random Intercept model II	*/
	/********************************/
	
	/*	Estimate a random intercept model with level-1 variables	*/
	
mixed mathach ses i.minority i.female, || idGrp:

	estat icc

	/*	Calculate and plot the group means	*/
	
	drop GrandMean idGrpEffect
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict idGrpEffect, reffects /*	If relevel(levelvar) is not	*/
							  /*	specified, random effects	*/
							  /*	are calculated for all		*/
							  /*	levels.						*/

gen predMathAch = GrandMean + idGrpEffect

predict predMathAch2, fitted /*	Same result as lines 96-105	*/

	sum predMathAch predMathAch2
	
	codebook idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
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
	
	drop GrandMean idGrpEffect predMathAch predMathAch2

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict sesEffect idGrpEffect, reffects

predict predMathAch, fitted

	codebook predMathAch sesEffect idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==6816, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
	
	/****************************/
	/*	Random Slope model II	*/
	/****************************/
	
	/*	Estimate a random slope model with level-1 and level-2 variables	*/
	
mixed mathach c.ses##c.size i.minority i.female i.sector, || idGrp: ses

	estat icc
	
	drop GrandMean sesEffect idGrpEffect predMathAch
	
	/*	Use margins and marginsplot to examine the interaction	*/
	
	qui margins, dydx(ses) at(size=(100(100)2800)) atmeans predict(xb)
	
marginsplot, recast(line) recastci(rarea) ciopts(color(%30))

	margins, dydx(size) at(ses=(-3.758(0.1)2.692)) atmeans
	
marginsplot, recast(line) recastci(rarea) ciopts(color(%30)) yline(0)

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict sesEffect idGrpEffect, reffects

predict predMathAch, fitted

	codebook predMathAch sesEffect idGrpEffect

twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==2305, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==4523, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
twoway (line GrandMean ses, lcolor(black) lwidth(thick) sort) ///
	(line predMathAch ses, lcolor(blue) lwidth(medthick) sort) ///
	(scatter mathach ses, mcolor(red) msize(tiny) sort) if minority==1 & female==1 & idGrp==6816, ///
	ytitle("Math Achievement", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Math Achievement by Group", size(medsmall))
	
		
	/************/
	/*	Cleanup	*/
	/************/
	
log close
