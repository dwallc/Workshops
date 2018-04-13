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
	/*	Example I - Math Achievement	*/
	/************************************/
	
	/*	Import the HSB All.xlsx dataset	*/
	
import excel "Data\HSB All.xlsx", sheet("HSB All") firstrow clear

	rename id idGrp

	destring idGrp, replace /*	Originally treated as type-string variable	*/
	
	bysort idGrp: gen idInd = _n
	
	gen id = _n
	
	order id idInd idGrp

	describe
	
	bro
	
	/*	Visually explore the 'mathach' variable.	*/
	
twoway scatter mathach ses if idGrp==1224

twoway scatter mathach ses if idGrp==1288

twoway scatter mathach ses if idGrp==1296
	
	/*	Estimate the null model with only individual-level variation	*/
	
mixed mathach /*	Shows random-effects and residual-error parameter estimates	*/
			  /*	as variances and covariances; the default.					*/
		  
mixed mathach, stddev /*	Shows random-effects and residual-error parameter	*/
					  /*	estimates as standard deviations and correlations.	*/
				  
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

	estat icc

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
	
	
	/************/
	/*	Cleanup	*/
	/************/
	
log close
