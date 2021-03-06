clear all
cls
set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Multilevel Modeling using Stata I.log", text replace

/************************************************************/
/*	Name: Multilevel Modeling using Stata I.do				*/
/*	Date: March 23, 2018									*/
/*	Author:	Desmond D. Wallace								*/
/*	Purpose:	Estimating null random intercept multilevel	*/
/*					models via the 'mixed' command.	 		*/
/*	Input Files:	Data\HSB All.xlsx						*/
/*					productivity.dta						*/
/*	Output File:	Multilevel Modeling using Stata I.log	*/
/************************************************************/


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
	
	
	/************************/
	/*	Example II - GSP	*/
	/************************/
	
	/*	First, load the productivity.dta dataset	*/
	
use "http://www.stata-press.com/data/r12/productivity.dta", clear

	describe gsp year state region
	
	/*	Visually explore the 'gsp' variable.	*/
	
twoway (line gsp year, connect(ascending)), ///
	by(region, title("log(Gross State Product) by Region", size(medsmall)))
	
	/*	Estimate the null model with only individual-level variation	*/
	
mixed gsp

	/*	Calculate and plot the grand mean	*/

predict GrandMean, xb

	label var GrandMean "GrandMean"
	
twoway (line GrandMean year, lcolor(black) lwidth(thick)) ///
	(scatter gsp year, mcolor(red) msize(tiny)), ///
	ytitle(log(Gross State Product), margin(medsmall)) ///
	legend(cols(4) size(small)) ///
    title("GSP for 1970-1986 by Region", size(medsmall))

	summarize gsp

	drop GrandMean
	
	/*	Estimate the model with individual- and group-level variation	*/
	
mixed gsp, || region: || state:

	estat icc

	/*	Calculate and plot the group means	*/
	
predict GrandMean, xb

	label var GrandMean "GrandMean"
	
predict RegionEffect, reffects relevel(region)

predict StateEffect, reffects relevel(state)

gen RegionMean = GrandMean + RegionEffect

gen StateMean = GrandMean + RegionEffect + StateEffect

predict RegionMean2, fitted relevel(region)

predict StateMean2, fitted

	sum RegionMean RegionMean2 StateMean StateMean2
	
twoway (line GrandMean year, lcolor(black) lwidth(thick)) ///
	(line RegionMean year, lcolor(blue) lwidth(medthick)) ///
	(line StateMean year, lcolor(green) connect(ascending)) ///
	(scatter gsp year, mcolor(red) msize(tiny)), ///
	ytitle(log(Gross State Product), margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	by(region, title("Multilevel Model of GSP by Region", size(medsmall)))
	
twoway (line GrandMean year, lcolor(black) lwidth(thick)) ///
	(line RegionMean year, lcolor(blue) lwidth(medthick)) ///
	(line StateMean year, lcolor(green) connect(ascending)) ///
	(scatter gsp year, mcolor(red) msize(medsmall)) if region==7, ///
	ytitle("log(Gross State Product)", margin(medsmall)) ///
	legend(cols(4) size(small)) ///
	title("Multilevel Model of GSP for Region 7", size(medsmall))
	
	
	/************/
	/*	Cleanup	*/
	/************/
	
log close
