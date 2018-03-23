clear all
cls
set more off
  quietly log
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Multilevel Modeling using Stata I.log", text replace

/************************************************************************/
/*	Name: Multilevel Modeling using Stata I.do							*/
/*	Date: March 23, 2018												*/
/*	Author:	Desmond D. Wallace											*/
/*	Purpose:	Estimating basic random slope and random coefficient	*/
/*					multilevel models via the 'mixed' command.	 		*/
/*	Input File:	productivity.dta										*/
/*	Output File:	Multilevel Modeling using Stata I.log				*/
/************************************************************************/


	/************************************************/
	/*	First, load the productivity.dta dataset	*/
	/************************************************/
	
use "http://www.stata-press.com/data/r12/productivity.dta", clear

	describe
	
	
	/****************************************/
	/*	Visually explor the 'gsp' variable.	*/
	/****************************************/
	
twoway (line gsp year, connect(ascending)), ///
	by(region, title("log(Gross State Product) by Region", size(medsmall)))
	
	
	/****************************************************/
	/*	Estimate a null model via the 'mixed' command.	*/
	/****************************************************/
	
mixed gsp /*	Shows random-effects and residual-error parameter estimates	*/
		  /*	as variances and covariances; the default.					*/
		  
mixed gsp, stddev /*	Shows random-effects and residual-error parameter	*/
				  /*	estimates as standard deviations and correlations.	*/
