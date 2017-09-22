set more off					/* Get rid of -MORE- in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Data Management Using Stata.log", text replace		/* Open new log file.	*/

/*	****************************************************************	*/
/*     	File Name:	Data Management Using Stata.do									*/
/*     	Date:   	September 2017											*/
/*      Author: 	Desmond D. Wallace										*/
/*      Purpose:	Basic data management and	*/
/*					manipulation commands in Stata.			*/
/*      Input File:	Data/gapminder.csv						*/
/*      Output File:	Data Management Using Stata.log 								*/
/*	****************************************************************	*/


	/************************************************/
	/* Opening data in various ways. 				*/
	/************************************************/

	 /* Open Stata format data and clear any open data. */

import delimited Data/gapminder.csv, clear

	/*	Export data in memory in various formats	*/
	
save Data/gapminder.dta, replace /*	Stata 14 and 15 format	*/
	*saveold Data/gapminder.dta, version(13) replace /*	Stata 13 format	*/
	*saveold Data/gapminder.dta, version(12) replace /*	Stata 12 format	*/
	*saveold Data/gapminder.dta, version(11) replace /*	Stata 11 format	*/
	
export excel using Data\gapminder.xlsx, ///
	firstrow(variables) replace /*	Excel format	*/
	*export excel using Data\gapminder.xls,
		*firstrow(variables) replace /*	Excel 97-03 format	*/
		
*export delimited using Data\gapminder.csv, ///
	*delimiter(,) replace /*	Comma-delimited file	*/
	*export delimited using Data\gapminder.csv, ///
		*delimiter(tab) replace /*	Tab-delimited file	*/
	*export delimited using Data\gapminder.csv, ///
		*delimiter(;) replace /*	Semicolon-delimited file	*/


	/************************************************/
	/* Basic data summaries and tabulations. 		*/
	/* If we are in the right directory, we can 	*/
	/* open up the data set. 						*/
	/************************************************/

  describe
  summarize
  summarize, detail

tab female /* nominal variable */
tab pid3_p /* categorical variable */

  tab1 state female race natlec  /* tab1 is short for tabulation, One-way tables of frequencies */

  tab1 female-educ		/* Illustrate varlist. A varlist is a list of variable names. */

  tab natlec female		/* cross-tabulation, tab dependent independent variables */
  tab natlec female, row
  tab natlec female, row nofreq

	/*	Variable Codebook	*/
	
	codebook female-educ /* Categorical variables */
	
	codebook income /* Ordinal variable treated as an interval variable */
	
	codebook state /* String variable */
  
log close
clear
exit			/* if you use -exit, STATA-, it will close the Stata window. */

