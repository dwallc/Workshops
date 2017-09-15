set more off					/* Get rid of -MORE- in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Introduction to Stata.log", text replace		/* Open new log file.	*/

/*	****************************************************************	*/
/*     	File Name:	Introduction to Stata.do									*/
/*     	Date:   	September 2017											*/
/*      Author: 	Desmond D. Wallace										*/
/*      Purpose:	Basic data manipulation commands in Stata.			*/
/*      Input File:	Data/introData.dta						*/
/*      Output File:	Introduction to Stata.log 								*/
/*	****************************************************************	*/


	/************************************************/
	/* Where are we?. How do we change directories?	*/
	/************************************************/


pwd 				/* print working directory. */
ls 					/* list contents. */

cd "D:\Dropbox\GitHub\Workshops\Stata\Introduction to Stata"	/* change directory. */	
	


	/************************************************/
	/* Opening data in various ways. 				*/
	/************************************************/


	 	  /* Open Stata format data and clear any open data. */

use Data/introData.dta, clear


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

