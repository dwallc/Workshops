#delimit cr		/* Set end of line delimiter (this is the default). 	*/
version 12			/* Declare version for forward compatibility.	*/
set more off					/* Get rid of -MORE- in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using comp01basic01.log, text replace		/* Open new log file.	*/

/*	****************************************************************	*/
/*     	File Name:	comp01basic01.do									*/
/*     	Date:   	August 2015											*/
/*      Author: 	Abigail A. Rury										*/
/*      Purpose:	Basic data manipulation commands in Stata.			*/
/*      Input File:	comp01.dta, comp01data01.dta,						*/
/*      		comp01data01.csv,										*/
/*      		comp01data01.txt,										*/
/*      		comp01data01.dct										*/
/*      Output File:	comp01basic01.log, 								*/
/*			comp01basic01.dta,											*/
/*			comp01basic01-version1.dta,									*/
/*			comp01basic01.csv,											*/
/*			comp01basic01.dct											*/
/*	****************************************************************	*/


	/************************************************/
	/* Where are we?. How do we change directories?	*/
	/************************************************/


pwd 				/* print working directory. */
ls 					/* list contents. */

cd H:/StataWorkshop/computers/
cd "/Users/ab8675309/Documents/Stata Workshop/computers/comp01data"	/* change directory. */	
	
cd "/Users/ab8675309/Documents/Stata Workshop/computers/comp01data"


	/************************************************/
	/* Opening data in various ways. 				*/
	/************************************************/


	 	  /* Open Stata format data and clear any open data. */

use comp01.dta, clear


	 	  /* Open comma-separated data and clear any open data. */

insheet using data/comp01data01.csv, clear comma names


	 	  /* Open fixed format data and clear any open data. */

infile using data/comp01basic01, using(data/comp01data01.txt) clear


	/************************************************/
	/* Basic data summaries and tabulations. 		*/
	/* If we are in the right directory, we can 	*/
	/* open up the data set. 						*/
	/************************************************/

use comp01.dta, clear	 	  /* Open data and clear any open data. */

  describe
  summarize
  summarize, detail

tab female /* nominal variable */
tab pid3_p /* categorical variable */


  tabstat female race educ income natlec turnout pid3_p, statistics(mean sd min max N) ///
	columns(statistics)  /* tabstat displays table of summary statistics */

	/* Note use of triple forward slash "///" to allow command to break over lines. */


  tab1 state female race natlec  /* tab1 is short for tabulation, One-way tables of frequencies */

  tab1 female-educ		/* Illustrate varlist. A varlist is a list of variable names. */

  tab natlec female		/* cross-tabulation, tab dependent independent variables */
  tab natlec female, row
  tab natlec female, row nofreq


		/* Illustrate missing values behavior. */

  tabulate income, summarize(natlec) missing

  summarize natlec if income == 1  	/* == means equal */
  summarize natlec if income <  8	/* < means less than */
  summarize natlec if income >= 8	/* >= means greater than or equal to */
  summarize natlec if income >= 15	
  summarize natlec if income >= 15 & !missing(income) /* Missing values are infinity. */

		/* Label variables to see how it can help. 	*/

label data "Introductory Stata Workshop (Rury 2015)"

  label variable natlec		"Economic Conditions"
  label variable female		"Female"

  tab natlec female, row nofreq


		/* Label values to see how it can help. 	*/

  label define female 0 Male 1 Female

	label values female female

	note natlec: "Information about the variable coding."

describe /* to see results */

	 /* List all notes */
        . notes

  tab natlec female, row nofreq

  table female, c(mean natlec sd natlec)



	/* Create an indicator (aka dummy or binary) variable */

recode pid3_p (1=1 "Democrat") (2/3=0 "Not Democrat"), gen(democrat)
	tab democrat

	/* or much simpler */
tab pid3_p, gen(party_indicator)
	tab party_indicator1


	/************************************************/
	/* Now do some graphical summaries of the data. */
	/************************************************/


hist natlec  /* histogram */

twoway hist natlec		/* twoway is suite of graph commands. */

twoway hist natlec, scheme(s1color) ///
	title(Distribution of Retrospective National Economic Evaluations)

twoway hist natlec, scheme(s1color) by(female)

twoway hist natlec, scheme(s1color) by(female, ///
	  title(Distribution of Retrospective National Economic Evaluations))


	/****************************************/
	/* Now do some basic data analysis. 	*/
	/****************************************/

bysort female: tab natlec income, col nokey nofreq

tab natlec female, row nofreq  /* cross-tabulation */

tab pid3_p, sum(natlec)		/* mean comparison analysis */
tab female, sum(natlec)		/* mean comparison analysis */

corr natlec female			/* correlation */

anova natlec female 		/* Analysis of variance*/


regress natlec female		/* bivariate regression */
regress natlec female income	/* multivariate regression */
regress turnout natlec female income
logit turnout natlec female income


	/************************************************/
	/* Basic commands for labeling and saving data.	*/
	/************************************************/


	compress		/* Stores variables more efficiently. */

  save comp01basic01, replace
  saveold comp01basic01-version1, replace

  outsheet using comp01basic01.csv, comma replace
  outfile using comp01basic01, nolabel replace dictionary


	/************************************************/
	/* Accessing saved results and locals. 		*/
	/************************************************/


use comp01data01, clear	 	  /* Open data and clear any open data. */

  describe

  summarize natlec

	display r(mean)			/* Access stored results. */
	display r(sd)

	return list				/* Shows all stored results. */


		/* Accessing saved variables from regressions. */

  regress natlec income

	display e(N)
	display e(R2)
	display _b[income]
	display _b[_cons]
	display _se[income]

	ereturn list

	matrix list e(b)
	matrix list e(V)


	/******************/
	/* Using globals. */
	/******************/

		/* Define a global. Make to use the quotes. */

global indvars "income educ female natlec"

		/* Access global with preceding $ symbol. */

  summarize $indvars

  logit turnout $indvars

  logit turnout $indvars pid3_p
  
log close
clear
exit			/* if you use -exit, STATA-, it will close the Stata window. */

