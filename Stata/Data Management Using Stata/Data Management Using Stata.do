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


	/********************************/
	/* Basic data manipulation. 	*/
	/********************************/
	
	/*	Sorting Data via sort Command	*/
	
sort continent
sort year country

	/*	Sorting Data via gsort Command	*/
	
gsort +country
gsort +country -year
gsort -pop /*	Which countries have the largest populations?	*/
gsort -year -pop /*	Countries with largest populations in 2007?	*/
gsort -year -lifeexp /*	Countries with largest life expectancies in 2007?	*/

	/*	Subsetting Data via drop/keep Commands	*/
	
	preserve /*	Preserving the data in its current state	*/
keep if year==2007
gsort -pop /*	Countries with largest populations in 2007?	*/
list country pop in 1/10 /*	Display the values of variables	*/
	restore /*	Restores data to previous state	*/
	
	preserve
keep if year==2007
drop pop gdppercap
gsort -lifeexp /*	Countries with largest life expectancies in 2007?	*/
list country lifeexp in 1/10
	restore
	
	/*	Creating Variables via gen/replace/egen Commands	*/
	
gen gdp = pop * gdppercap
	*gen gdp = . /*	Creates variable of missing values	*/
	*replace gdp = pop * gdppercap /*	Replacing missing values with	*/
								   /*	product of gdp and gdppercap.	*/
egen avggdp = mean(gdp) /*	Average GDP	*/
egen avggdp_year = mean(gdp), by(year) /*	Average GDP for each year	*/

	/*	Summarizing Data via collapse Command	*/
	
	preserve
collapse (mean) gdp /*	Average GDP	*/
list
	restore
	
	preserve
collapse (mean) gdp, by(year) /*	Average GDP for each year	*/
list
	restore
	

log close
clear
exit			/* if you use -exit, STATA-, it will close the Stata window. */

