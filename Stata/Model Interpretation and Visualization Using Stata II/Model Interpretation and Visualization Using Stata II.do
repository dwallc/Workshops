set more off					/* Get rid of '-MORE-' in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Model Interpretation and Visualization Using Stata II.log", ///
	text replace		/* Open new log file.	*/


/*	*****************************************************************************/
/*	File Name:	Model Interpretation and Visualization Using Stata II.do		*/
/*	Date:   	March 9, 2018													*/
/*	Author: 	Desmond D. Wallace												*/
/*	Purpose:	Interpreting and Visualizing Regression							*/
/*					Model Results in Stata.										*/
/*	Input Files:	Data\MIVdata01.dta											*/
/*	Output Files:	Model Interpretation and Visualization Using Stata I.log	*/
/*	*****************************************************************************/


	/****************************************/
	/* Part I - Logarithm Transformations	*/
	/****************************************/

	/*	Open the MIVdata01 data file, and clear the memory.	*/
	
use Data\MIVdata01, clear

	/*	Estimate regression model predicting a respondent's income	*/
	/*	based on their age and gender.								*/
	
reg realrinc age

	/*	Calculate marginal effects and elasticities from level-level model	*/
	
		/*	Marginal Effect - dy/dx	*/
		
	margins, dydx(age) /*	Interpretation: A 1-year increase in age yields	*/
					   /*	a $255.8004 increase in income.					*/
	
		/*	Semi-Elasticity - dy/ex	*/
		
	margins, dyex(age) /*	Interpretation: A 1-unit increase in ln(age)	*/
					   /*	yields a $11149.15 increase in income.			*/
					   
		/*	Semi-Elasticity - ey/dx	*/
		
	margins, dyex(age) /*	Interpretation: A 1-unit increase in age yields	*/
					   /*	a 0.0127487 unit increase in ln(income).		*/
					   
		/*	Elasticity - ey/ex	*/
		
	margins, dyex(age) /*	Interpretation: A proportional increase in age	*/
					   /*	yields a 52.36568 percent increase in income.	*/
					   
	/*	Calculate marginal effects from level-log model	*/
	
		/*	Generate log transformed version of 'age' variable	*/
		
	gen age_ln = ln(age)
	
		/*	Estimate level-log model	*/
		
	reg realrinc age_ln
	
		/*	Marginal Effect - dy/dx	*/
		
	margins, dydx(age_ln) /*	Interpretation: A 1-unit increase in	*/
						  /*	ln(age) yields a $13137.79 increase		*/
						  /*	in income.								*/
						  
	/*	Calculate marginal effects from log-level model	*/
	
		/*	Generate log transformed version of 'realrinc' variable	*/
		
	gen realrinc_ln = ln(realrinc)
	
		/*	Estimate level-log model	*/
		
	reg realrinc_ln age
	
		/*	Marginal Effect - dy/dx	*/
		
	margins, dydx(age) /*	Interpretation: A 1-year increase in age	*/
					   /*	yields a 0.0156458 unit increase in			*/
					   /*	ln(income).									*/

	/********************************/
	/* Part II - Regression Plots	*/
	/********************************/
	
	/*	Install coefplot, if necessary	*/

*search coefplot /*	Click on the fifth link, then click on the	*/
				 /*	blue "Click here to install" link.			*/

	/*	Use coefplot to create a regression plot	*/
	
coefplot, title("Model Results") /*	Basic plot	*/

coefplot, title("Model Results") xline(0) /*	Vertical line to help	*/
										  /*	identify whether a		*/
										  /*	coefficient estimate is	*/
										  /*	statistically			*/
										  /*	significant.			*/
										  
	graph export Graphs/MIVcoef01.png, as(png) replace
				   
coefplot, title("Model Results") xline(0) drop(_cons) /*	Remove constant	*/

	graph export Graphs/MIVcoef02.png, as(png) replace


	/****************************************/
	/* Part III - Predicted (Fitted) Values	*/
	/****************************************/
				
	/*	Estimate a regression model predicting a respondent's	*/
	/*	incoe based on their age and gender. 					*/
			
regress realrinc age i.female

	/*	Plot regression lines	*/
	
predict yhat

graph twoway line yhat age if female==0, sort || ///
	line yhat age if female==1, sort ///
	xtitle(Age) ytitle(E(Income | Age)) ///
	legend(label(1 Male) label(2 Female)) /*	Create graph of predicted	*/
										  /*	means plotted against		*/
										  /*	age for males and			*/
										  /*	females.					*/
			
	graph export Graphs/mfxOLS.png, as(png) replace /*	Save graph as	*/
													/*	.png file.		*/
	
	/*	Calculate predicted means via the margins command and	*/
	/*	graph results via the marginsplot command. 				*/
			
margins /*	Overall predicted mean with independent variables	*/
		/*	held to their mean value. 							*/
					
marginsplot /*	Graphs the result from the previously executed	*/
			/*	margins command. NOTE: marginsplot must be		*/
			/*	executed IMMEDIATELY after margins command,		*/
			/*	or you will receive an error.					*/
			
	/*	Specific Values - Continuous Variable	*/
						
	margins, at(age=18) /*	Predicted mean when age = 18, and	*/
						/*	remaining independent variables		*/
						/*	held to their mean values.			*/
								
	marginsplot
		
	margins, at(age=(33 47 61)) atmeans /*	Calculates predicted means at	*/
										/*	the 25th, 50th, and 75th		*/
										/*	percentiles of the 'age'		*/
										/*	variable, and explicitly		*/
										/*	setting the remaining			*/
										/*	independent variables at		*/
										/*	their mean value. 				*/
												
	marginsplot
		
	marginsplot, recast(line) recastci(rarea) ///
		plotopts(color(black)) ///
		ciopts(color(gs12))	/*	Changes the plot options so that	*/
							/*	the predicted means are plotted		*/
							/*	as a line, and the confidence		*/
							/*	intervals are plotted as a shaded	*/
							/*	area. 								*/
									
	margins, at(age=(18(1)89)) ///
		atmeans noatlegend /*	Calculating predicted means for each	*/
						   /*	value of 'age' while holding the		*/
						   /*	remaining variables at their mean 		*/
						   /*	value. In addition, I am not			*/
						   /*	producing the _at legend to				*/
						   /*	preserve space. 						*/					
									
	marginsplot, recast(line) recastci(rarea) ///
		plotopts(color(black)) ///
		ciopts(color(gs12))
			
	/*	Specific Values - Discrete Variables	*/
			
	margins female, atmeans /*	Calculating the overall expected mean for	*/
							/*	males and females, holding remaining		*/
							/*	variables constant. 						*/
		
									
	marginsplot
		
	marginsplot, recast(bar) ///
		plotopts(color(gs12)) ///
		ciopts(color(black)) /*	Changes the plot options so that the	*/
							 /*	predicted means are plotted as bars. 	*/
									
	/*	Specific Values - Continuous and Discrete Variables	*/
			
	margins, at((mean) age female=1) /*	Predicted mean for average	*/
									 /*	aged female.				*/
													
	marginsplot
		
	margins female, at(age=(18(1)89)) ///
		atmeans noatlegend /*	Predicted means for males and females for	*/
						   /*		each age. 								*/
		
	marginsplot, recast(line) recastci(rarea) ///
		ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
		
	/********************************/
	/* Part IV - Marginal Effects	*/
	/********************************/
			
	/*	Marginal Change	*/
			
	margins, dydx(age) /*	Average marginal effect a one-unit increase	*/
					   /*	in age has on income, holding all other		*/
					   /*	variables at their mean value. Notice that	*/
					   /*	both the marginal effect and the standard	*/
					   /*	error are the same as the coefficient		*/
					   /*	estimate and standard error from the		*/
					   /*	regression output. 							*/	
								
	marginsplot
		
	margins female, dydx(age) /* Same as previous example. */
		
	marginsplot, recast(bar) ///
		plotopts(color(gs12)) ///
		ciopts(color(black))
			
	/*	Discrete Change	*/
			
	margins, dydx(female) atmeans /*	Discrete change represents the	*/
								  /*	difference in predicted means	*/
								  /*	for males and females, holding	*/
								  /*	remaining variables at their	*/
								  /*	mean values. 					*/
											
	marginsplot
		
	margins, dydx(female) at(age=(18(1)89)) ///
		atmeans noatlegend /*	Despite varying the highest year of	*/
						   /*	education completed, the effect		*/
						   /*	gender has on income does not 		*/
						   /*	change.								*/
			
	marginsplot, recast(line) recastci(rarea) ///
		plotopts(color(black)) ///
		ciopts(color(gs12))
		
log close
clear
exit														
