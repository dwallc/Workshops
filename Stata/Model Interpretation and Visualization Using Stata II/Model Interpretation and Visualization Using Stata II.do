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
/*	Purpose:	Interpreting and visualizing regression							*/
/*					model results in Stata for									*/
/*					non-linear relationships.									*/
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

	/*	Calculate marginal effects and elasticities	*/
	/*	from level-level model								*/
	
		/*	Marginal Effect - dy/dx	*/
		
			/*	Average	*/
		
	margins, dydx(age) /*	Interpretation: A 1-year increase in age yields	*/
					   /*	a $255.8004 increase in income.					*/
					   
			/*	Conditional	*/
			
	margins, dydx(age) at(age=(18(5)89)) noatlegend
	
		marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
		ciopts(color(gs12))
	
		/*	Semi-Elasticity - dy/ex	*/
		
			/*	Average	*/
		
	margins, dyex(age) /*	Interpretation: A 1-unit increase in ln(age)	*/
					   /*	yields a $11149.15 increase in income.			*/
					   
			/*	Conditional	*/
			
	margins, dyex(age) at(age=(18(5)89)) noatlegend
	
		marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
		ciopts(color(gs12))
					   
		/*	Semi-Elasticity - ey/dx	*/
		
			/*	Average	*/
		
	margins, eydx(age) /*	Interpretation: A 1-year increase in age yields	*/
					   /*	a 0.0127487 unit increase in ln(income).		*/
					   
			/*	Conditional	*/
			
	margins, eydx(age) at(age=(18(5)89)) noatlegend
	
		marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
		ciopts(color(gs12))
					   
		/*	Elasticity - ey/ex	*/
		
			/*	Average	*/
		
	margins, eyex(age) /*	Interpretation: A 1-nit increase in ln(age)	*/
					   /*	yields a 0.5236568 increase in ln(income).	*/
					   
			/*	Conditional	*/
			
	margins, eyex(age) at(age=(18(5)89)) noatlegend
	
		marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
		ciopts(color(gs12))
	
	/*	Note that semi-elasticities and elasticities are a function of x.	*/
	/*	In other words, resulting values vary based on specified values.	*/
	
	/*	Calculate marginal effects from level-log model	*/
	
		/*	Generate log transformed version of 'age' variable	*/
		
	gen age_ln = ln(age)
	
		/*	Estimate level-log model	*/
		
	reg realrinc age_ln
	
		/*	Average Marginal Effect - dy/dx	*/
		
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
					   
	/*	Calculate marginal effects from log-log model	*/
	
		/*	Estimate log-log model	*/
		
	reg realrinc_ln age_ln
	
		/*	Marginal Effect - dy/dx	*/
		
	margins, dydx(age_ln) /*	Interpretation: A 1-unit increase in	*/
						  /*	ln(age) yields a 0.8771319 unit 		*/
						  /*	increase in ln(income).					*/

	/*	NOTE: Average Marginal effects (dy/dx) calculated from linear-log,	*/
	/*	log-linear, and log-log models do equal the average					*/
	/*	semi-elasticities  and elasticities calculated from 				*/
	/*	the linear-linear model.											*/
	
	
	/********************************/
	/* Part II - Quadratic Terms	*/
	/********************************/
	
	/*	Assume that there is a curvilinear relationship	*/
	/*	between independent variable (age) and			*/
	/*	dependent variable (income). To account for		*/
	/*	such a relationship, one can include a squared	*/
	/*	term of the 'age' variable.						*/

	/*	Estimate regression model regressing income on gender, parental	*/
	/*	education, age, and age squared.								*/
	
reg realrinc paeduc maeduc i.female c.age##c.age

	/*	Calculate predicted values for varying values of paternal	*/
	/*	education, for both males and females while holding age		*/
	/*	at its mean value.											*/
	
margins female, at(paeduc=(0(1)20)) atmeans

	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
		
	/*	Re-do above sequence, but do not utilize factor notation for	*/
	/*	quadratic term.													*/
	
		/*	Generate squared age variable	*/
		
	gen age_sq = age^2
	
reg realrinc paeduc maeduc i.female age age_sq

margins female, at(paeduc=(0(1)20)) atmeans

	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
		
	/*	Calculate predicted values for varying ages	*/
	
		/*	Factor Notation Version	*/
		
	reg realrinc paeduc maeduc i.female c.age##c.age
	
margins female, at(age=(18(1)30)) atmeans

	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
		
		/*	Generated Variable Version	*/
		
	reg realrinc paeduc maeduc i.female age age_sq
	
margins female, at(age=(18(1)30)) atmeans

	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
		
	/*	NOTE: While the coefficient estimates will be the same, the fitted	*/
	/*	values generated from each model will differ. This is due to the	*/
	/*	interaction term in the factor notation model is held at			*/
	/*	mean(age)*mean(age) while the interaction term in the generated		*/
	/*	variable version is held at mean(age_sq). 							*/
	/*	mean(age)*mean(age) != mean(age_sq)									*/


	/****************************************/
	/* Part III - Interactions	*/
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
