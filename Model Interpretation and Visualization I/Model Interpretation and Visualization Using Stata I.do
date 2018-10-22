clear all	/*	Clears everything from memory, close any open files, close any open graph windows, etc.	*/
cls	/*	Clear Results window	*/
set more off	/* Get rid of '-MORE-' in display.*/
quietly log		/* Reports whether log file is open or closed. 	*/
local logon = r(status) /*	Create a local macro saving the staus of log file.	*/
if "`logon'" == "on" {
	log close	/*	If a log file is open, close it.	*/
}
log using "Model Interpretation and Visualization Using Stata I.log", ///
	text replace		/* Open new log file as a text file.	*/
	
	
/*	*****************************************************************************/
/*	File Name:	Model Interpretation and Visualization Using Stata I.do			*/
/*	Date:   	October 22, 2018												*/
/*	Author: 	Scott J. LaCombe												*/
/*	Purpose:	Create tables and plots to report regression results, and		*/
/*					introduce margins and marginsplot commands.					*/
/*	Input Files:	Data\MIVdata.dta											*/
/*	Output Files:	Model Interpretation and Visualization Using Stata I.log,	*/
/*					Tables\MIVmodel01.txt,										*/
/*					Tables\MIVmodel01.tex,										*/
/*					Graphs\MIVcoefplot01.png,									*/
/*					Graphs\MIVcoefplot01b.png									*/
/*	*****************************************************************************/


	/****************************/
	/* Load data into memory.	*/
	/****************************/
	
use Data\MIVdata, clear

/*	This dataset is a dataset used from the book
	Interpreting and Visualizing Regression Models.
	This dataset features responses from the General
	Social Survey covering the period 1972 - 2010.	*/
	
	
	/********************************/
	/* Estimate regression models.	*/
	/********************************/
	
	/*	Estimate an OLS regression models predicting a respondent's	*/
	/*	income based on their age and gender.						*/
		
reg realrinc age i.female

	estimates store ols1
	
	/*	Estimate probit and logit regression models predicting	*/
	/*	whether a respondent is willing to vote for a female	*/
	/*	president based on their number of children and whether	*/
	/*	respondent graduated from high school.					*/
	
probit fepres children i.hsgrad

	estimates store probit1
	
logit fepres children i.hsgrad

	estimates store logit1
	
	
	/********************************/
	/* Part I - Regression Tables	*/
	/********************************/
	
	/*	Install estout, if necessary	*/
		
ssc install estout, replace

	/*	Use estout to create tables	*/
		
	capture mkdir Tables	/*	Create a folder called "Tables" if it does not exist.	*/
	
estout ols1 probit1 logit1, cells(b(star fmt(3)) se(par)) eqlabels(none) ///
	stats(N aic bic, fmt(0 2) labels(N AIC BIC)) starlevels(* 0.05) ///
	varlabel(age "Age" 1.female "Gender" _cons Constant ///
	children "Number of Children" 1.hsgrad "High School Grad.") ///
	drop(0.female 0.hsgrad) modelwidth(16) varwidth(45) ///
	legend collabels(, none) mlabels(OLS Probit Logit)
		
estout ols1 probit1 logit1 using Tables\MIVtable01.txt, ///
	replace cells(b(star fmt(3)) se(par)) eqlabels(none) ///
	stats(N aic bic, fmt(0 2) labels(N AIC BIC)) ///
	varlabel(age "Age" 1.female "Gender" _cons Constant ///
	children "Number of Children" 1.hsgrad "High School Grad.") ///
	drop(0.female 0.hsgrad) modelwidth(16) starlevels(* 0.05) ///
	legend varwidth(45) collabels(, none) mlabels(OLS Probit Logit)
	
estout ols1 probit1 logit1 using Tables\MIVtable01.tex, ///
	replace style(tex) cells(b(star fmt(3)) se(par)) ///
	stats(N aic bic, fmt(0 2) labels(N AIC BIC)) eqlabels(none) ///
	varlabel(age "Age" 1.female "Gender" _cons Constant ///
	children "Number of Children" 1.hsgrad "High School Grad.") ///
	drop(0.female 0.hsgrad) modelwidth(16) starlevels(* 0.05) ///
	legend varwidth(45) collabels(, none) mlabels(OLS Probit Logit)
	
/*	For more information on estout: http://repec.sowi.unibe.ch/stata/estout/	*/
	

	/********************************/
	/* Part II - Coefficient Plots	*/
	/********************************/
	
	/*	Install coefplot, if necessary	*/

ssc install coefplot, replace

	/*	Use coefplot to create regression plots	*/
	
	capture mkdir Graphs	/*	Create a folder called "Graphs" if it does not exist.	*/
	
		/*	OLS Models	*/
		
coefplot ols1, title("OLS Model Results") xline(0) ///
	rename(age = "Age" 1.female = "Gender" _cons = "Constant") ///
	xtitle("Coefficient Estimate")
											  
	graph export Graphs/MIVcoefplot01.png, as(png) replace
				   
coefplot ols1, title("OLS Model Results") xline(0) drop(_cons) ///
	rename(age = "Age" 1.female = "Gender")  ///
	xtitle("Coefficient Estimate")	/*	Remove constant	*/

	graph export Graphs/MIVcoefplot01b.png, as(png) replace
	
		/*	Binary Regression (BRM) Models	*/
		
			/*	Probit and Logit Results on Same Plot	*/
		
coefplot (probit1, label(Probit)) (logit1, label(Logit)), ///
	title("Probit and Logit Model Results") xline(0) drop(_cons)  ///
	rename(children = "Number of Children" 1.hsgrad = "High School Graduate") ///
	xtitle("Coefficient Estimate")	/*	Includes probit and logit models on same graph.	*/
	
	graph export Graphs/MIVcoefplot02.png, as(png) replace
	
			/*	Probit and Logit Results Side-by-Side	*/
	
coefplot probit1, bylabel(Probit) || logit1, bylabel(Logit) ||, xline(0) drop(_cons) ///
	rename(children = "Number of Children" 1.hsgrad = "High School Graduate") ///
	xtitle("Coefficient Estimate")
	
	graph export Graphs/MIVcoefplot02b.png, as(png) replace
	
/*	For more information on coefplot: http://repec.sowi.unibe.ch/stata/coefplot/	*/


	/****************************************/
	/* Part III - Predicted (Fitted) Values	*/
	/****************************************/
	
/*	margins - Marginal means, predictive margins, and marginal effects	*/
/*	marginsplot - Graph results from margins (profile plots, etc.)	*/

	/*	Part A - OLS Model	*/
	
	estimates restore ols1
	
margins /*	Overall predicted mean with independent variables	*/
		/*	held to their mean value. 							*/
		
marginsplot /*	Graphs the result from the previously executed	*/
			/*	margins command. NOTE: marginsplot must be		*/
			/*	executed IMMEDIATELY after margins command,		*/
			/*	or you will receive an error.					*/

		/*	Specific Values - Continuous Variable	*/

margins, at(age=18) atmeans /*	Predicted mean when age = 18, and	*/
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
	atmeans noatlegend	/*	Calculating predicted means for each	*/
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
		
marginsplot, recast(dot) ///
	plotopts(color(gs12)) ///
	ciopts(color(black))	/*	Changes the plot options so that the	*/
							/*	predicted means are plotted as bars. 	*/

		/*	Specific Values - Continuous and Discrete Variables	*/
			
margins, at((mean) age female=1)	/*	Predicted mean for average	*/
									/*	aged female.				*/

marginsplot
		
margins female, at(age=(18(1)89)) ///
	atmeans noatlegend	/*	Predicted means for males and females for	*/
						/*		each age. 								*/

marginsplot, recast(line) recastci(rarea) ///
	ci1opts(color(ltblue)) ///
	ci2opts(color(orange))
		
	/*	Part B - BRM Models	*/
	
	estimates restore logit1
		
margins

marginsplot

		/*	Specific Values - Continuous Variable	*/

margins, at(children=(0(1)8))

marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
	ciopts(color(gs12))
	
margins, at(children=(0(1)8)) atmeans

marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
	ciopts(color(gs12))

		/*	Specific Values - Discrete Variables	*/

margins hsgrad

marginsplot, recast(dot) plotopts(color(gs12)) ///
	ciopts(color(black))
	
margins hsgrad, atmeans

marginsplot, recast(dot) plotopts(color(gs12)) ///
	ciopts(color(black))
	
		/*	Specific Values - Continuous and Discrete Variables	*/

margins hsgrad, at(children=(0(1)8))

marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
	ci2opts(color(orange))
	

	/********************************/
	/* Part IV - Marginal Effects	*/
	/********************************/
			
	/*	Part A - OLS Model	*/
		
	estimates restore ols1
	
		/*	Marginal Change	*/
			
margins, dydx(age)	/*	Average marginal effect a one-unit increase	*/
					/*	in age has on income, holding all other		*/
					/*	variables at their mean value. Notice that	*/
					/*	both the marginal effect and the standard	*/
					/*	error are the same as the coefficient		*/
					/*	estimate and standard error from the		*/
					/*	regression output. 							*/	
								
marginsplot
		
margins female, dydx(age) /* Same as previous example. */
		
marginsplot, recast(dot) ///
	plotopts(color(gs12)) ///
	ciopts(color(black))
			
		/*	Discrete Change	*/
			
margins, dydx(female) atmeans	/*	Discrete change represents the	*/
								/*	difference in predicted means	*/
								/*	for males and females, holding	*/
								/*	remaining variables at their	*/
								/*	mean values. 					*/
											
marginsplot

margins, dydx(female) atmeans continuous	/* Treat female as a continuous variable.	*/

marginsplot
		
margins, dydx(female) at(age=(18(1)89)) ///
	atmeans noatlegend	/*	Despite varying the highest year of	*/
						/*	education completed, the effect		*/
						/*	gender has on income does not 		*/
						/*	change.								*/
			
marginsplot, recast(line) recastci(rarea) ///
	plotopts(color(black)) ///
	ciopts(color(gs12))
	
margins, dydx(female) at(age=(18(1)89)) ///
	atmeans noatlegend continuous
	
marginsplot, recast(line) recastci(rarea) ///
	plotopts(color(black)) ///
	ciopts(color(gs12))
	
	/*	Part B - BRM Models	*/
		
	estimates restore logit1
	
		/*	Average Marginal Change	*/
			
margins, dydx(children)

marginsplot

		/*	Average Conditional Marginal Change	*/
		
margins hsgrad, dydx(children)

marginsplot, recast(dot) plotopts(color(gs12)) ///
	ciopts(color(black))
	
margins, dydx(hsgrad) continuous

marginsplot
			
		/*	Conditional Marginal Change	*/
			
margins hsgrad, dydx(children) atmeans

marginsplot, recast(dot) plotopts(color(gs12)) ///
	ciopts(color(black))
			
margins hsgrad, dydx(children) at(children=(0(1)8))

marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
	ci2opts(color(orange))
	
margins, dydx(hsgrad) atmeans continuous

marginsplot

margins, dydx(hsgrad) at(children=(0(1)8)) continuous

marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
	ciopts(color(gs12))

		/*	Average Discrete Change	*/
		
margins, dydx(hsgrad)

marginsplot

		/*	Conditional Discrete Change	*/
		
margins, dydx(hsgrad) atmeans

marginsplot

margins, dydx(hsgrad) at(children=(0(1)8))

marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
	ciopts(color(gs12))
			
margins hsgrad, at(children=(0 1)) post

	nlcom (_b[1._at#0.hsgrad] - _b[2._at#0.hsgrad]) ///
		(_b[1._at#1.hsgrad] - _b[2._at#1.hsgrad])
	
	estimates restore logit1
	
margins hsgrad, at(children=(7 8)) post

	nlcom (_b[1._at#0.hsgrad] - _b[2._at#0.hsgrad]) ///
		(_b[1._at#1.hsgrad] - _b[2._at#1.hsgrad])
		
		
	/************/
	/* Cleanup	*/
	/************/
	
log close
