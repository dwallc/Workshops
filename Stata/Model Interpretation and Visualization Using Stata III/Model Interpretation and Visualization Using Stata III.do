set more off					/* Get rid of '-MORE-' in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
log using "Model Interpretation and Visualization Using Stata III.log", ///
	text replace		/* Open new log file.	*/


/*	*****************************************************************************/
/*	File Name:	Model Interpretation and Visualization Using Stata III.do		*/
/*	Date:   	March 9, 2018													*/
/*	Author: 	Desmond D. Wallace												*/
/*	Purpose:	Interpreting and visualizing regression							*/
/*					model results in Stata for									*/
/*					non-linear models.											*/
/*	Input Files:	Data\MIVdata02.dta											*/
/*	Output Files:	Model Interpretation and Visualization Using Stata III.log	*/
/*	*****************************************************************************/


	/********************************/
	/* Part I - Logit/Probit Models	*/
	/********************************/

	/*	Open the MIVdata02 data file, and clear the memory.	*/
		
use Data\MIVdata02, clear

	/*	Estimate a binary regression model (BRM) predicting	*/
	/*	support of a female president.						*/
				
probit fepres children i.hsgrad
	
logit fepres children i.hsgrad

	estimates store results
	
	/*	Calculate predicted probabilities.	*/
			
margins
		
margins, at(children=(0(1)8))

	marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
			ciopts(color(gs12))
					
margins hsgrad, at(children=(0(1)8)) atmeans
											 
	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
													
margins hsgrad, atmeans
									
	marginsplot, recast(bar) plotopts(color(gs12)) ///
			ciopts(color(black))
			
	/*	Calculate marginal changes.	*/
			
		/*	Average Marginal Effects	*/
			
margins, dydx(children)

		/*	Average Conditional Marginal Effects	*/
		
margins hsgrad, dydx(children)
										
	marginsplot, recast(bar) plotopts(color(gs12)) ///
			ciopts(color(black))
			
		/*	Conditional Marginal Effects	*/
			
margins hsgrad, dydx(children) atmeans

	marginsplot, recast(bar) plotopts(color(gs12)) ///
			ciopts(color(black))
			
margins hsgrad, dydx(children) at(children=(0(1)8))

	marginsplot, recast(line) recastci(rarea) ci1opts(color(ltblue)) ///
		ci2opts(color(orange))
						
	/*	Calculate discrete changes.	*/
	
		/*	Average Discrete Effects	*/
		
margins, dydx(hsgrad)

		/*	Conditional Marginal Effects	*/
		
margins, dydx(hsgrad) atmeans

margins, dydx(hsgrad) at(children=(0(1)8))

	marginsplot, recast(line) recastci(rarea) plotopts(color(black)) ///
			ciopts(color(gs12))
			
margins hsgrad, at(children=(0 1)) post

	nlcom (_b[1._at#0.hsgrad] - _b[2._at#0.hsgrad]) ///
		(_b[1._at#1.hsgrad] - _b[2._at#1.hsgrad])
	
	estimates restore results
	
margins hsgrad, at(children=(7 8)) post

	nlcom (_b[1._at#0.hsgrad] - _b[2._at#0.hsgrad]) ///
		(_b[1._at#1.hsgrad] - _b[2._at#1.hsgrad])
			

log close
clear
exit
