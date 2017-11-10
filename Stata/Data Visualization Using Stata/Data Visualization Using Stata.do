set more off					/* Get rid of -MORE- in display.*/
  quietly log					/* Close log file if open. 	*/
  local logon = r(status)
  if "`logon'" == "on" {
	log close
	}
	
log using "Data Visualization Using Stata.log", text replace		/* Open new log file.	*/


/*	*********************************************************************/
/*     	File Name:	Data Visualization Using Stata.do					*/
/*     	Date:   	November 10, 2017									*/
/*      Author: 	Desmond D. Wallace									*/
/*      Purpose:	Exploring Graphing Capabilities in Stata.			*/
/*      Input File:	Data/DVdata01.dta,									*/
/*      			Data/DVdata02.dta,									*/
/*      Output File: Data Visualization Using Stata.log, 				*/
/*					 Graphs/DVgraph01.gph,								*/
/*					 Graphs/DVgraph01.pdf,								*/
/*					 Graphs/DVgraph01.svg								*/
/*	*********************************************************************/

		/*	Open the DVdata01 data file, and clear the memory.	*/
		
	use Data/DVdata01, clear /* Works if file is located in current working
								directory. */

	/************************************/
	/* Making Bar Graphs and Box Plots	*/
	/************************************/
	
		/* Bar graphs can be used to report a statistic about
			a variable, or multiple variables. */
			
	graph bar age /* Creates a single bar representing the variable's mean */
	
	graph save "Graphs\DVgraph01", replace /* Saves graph as a Stata-type
													file */
												
	graph export "Graphs\DVgraph01.pdf", as(pdf) replace /* Saves graph
															as pdf */
															
	graph export "Graphs\DVgraph01.png", as(png) replace /* Saves graph
															as png */
															
	graph export "Graphs\DVgraph01.svg", as(svg) replace /* Saves graph
															as svg */
	
	graph bar (mean) age /* Same as above. */
	
	graph bar (median) age /* Changes the statistic from mean to median. */
	
	graph bar (p25) age (mean) age ///
		(median) age (p75) age /* Featuring the 25th and 75th percentiles,
									along with the mean and median */
									
	graph bar age, over(female) /* Mean age for each gender category */
	
	graph bar age, over(female) asyvars  /* Treats the categories as
											different y variables */

	
	graph bar (p25) age (mean) age ///
		(median) age (p75) age, over(female) /* Featuring the 25th and 75th
												percentiles, along with the
												mean and median for each
												gender */
												

		/* Bar graphs can also be used to report the
			frequency breakdown of a variable. */
			
			/** The following two commands are used if the version
				of Stata is 13 or earlier. **/
			
	egen count = count(age) /* Creates a variable representing the
								number of observations */
	
	graph bar (count) count, over(pid3r) /* Each bar represents the number
											of observations fitting the
											specific category. */
											
	graph bar (count) count, over(pid3r) asyvars /* Treats the categories as
													different y variables */
													
	graph bar (count) count, over(pid3r) showyvars /* Places labels underneath
														the bars */
														
	graph bar (count) count, over(pid3r) asyvars showyvars /* Treats the 
																categories as
																different y 
																variables and
																places labels
																underneath the
																bars */
											
			/** The following command is used if the version
				of Stata is 14. **/ 
				
	graph bar (count), over(pid3r)
	
	graph bar (count), over(pid3r) asyvars
	
	graph bar (count), over(pid3r) showyvars
	
	graph bar (count), over(pid3r) asyvars showyvars
	
	
		/* Box Plots are useful for visualizing a variable's
			distribution based on the five number summary. */
			
	graph box age /* Creates a single box plot */
	
	graph box age, over(approval) /* Creates a box plot for each
										category of approval
										variable */
										

		/* Distribution plots are useful for
			visualizing a variable's distribution. */

	histogram age /* Plots a rectangle encompassing multiple
						x values against a density; sum of the
						rectangles' area equals 1 */
						
	histogram age, discrete /* Treats age as a discrete variable */
	
	histogram age, discrete fraction /* Changes the y-axis to represent
										fractions; sum of rectangles'
										height equals 1 */
	
	histogram age, discrete percent /* Same as above, except the sum of
										rectangles' height equals 100 */
	
	histogram age, discrete frequency /* Changes the y-axis to represent
											the number of observations in
											each category; sum of
											rectangles' height equals the
											number of total observations */
											
	kdensity age /* Creates a plot of each value of x1 against
					its kernel-density estimate */
	
			
	
	/************************/
	/* Making twoway Graphs	*/
	/************************/
	
		/* Open the comp03data02 data file, and clear the memory. */
		
	use Data/DVdata02, clear
	
		/* Scatterplots, Fit, and CI Fit plots display the relationship
			between two variables. */
			
	scatter y1 x1 /* Places y1 variable on y-axis and x1 variable
						on x-axis */
						
	graph save "Graphs\DVgraph02a", replace /* Saves graph as a Stata-type
													file */
												
	graph export "Graphs\DVgraph02a.pdf", as(pdf) replace /* Saves graph
															as pdf */
															
	graph export "Graphs\DVgraph02a.png", as(png) replace /* Saves graph
															as png */
															
	graph export "Graphs\DVgraph02a.svg", as(svg) replace /* Saves graph
															as svg */
						
	scatter y1 x1, mcolor(%30) /* Changes transparency of markers */
	
	graph save "Graphs\DVgraph02b", replace /* Saves graph as a Stata-type
													file */
												
	graph export "Graphs\DVgraph02b.pdf", as(pdf) replace /* Saves graph
															as pdf */
															
	graph export "Graphs\DVgraph02b.png", as(png) replace /* Saves graph
															as png */
															
	graph export "Graphs\DVgraph02b.svg", as(svg) replace /* Saves graph
															as svg */
							
	scatter y1 x1, by(z) /* Creates separate graphs for each value
							of the z variable */
								
	scatter y2 x2, mcolor(%30)
		
	scatter y2 x2, by(z) mcolor(%30)														
														
		/* Line plots are useful for visualizing a variable's
			frequency along a number line. */
			
	twoway line y1 id, sort /* Plots each value of y1 against a
								unique identification number */
								
	twoway connected y1 id, sort /* Same as above, except includes markers */
								
	twoway line y1 y2 id, sort /* Plots each value of y1 and y2 against a
									unique identification number */
									
	twoway connected y1 y2 id, sort /* Plots each value of y1 and y2 against a
										unique identification number */
										
		/* Distribution plots are useful for
			visualizing a variable's distribution. */
			
	twoway histogram y1 /* Plots a rectangle encompassing multiple
							x values against a density; sum of the
							rectangles' area equals 1 */
							
	twoway histogram y1, fraction /* Changes the y-axis to represent
										fractions; sum of rectangles'
										height equals 1 */
										
	twoway histogram y1, percent /* Same as above, except the sum of
										rectangles' height equals 100 */
										
	twoway histogram y1, frequency /* Changes the y-axis to represent
										the number of observations in
										each category; sum of
										rectangles' height equals the
										number of total observations */
										
		/* Typically, one creates a histogram with respect to
			continuous variables. But histograms can also be used
			to visualize the distribution of discrete variables. */
			
	twoway histogram z /* Bars are not exactly centered on correct values */
	
	twoway histogram z, discrete /* Bars are centered on correct values */
	
	twoway kdensity y1 /* Creates a plot of each value of x1 against
							its kernel-density estimate */
							
	twoway (histogram y1) (kdensity y1) /* Overlays kernel-density plot
											on a histogram */										

log close
clear
exit
