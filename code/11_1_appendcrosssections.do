/******************************************************************************* 	
*	Replication file: 													   *
********************************************************************************
*/



*** Standard settings go here
	clear all
	set more off


					
**********************************************
*  LOAD DATA
**********************************************

clear all
*** Load data
	use "$datadir/ISSP2020/ISSP2020_clean_wcountry.dta", clear 
	
	append using "$datadir/ISSP2010/ISSP2010_clean_wcountry.dta"
	
	
	save "$datadir/ISSP2020/ISSPbothwaves.dta", replace  