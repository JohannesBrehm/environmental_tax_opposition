/******************************************************************************* 	
*	Replication file: ISSP 2010												   *
********************************************************************************
*/


*** Standard settings go here
	clear all
	set more off

	
	*** Load data
	use "$datadir/ISSP2010/imputed_variables.dta", clear
	merge 1:1 CASEID using "$datadir/ISSP2010\ZA5500_v3-0-0.dta", keepusing(c_alphan country) gen(merge_check)
	drop if merge_check == 2 // countries not in 2020 ISSP sample
	drop merge_check
	
	*RELATIVE INCOME DECILE (required: ssc install egenmore)
	*ssc install egenmore
	
	foreach v in AT_INC AU_INC CH_INC DE_INC DK_INC ES_INC FI_INC FR_INC HR_INC IS_INC JP_INC KR_INC LT_INC NO_INC NZ_INC SE_INC SI_INC SK_INC TW_INC US_INC {
	egen `v'_decile=xtile(`v'), nq(10)
	}

	gen INC_decile = . 
	foreach v in AT AU CH DE DK ES FI FR HR IS JP KR LT NO NZ SE SI SK TW US{
	replace INC_decile = `v'_INC_decile if c_alphan == "`v'"
	}
	la var INC_decile "Country specific household income decile"
	label define INC_deciles_lb 10 "Highest decile" 0 "Lowest decile"
	la value INC_decile INC_deciles_lb
	
		*Clean 
		drop *_INC
		drop *_INC_decile
		
	*drop variables not needed 
	drop *_RINC
		
	save "$datadir/ISSP2010/imputed_variables_wIncome_2010.dta", replace