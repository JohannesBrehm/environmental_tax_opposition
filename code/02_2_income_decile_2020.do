/******************************************************************************* 	
*	Replication file: ISSP 2020												   *
********************************************************************************
*/


*** Standard settings go here
	clear all
	set more off

	
	*** Load data
	use "$datadir/ISSP2020/imputed_variables.dta", clear
	
	merge 1:1 CASEID using "$datadir/ISSP2020/ZA7650_v2-0-0.dta", keepusing(c_alphan) gen(merge_check)
	drop merge_check
	
	*RELATIVE INCOME DECILE (required: ssc install egenmore)
	
	replace IT_INC = IT_RINC if IT_INC == -4 // for Italy, respondents got -4 if they live alone
	
	foreach v in AT_RINC AU_RINC CH_RINC CN_RINC DE_RINC DK_RINC ES_RINC FI_RINC FR_RINC HR_RINC HU_RINC IN_RINC IS_RINC IT_RINC JP_RINC KR_RINC LT_RINC NO_RINC NZ_RINC PH_RINC RU_RINC SE_RINC SI_RINC SK_RINC TH_RINC TW_RINC US_RINC ZA_RINC {
	egen `v'_deciles=xtile(`v'), nq(10)
	}

	gen RINC_deciles = . 
	foreach v in AU CH CN DE DK ES FI FR HR HU IN IS IT JP KR LT NO NZ PH RU SE SI SK TH TW US ZA {
	replace RINC_deciles = `v'_RINC_deciles if c_alphan == "`v'"
	}
	la var RINC_deciles "Country specific personal income decile"
	label define RINC_deciles_lb 10 "Highest decile" 0 "Lowest decile"
	la value RINC_deciles RINC_deciles_lb
	
	
	foreach v in AT_INC AU_INC CH_INC CN_INC DE_INC DK_INC ES_INC FI_INC FR_INC HR_INC HU_INC IN_INC IS_INC IT_INC JP_INC KR_INC LT_INC NO_INC NZ_INC PH_INC RU_INC SE_INC SI_INC SK_INC TH_INC TW_INC US_INC ZA_INC {
	egen `v'_deciles=xtile(`v'), nq(10)
	}

	gen INC_deciles = . 
	foreach v in AU CH CN DE DK ES FI FR HR HU IN IS IT JP KR LT NO NZ PH RU SE SI SK TH TW US ZA {
	replace INC_deciles = `v'_INC_deciles if c_alphan == "`v'"
	}
	la var INC_deciles "Country specific household income decile"
	label define INC_deciles_lb 10 "Highest decile" 0 "Lowest decile"
	la value INC_deciles INC_deciles_lb
	
		*Clean 
		drop *_RINC
		drop *_INC
		drop *_RINC_deciles
		drop *_INC_deciles
		
	save "$datadir/ISSP2020/imputed_variables_wIncome.dta", replace