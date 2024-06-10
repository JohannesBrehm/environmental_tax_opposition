/******************************************************************************* 	
*	Replication file: 												   *
********************************************************************************
*/

********************************************************************************
*	PART 1:  Set paths						                                   *
********************************************************************************

*** Standard settings go here
	clear all
	set more off

		
			
********************************************************************************
*	MERGE DATA															   	   *
********************************************************************************	
	
	//Load data
	use "$datadir/ISSP2010\ZA5500_v3-0-0.dta", clear
	
	//Keep only high-income countries that are in the 2020 survey 
	keep if c_alphan == "AT" | c_alphan == "AU" | c_alphan == "CH" | c_alphan == "DE" | c_alphan == "DK" | c_alphan == "ES" | c_alphan == "FI" | c_alphan == "FR" | c_alphan == "HR" | c_alphan == "IS" | c_alphan == "JP" | c_alphan == "KR" | c_alphan == "LT" | c_alphan == "NO" | c_alphan == "NZ" | c_alphan == "SE" | c_alphan == "SI" | c_alphan == "SK" | c_alphan == "TW" | c_alphan == "US" 


	
	//Clean data 
	**Code as missing if a variable is missing for a whole country, then impute the values over all countries in a second step 
	replace TOPBOT=. if c_alphan == "NZ"
	replace PARTY_LR =. if c_alphan == "IL"|c_alphan == "TW"
	replace HHCHILDR=. if c_alphan == "JP"
	replace HHTODD=. if c_alphan == "JP"|c_alphan == "DK"|c_alphan == "NZ"
	

	//Replace "Don't know" with middle value for polarized variables, "no answer" as NA
	recode v5 (99=.) (98=.)
	recode v6 (99=.) (98=.)
	recode v11 (8=3) (9=.)
	recode v13 (8=3) (9=.)
	recode v15 (8=3) (9=.)
	recode v16 (98=.) (99=.)
	recode v22 (8=3) (9=.)
	recode v23 (8=3) (9=.)
	recode v24 (8=3) (9=.)
	recode v27 (8=3) (9=.)
	recode v33 (8=3) (9=.)
	recode v35 (8=3) (9=.)
	recode v36 (8=3) (9=.)
	recode v37 (8=3) (9=.)
	recode v38 (8=3) (9=.)
	recode v39 (8=3) (9=.)
	recode v40 (8=3) (9=.)
	recode v41 (8=3) (9=.)
	recode v42 (8=3) (9=.)
	recode v43 (8=3) (9=.)
	recode v44 (8=3) (9=.)
	recode v45 (8=3) (9=.)
	recode v60 (8=3) (9=.)
	recode v62 (8=.) (9=.)
	recode v63 (8=.) (9=.)
	
	recode PARTY_LR (97=.) (98=.) (99=.) (0=.)
	recode VOTE_LE (7=.) (8=.) (9=.) (0=2) // NAP/not eliglible is not voted 
	recode ATTEND (97=.) (98=.) (99=.)
	recode RELIGGRP (97=.) (98=.) (99=.)
	recode SEX (9=.)
	recode AGE (998=.) (999=.)
	recode DEGREE (8=.) (9=.)		
	recode TOPBOT (0=.) (98=.) (99=.)	
	
	recode ISCO88 (9997=.) (9998=.)  (9999=.) 
	recode URBRURAL (9=.)
	
	
	recode HHCHILDR (96=0) (99=.) // If "not a private household" in HOMPOP
	recode HHTODD (96=0) (99=.) // If "not a private household" in HOMPOP

	recode ISCO88 (9997=.) (9998=.)  (9999=.) 
	recode URBRURAL (9=.)
	
	recode *_INC (999990=.) (999997=.) (999998=.) (999999=.) (99999999=.) (99999990=.) (9999990=.) (9999999=.) (99999997=.) (99999998=.)
	
	//Save data set for imputation
	save "$datadir/ISSP2010\ISSP_2010_basic_cleaning", replace

  