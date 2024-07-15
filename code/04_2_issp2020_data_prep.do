/******************************************************************************* 	
*	Replication file: ISSP 2020												   *
********************************************************************************
*/


*** Standard settings go here
	clear all
	set more off

					
			
	
********************************************************************************
*	MERGE DATA															   	   *
********************************************************************************	
		
	
	import excel "${maindir}/data/country_predictors/country_predictors.xlsx", sheet("stata_2020") firstrow clear
	destring, force replace
	save "${maindir}/data/country_predictors/country_predictors_2020.dta", replace 
	
		
	use "$datadir/ISSP2020/imputed_variables_wIncome_full.dta", clear
	rename * *_i
	rename CASEID_i CASEID
	rename RINC_deciles_i RINC_decile
	rename INC_deciles_i INC_decile
	save "$datadir/ISSP2020/imputed_variables_wIncome_full_test.dta", replace
		
	use "$datadir/ISSP2020/ZA7650_v2-0-0.dta", clear
	merge 1:1 CASEID using "$datadir/ISSP2020/imputed_variables_wIncome_full_test.dta"
	drop _merge
	
	*Set globals with imputed or replaced values 
	global imputed v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v46 v47 v49 v50 v51 v52 v53 v54 v55 v56 v57 v58 v59 v60 PARTY_LR SEX EDULEVEL AGE HHTODD HHCHILDR TOPBOT ISCO08 WORK v48 ATTEND VOTE_LE RELIGGRP HHADULT URBRURAL
	foreach v in $imputed{
	replace `v' = `v'_i if `v' < 0
	}
	

	*Trust in people is in a different variable for China, replace in main variable
	replace v10 = CN_v10 if c_alphan =="CN"
		
	*Same with SI_v49
	replace v49 = SI_v49 if c_alphan =="SI"

	
	*Clean data 
	
	*drop imputed values
	drop *_i
	
	*Drop country-specifc vars 
	
		*Education
		drop *_ISCD
		
		*Religion
		drop *_RELIG
		
		*Party vote in last election
		drop *_PRTY
		
		*Ethnic groups
		drop *_ETHN1
		drop *_ETHN2
		
		*Region
		drop *_REG
	
********************************************************************************
*	Bring all Likert variables on the same scale							   *
********************************************************************************	

	
	* 0 very unwilling/strongly against/never/not at all – 1 very willing/strongy in favour/a lot/great extend
	foreach v in v20 v21 v22 v23 v24 v25 v28  v30 v31 v32 v33 v34 v36 v37 v38 v39 v40 v41 v42 v43 v47{
	recode `v' (1=1) (2=0.75) (3=0.5) (4=0.25) (5=0)
	}
	
	* Opposite sign, 5-point Likert scale
	foreach v in v10 v15 v35 v46 v58 v59 v60 URBRURAL{
	recode `v' (1=0) (2=0.25) (3=0.5) (4=0.75) (5=1)
	}
	
	* 4-point Likert scale 
	foreach v in v17{
	recode `v' (1=0) (2=0.33) (3=0.66) (4=1) 
	}
	
	* 4-point Likert scale 
	foreach v in v52 v53{
	recode `v' (1=1) (2=0.66) (3=0.33) (4=0) 
	}
	
	*Dummies 
	foreach v in v54 v55 v56 v57 VOTE_LE{
	recode `v' (2=0)
	}
	
	*10-point Likert scale
	foreach v in v11 v12 v13 v14 {
	recode `v' (0=0.0000) (1=0.1000) (2=0.2000) (3=0.3000) (4=0.4000) (5=0.5000) (6=0.6000) (7=0.7000) (8=0.8000) (9=0.9000) (10=1.000)
	}
	
	* Reversed 10-point Likert scale
	foreach v in v18 v19{
	recode `v' (10=0) (9=0.1) (8=0.2) (7=0.3) (6=0.4) (5=0.5) (4=0.6) (3=0.7) (2=0.8) (1=0.9) (0=1)
	}
	

	** Round variables to 2 decimales
	tostring v10 v20 v21 v22 v23 v24 v25 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v47 v15 v46 v58 v59 v60 URBRURAL v17 v52 v53 v54 v55 v56 v57 v11 v12 v13 v14  v18 v19, replace force format(%7.2f)

	destring v10 v20 v21 v22 v23 v24 v25 v28 v29 v30 v31 v32 v33 v34 v35 v36 v37 v38 v39 v40 v41 v42 v43 v47 v15 v46 v58 v59 v60 URBRURAL v17 v52 v53 v54 v55 v56 v57 v11 v12 v13 v14  v18 v19, replace 
	
	save "$datadir/ISSP2020/reversed.dta", replace

	********************************************************************************
*	Code new variables														   *
********************************************************************************	
	
	
// OUTCOME VARIABLE 
	gen wtp_taxes = . 
	replace wtp_taxes = 1 if v27==1|v27==2
	replace wtp_taxes = 0 if v27==3|v27==4|v27==5|v27==-8
	la var wtp_taxes "Willingness to pay much higher taxes to protect the environment"
	label define wtp_taxes_lb 1 "Willing to pay much higher taxes" 0 "Not willing to pay much higher taxes"
	la value wtp_taxes wtp_taxes_lb
	tab wtp_taxes
	
// OPPOSITION 
	gen nowtp_taxes = . 
	replace nowtp_taxes = 1 if v27==4|v27==5
	replace nowtp_taxes = 0 if v27==3|v27==1|v27==2|v27==-8
	la var nowtp_taxes "No willingness to pay much higher taxes to protect the environment"
	
		gen nowtp_taxes_strong = . 
	replace nowtp_taxes_strong = 1 if v27==5
	replace nowtp_taxes_strong = 0 if v27==4|v27==3|v27==1|v27==2|v27==-8
	la var nowtp_taxes_strong "No willingness to pay much higher taxes to protect the environment strong"
	
		gen nowtp_taxes_weak = .
	replace nowtp_taxes_weak = 1 if v27==-8|v27==3|v27==4|v27==5
	replace nowtp_taxes_weak = 0 if v27==1|v27==2
	la var nowtp_taxes_weak "No willingness to pay much higher taxes to protect the environment weak"
	
	
	gen wtp_prices = . 
	replace wtp_prices = 1 if v26==1|v26==2
	replace wtp_prices = 0 if v26==3|v26==4|v26==5|v26==-8
	la var wtp_prices "Willingness to pay much higher prices to protect the environment"	
	label define wtp_prices_lb 1 "Willing to pay much higher prices" 0 "Not willing to pay much higher prices"
	la value wtp_prices wtp_prices_lb
	tab wtp_prices
	
// OPPOSITION 
	gen nowtp_prices = . 
	replace nowtp_prices = 1 if v26==4|v26==5
	replace nowtp_prices = 0 if v26==3|v26==1|v26==2|v26==-8
	la var nowtp_prices "No willingness to pay much higher prices to protect the environment"
	
// ENVIRONMENTAL EVALUATIONS 
	
	*CONCERN
	gen concern = v15
	la var concern "Concerned about environmental issues"
	label define concern_ln 1 "Very concerned" 0 "Not at all concerned"
	la value concern concern_ln
	
	*MOST IMPORTANT ENVIRONMENTAL PROBLEM
	gen cc_env_problem= 0 
	replace cc_env_problem = 1 if v16 == 7 
	la var cc_env_problem "Climate change most important environmental problem"
	label define environ_problems_lb 1 "Yes" 0 "Others"
	la value cc_env_problem environ_problems_lb
	
	*PRIORIZATION of the environment
	gen priorization_env = 0 
	replace priorization_env = 1 if v1 == 4
	replace priorization_env = 1 if v2 == 4
	la var priorization_env "Environment among most important issues in country"
	label define prio_lb 1 "Most important" 0 "Not most important"
	la value priorization_env prio_lb
	
	*KNOWLEDGE
	gen knowledge = v35 
	la var knowledge "Knowledge"
	label define knowledge_lb 1 "Easy to know" 0 " Hard to know"
	la value knowledge knowledge_lb
	
	*SCEPTICISM 
	gen scepticism = v34
	la var scepticism "Scepticism"
	label define scepticism_lb 1 "Agree strongly" 0 "Disagree strongly"
	la value scepticism scepticism_lb
	
	*SERIOUSNESS
	alpha v18 v19, item generate(seriousness) // 0.88
	la var seriousness "Seriousness climate change"
	replace seriousness=round(seriousness, 0.1)
	** Round variables to 2 decimales
	tostring seriousness , replace force format(%7.2f)
	destring seriousness, replace
	recode seriousness (-4=-0.1) (-2=-0.1) // does not belief in climate change so seriousness missing. Recoded as -0.1 to not lose observations
	label define seriousness_lb 1 "Very serious" 0 "Not serious at all"
	la value seriousness seriousness_lb
	
	
	
	*BELIEF
	gen belief_cc = v17 
	la var belief_cc "Belief in human made climate change"
	label define belief_cc_lb 1 "Strong belief human made CC" 0 "Does not believe"
	la value belief_cc belief_cc_lb
	
	*RISK PERCEPTIONS 
	alpha v37 v38 v39 v40 v41 v42 v43, item generate(risk_perception) // 0.78
	la var risk_perception "Risk perception environmental issues"
	label define risk_perception_lb 1 "Strong risk perceptions" 0 "Weak risk perceptions"
	la value risk_perception risk_perception_lb
	
	*EVERY DAY LIFE AFFECTED BY ENVIRONMENTAL ISSUES
	gen everyday_life = v36 
	la var everyday_life "Everyday life affected"
	
	
	*neighbourhood EXPERIENCE ENVIRONMENTAL ISSUES v36
	alpha v58 v59 v60, item generate(direct_experience) //0.76
	la var direct_experience "Neighbourhood experience of environmental issues"
	label define direct_experience_lb 1 "Direct experience" 0 "No direct experience"
	la value direct_experience direct_experience_lb
	
	*ENJOYS NATURE 
	gen enjoy_nature = v46 
	la var enjoy_nature "Enjoys being in nature" 
	label define enjoy_nature_lb 1 "Enjoys nature" 0 "Does not enjoy nature"
	la value enjoy_nature enjoy_nature_lb
	
	*PROTECTED AREAS MORE IMPORTANT THAN GROWTH
	gen protected_areas = v29 
	recode protected_areas (5=1) (4=0.75) (3=0.5) (2=0.25) (1=0)
	la var protected_areas "Protected areas more imp. than econ. growth"
	label define protected_areas_lb 1 "More important" 0 "Less important"
	la value protected_areas protected_areas_lb
	
// PSYCHOLOGICAL FACTORS/VALUES AND IDEOLOGIES 
	
	*TRUST IN PEOPLE
	gen trust_people = v10 
	la var trust_people "Trust in people"
	label define trust_people_lb 1 "High trust" 0 "No trust"
	la value trust_people trust_people_lb
	
	*TRUST IN INSTITUTIONS
	alpha v11 v12 v13 v14, item generate(trust_institutions) // 0.74
	la var trust_institutions "Trust in institutions"
	replace trust_institutions=round(trust_institutions, 0.1)
	** Round variables to 2 decimales
	tostring trust_institutions , replace force format(%7.2f)
	destring trust_institutions, replace
	label define trust_institutions_lb 1 "Complete trust" 0 "No trust at all"
	la value trust_institutions trust_institutions_lb
	

	*RECIPROCITY 
	gen reciprocity = v33
	la var reciprocity "Reciprocity"
	label define reciprocity_lb 1 "Agree strongly" 0 "Disagree strongly"
	la value reciprocity reciprocity_lb	
	
	*ENVIRONMENTAL COMMITMENT
	gen env_committment = v31
	la var env_committment "Environmental committment"
	
	*DEGROWTH!!!! WRONG CODING OF SOME VARS !!!! 
	alpha  v21 v22 v24 v25 v28 v29, item 
	alpha v22 v25, item 
	alpha v22 v24 v25 v28, item 
	
	*MODERN LIFE HARMS ENVIRONMENT 
	gen modern_life_harms = v22
	la var modern_life_harms "Modern life harms the environment"
	label define modern_life_harms_lb 1 "Strongly agree" 0 "Strongly disagree"
	la value modern_life_harms modern_life_harms_lb
	
	*SCIENCE SOLVE ENVIRONMENTAL PROBLEMS
	gen science_solution = v20
	la var science_solution "Science will solve environmental problems"
	label define science_solution_lb 1 "Strongly agree" 0 "Strongly disagree"
	la value science_solution science_solution_lb

	*WILLINGNESS CUT STANDARD OF LIVING
	gen willing_cutsl = . 
	replace willing_cutsl = 0 if v28==1|v28==0.75|v28==0.5|v28==-8
	replace willing_cutsl = 1 if v28==0.25|v28==0
	la var willing_cutsl "Protect environment: cut your standard of living"	
	label define willing_cutsl_lb 1 "Very unwilling" 0 "Not very unwilling"
	la value willing_cutsl willing_cutsl_lb
	tab willing_cutsl
	
	*GROWTH HARMS ENVIRONMENT 
	gen growth_harms = v25
	la var growth_harms "Economic growth harms environment"
	label define growth_harms_lb 1 "Agree strongly" 0 "Disagree stronlgy"
	la value growth_harms growth_harms_lb	
	
	*WORRY ABOUT JOBS
	gen worry_jobs = v21
	label var worry_jobs "Jobs/prices more important than environment"
	label define worry_jobs_lb 1 "Agree strongly" 0 "Disagree stronly"
	la value worry_jobs worry_jobs_lb
	
	
	*PRO-ENVIRONMENTAL BEHAVIOUR
	alpha v31 v52 v53 v54 v55 v56 v57, item // 0.64
	alpha v54 v55 v56 v57 , item //0.63
	
	*ENVIRONMENTAL CONSUMPTION 
	gen env_consumption = v53
	label var env_consumption "Environmental consumption"
	label define green_consumption_lb 1 "Always" 0 "Never"
	label value env_consumption green_consumption_lb
	
	*POLITICAL IDEOLOGY -- left/right -- 
	tab PARTY_LR
	gen right_party = 0 
	replace right_party = 1 if PARTY_LR==4|PARTY_LR==5
	la var right_party "Right party preference"
	label define right_party_lb 1 "Right party preference" 0 "Left or center party preference"
	la value right_party right_party_lb
	 
	*VOTE in last election
	tab VOTE_LE
	gen vote_le = VOTE_LE
	la var vote_le "Voted in last election"
	label define vote_le_lb 1 "Yes" 0 "No"
	la value vote_le vote_le_lb
	
	*RELIGIOUS -- where to place missings
	gen attend_service = 1
	replace attend_service = 0 if ATTEND >7
	
	gen religious = . 
	replace religious = 1 if  RELIGGRP >0
	replace religious = 0 if  RELIGGRP ==0
	
	alpha attend_service religious, item generate(religiousness) // 0.71
	la var religiousness "Religiousness"
	label define religiousness_lb 1 "Religios" 0 "Not religious"
	la value religiousness religiousness_lb
	drop religious attend_service
	
	*VEGETARIAN 
	gen vegetarian = 0 
	replace vegetarian = 1 if v50 == 0 | v50 == -4
	la var vegetarian "Vegetarian"
	label define vegetarian_lb 1 "Yes" 0 "No"
	la value vegetarian vegetarian_lb	
	
	*USED PLANE
	gen plane = 0
	replace plane = 1 if v48>0
	la var plane "Used plane in last 12 months"
	label define plane_lb 1 "Yes" 0 "No"
	la value plane plane_lb
	
	*MORE ROOMS THAN ADULTS IN HH -- maybe problematic
	gen aux_var = HHADULT-v51
	gen large_house = 0 
	replace large_house = 1 if aux_var>0
	drop aux_var 
	la var large_house "Large house"
	label define large_house_lb 1 "Yes" 0 "No"
	la value large_house large_house_lb	
	
	*TIME IN CAR/MOTOR VEHICLE // thing about missings
	gen time_car = 1
	replace time_car = 0 if v49==0
	tab time_car 
	la var time_car "Spends time in motorized vehicle"
	label define time_car_lb 1 "Yes" 0 "No"
	la value time_car time_car_lb	
	
	*SIGNED PETITION
	gen petition = v55
	la var petition "Signed environmental petition"
	la define pet_lb 1 "Yes" 0 "No"
	la val petition pet_lb
	
	*ENVIRONMENTAL DONATION
	gen donation = v56
	la var donation "Environmental donation"
	la define donation_lb 1 "Yes" 0 "No"
	la val donation donation_lb
	
	
// DEMOGRAPHICS 

	*SEX 
	gen female = . 
	replace female = 1 if SEX == 2
	replace female = 0 if SEX == 1
	la var female "Female"
	label define female_lb 1 "Female" 0 "Male"
	la value female female_lb
	
	*AGE 
	tab AGE
	gen age = AGE 
	la var age "Age"
	
	*EDUCATION
	gen education = EDULEVEL
	la var education "Education level"
	label define education_lb 8 "PhD, Post tertiary specialization" 0 "No education, incomplete primary"
	la value education education_lb
	
	*RECODE INCOME DECILES 
	recode RINC_decile INC_decile (2=1) (3=2) (4=3) (5=4) (6=5) (7=6) (8=7) (9=8) (10=9) (11=10) 
	label define inc_decile_lb 1 "Lowest decile" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "Highest decile"
	la value RINC_decile inc_decile_lb
	la value INC_decile inc_decile_lb

	*GAP PERCEPTION REALITY 
		*Top-Bottom self-placement minus relative household income decile
		gen gap_perception = TOPBOT-INC_decile
		la var gap_perception "Gap decile self-placement vs. decile household income"
		label define gap_perception_lb -9 "Higher selfplacement than reality" 0 "Correct placement" 9 "Lower selfplacement than reality"
		la value gap_perception gap_perception_lb
	
	*CHILDREN ABOVE SCHOOL ENTRY AGE IN HOUSEHOLD
	gen children = 0 
	replace children = 1 if HHCHILDR>0
	replace children = 1 if HHTODD>0
	replace children = . if HHCHILDR == -4
	la var children "Household with children"
	label define children_lb 1 "Yes" 0 "No"
	la value children children_lb
	
	*GREEN JOB
		*** Generate categorization of jobs based on ISCO08 classification. We follow the recent OECD-ILO classification (OECD, 2023: The green side of productivit – An international classification of green and brown occupations https://www.oecd-ilibrary.org/economics/the-green-side-of-productivity_a363530f-en

		gen brown_job = 0 
	replace brown_job = 1 if ISCO08==2113|ISCO08==2145|ISCO08==2146|ISCO08==3122|ISCO08==3133|ISCO08==3134|ISCO08==3135|ISCO08==6210|ISCO08==7112|ISCO08==7132|ISCO08==7211|ISCO08==7221|ISCO08==7222|ISCO08==7224|ISCO08==7314|ISCO08==7315|ISCO08==7412|ISCO08==7413|ISCO08==7421|ISCO08==7511|ISCO08==7514|ISCO08==7516|ISCO08==7521|ISCO08==7522|ISCO08==7523|ISCO08==7532|ISCO08==7533|ISCO08==7534|ISCO08==7542|ISCO08==8112|ISCO08==8121|ISCO08==8122|ISCO08==8141|ISCO08==8142|ISCO08==8143|ISCO08==8151|ISCO08==8152|ISCO08==8154|ISCO08==8160|ISCO08==8171|ISCO08==8172|ISCO08==8181|ISCO08==8183|ISCO08==8189|ISCO08==8311|ISCO08==8342|ISCO08==8343|ISCO08==9129|ISCO08==9311|ISCO08==9623

		*Label variable
		lab var brown_job "Brown job"
		label define brown_job_lb 1 "Brown" 0 "Rest"
		label value brown_job brown_job_lb
		

		
		

	*URBAN RURAL 
	gen rural = URBRURAL
	la var rural "Living in rural area"
	label define rural_lb 0 "A big city" 1 "A farm or home in the country"
	la value rural rural_lb
	
	**CLEAN ISSP DATASET
	save "$datadir/ISSP2020/ISSP2020_clean.dta", replace
		
// CONTRY-SPECIFIC INDICATORS

********************************************************************************
*	MERGE IN COUNTRY-SPECIFIC INDICATORS															   *
********************************************************************************	
	
	use "$datadir/ISSP2020/ISSP2020_clean.dta", clear
	merge m:1 country using "${datadir}/country_predictors/country_predictors_2020.dta" 
	drop if _merge == 2 //keine Ahnung woher die Obs. kommt
	drop _merge // all matched
	
	*IMPLEMENTED POLICIES - CARBON PRICE 
	la var weighted_carbon_price "Existing carbon price"
	
	*REVENUE RECYCLING
	la var revenue_recycling "Revenue recycling"
	label define revenue_recycling_cat_lb  0 "No revenue recycling/no carbon price" 1 "Revenue recycling"
	label value revenue_recycling revenue_recycling_cat_lb	
	
	
	*GOD GOVERNANCE
	la var good_governance "Good governance"
	
	*ENVIRONMENTAL TAXES 
	la var env_tax "Environmental taxes (energy and transport)"
		
	*INCOME TAXES 
	la var inc_tax "Income taxes"
	
	
	
*** Clean data keep only relevant data
	keep CASEID country c_alphan nowtp_taxes nowtp_taxes_strong nowtp_taxes_weak wtp_taxes wtp_prices nowtp_prices concern priorization_env knowledge scepticism seriousness belief_cc risk_perception direct_experience cc_env_problem everyday_life /// ENVIRONMENTAL VALUATIONS
	trust_people trust_institutions reciprocity  worry_jobs right_party vote_le religiousness   /// VALUES 
	vegetarian  plane large_house time_car growth_harms protected_areas env_committment env_consumption willing_cutsl 	modern_life_harms science_solution donation petition enjoy_nature /// ENV VALUES
	female age education RINC_decile INC_decile gap_perception children brown_job rural /// DEMOGRAPHICS
	weighted_carbon_price revenue_recycling env_tax inc_tax good_governance /// COUNTRY LEVEL PREDICOTORS 
	DATEYR DATEMO DATEDY WEIGHT   
	

	order CASEID country c_alphan wtp_taxes wtp_prices concern priorization_env knowledge scepticism seriousness belief_cc risk_perception direct_experience cc_env_problem everyday_life /// ENVIRONMENTAL VALUATIONS
	trust_people trust_institutions reciprocity  worry_jobs right_party vote_le religiousness  /// VALUES 
	vegetarian  plane large_house time_car growth_harms protected_areas env_committment env_consumption donation petition willing_cutsl  	modern_life_harms science_solution enjoy_nature /// ENV VALUES
	female age education RINC_decile INC_decile gap_perception children brown_job rural /// DEMOGRAPHICS
	weighted_carbon_price revenue_recycling env_tax inc_tax good_governance /// COUNTRY LEVEL PREDICOTORS 
	DATEYR DATEMO DATEDY WEIGHT 

*** Drop obs with missing outcome variable to get effective sample size
	drop if nowtp_taxes ==.

*** Save dataset
	save "$datadir/ISSP2020/ISSP2020_clean_wcountry.dta", replace
	
********************************************************************************
*	DESCRIPTIVES															   *
********************************************************************************	
	use "$datadir/ISSP2020/ISSP2020_clean_wcountry.dta", clear

	gen emerging_econ = . 
	replace emerging_econ = 1 if c_alphan == "CN"|c_alphan == "RU"|c_alphan == "IN"|c_alphan == "PH"|c_alphan == "ZA"|c_alphan == "TH"
	replace emerging_econ = 0 if c_alphan == "AT"|c_alphan == "AU"| c_alphan == "CH"|c_alphan == "DE"|c_alphan == "DK"|c_alphan == "ES"|c_alphan == "FI"|c_alphan == "FR"|c_alphan == "HR"|c_alphan == "IS"|c_alphan == "JP"|c_alphan == "KR"|c_alphan == "LT"|c_alphan == "NO"|c_alphan == "NZ"|c_alphan == "SE"|c_alphan == "SI"|c_alphan == "SK"|c_alphan == "TW"|c_alphan == "US"|c_alphan == "IT"|c_alphan == "HU"


//Descriptive table: High-income vs. emerging countries
	ssc install estout
	eststo clear
	global vars concern priorization_env cc_env_problem knowledge scepticism risk_perception direct_experience everyday_life belief_cc seriousness      /// ENVIRONMENTAL VALUATIONS
	trust_people trust_institutions religiousness   right_party vote_le  env_committment reciprocity  enjoy_nature /// VALUES 
	  plane large_house time_car env_consumption donation petition /// lifestyle
	worry_jobs growth_harms modern_life_harms science_solution    /// Relationship Environment Economy
	female age children rural education   INC_decile gap_perception  brown_job  /// DEMOGRAPHICS
	good_governance env_tax inc_tax weighted_carbon_price  revenue_recycling // country-level tax system
	bys emerging_econ: eststo: estpost summarize $vars
	esttab using "${outputdir}/descriptives_vars.tex", cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2)) ") label replace  noobs 
	


//Descriptive table: 2010 vs. 2020 

**Same high income sample as 2010 data

keep  if c_alphan == "AT"|c_alphan == "AU"| c_alphan == "CH"|c_alphan == "DE"|c_alphan == "DK"|c_alphan == "ES"|c_alphan == "FI"|c_alphan == "FR"|c_alphan == "HR"|c_alphan == "IS"|c_alphan == "JP"|c_alphan == "KR"|c_alphan == "LT"|c_alphan == "NO"|c_alphan == "NZ"|c_alphan == "SE"|c_alphan == "SI"|c_alphan == "SK"|c_alphan == "TW"|c_alphan == "US"

**Average share
	sum nowtp_taxes
	
	
	eststo clear
	global vars ///
	worry_jobs growth_harms modern_life_harms science_solution    /// Relationship Environment Economy
	trust_people trust_institutions religiousness   right_party vote_le  env_committment reciprocity  /// VALUES 
	env_consumption donation petition /// lifestyle
	female age children rural education   INC_decile gap_perception  brown_job  /// DEMOGRAPHICS
	concern priorization_env cc_env_problem knowledge scepticism risk_perception   seriousness everyday_life     /// ENVIRONMENTAL VALUATIONS
	good_governance env_tax inc_tax weighted_carbon_price  revenue_recycling // country-level tax system

	eststo: estpost summarize $vars 
	esttab using "${outputdir}/descriptives_vars_2020_highincome.tex", cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") label replace  noobs 

	
	*** export data for world map taxes 
	use "$datadir/ISSP2020/ISSP2020_clean_wcountry.dta", clear
	
	keep nowtp_taxes country
	
	decode(country), gen(NAME_SORT)
	
	bysort country: egen wtp_low = mean(nowtp_taxes)
	
	keep wtp_low NAME_SORT
	
	duplicates drop wtp_low NAME_SORT, force
	
	replace NAME_SORT = "Australia" if NAME_SORT == "36. AU-Australia"
	replace NAME_SORT = "Austria" if NAME_SORT == "40. AT-Austria"
	replace NAME_SORT = "China" if NAME_SORT == "156. CN-China"
	replace NAME_SORT = "Taiwan" if NAME_SORT == "158. TW-Taiwan"
	replace NAME_SORT = "Croatia" if NAME_SORT == "191. HR-Croatia"
	replace NAME_SORT = "Denmark" if NAME_SORT == "208. DK-Denmark"
	replace NAME_SORT = "Finland" if NAME_SORT == "246. FI-Finland"
	replace NAME_SORT = "France" if NAME_SORT == "250. FR-France"
	replace NAME_SORT = "Germany" if NAME_SORT == "276. DE-Germany"
	replace NAME_SORT = "Hungary" if NAME_SORT == "348. HU-Hungary"
	replace NAME_SORT = "Iceland" if NAME_SORT == "352. IS-Iceland"
	replace NAME_SORT = "India" if NAME_SORT == "356. IN-India"
	replace NAME_SORT = "Italy" if NAME_SORT == "380. IT-Italy"
	replace NAME_SORT = "Japan" if NAME_SORT == "392. JP-Japan"
	replace NAME_SORT = "Korea, South" if NAME_SORT == "410. KR-Korea (South)"
	replace NAME_SORT = "Lithuania" if NAME_SORT == "440. LT-Lithuania"
	replace NAME_SORT = "New Zealand" if NAME_SORT == "554. NZ-New Zealand"
	replace NAME_SORT = "Norway" if NAME_SORT == "578. NO-Norway"
	replace NAME_SORT = "Philippines" if NAME_SORT == "608. PH-Philippines"
	replace NAME_SORT = "Russia" if NAME_SORT == "643. RU-Russia"
	replace NAME_SORT = "Slovakia" if NAME_SORT == "703. SK-Slovakia"
	replace NAME_SORT = "Slovenia" if NAME_SORT == "705. SI-Slovenia"
	replace NAME_SORT = "South Africa" if NAME_SORT == "710. ZA-South Africa"
	replace NAME_SORT = "Spain" if NAME_SORT == "724. ES-Spain"
	replace NAME_SORT = "Sweden" if NAME_SORT == "752. SE-Sweden"
	replace NAME_SORT = "Switzerland" if NAME_SORT == "756. CH-Switzerland"
	replace NAME_SORT = "Thailand" if NAME_SORT == "764. TH-Thailand"
	replace NAME_SORT = "United States of America" if NAME_SORT == "840. US-United States"
	
	
	save "$datadir/ISSP2020/wtp_for_map.dta", replace
					
