/******************************************************************************* 	
*	Replication file: ISSP 2010												   *
********************************************************************************
*/


*** Standard settings go here
	clear all
	set more off

		
			
********************************************************************************
*	LOAD DATA															   	   *
********************************************************************************	
	
	//Load countra level predictors
	import excel "${maindir}/data/country_predictors/country_predictors.xlsx", sheet("stata_2010") firstrow clear
	destring, replace
	save "${maindir}/data/country_predictors/country_predictors_2010.dta", replace 
	
	
	//Load data with imported values 
	use "$datadir/ISSP2010/imputed_variables_wIncome_2010.dta", clear
	merge 1:1 CASEID using   "$datadir/ISSP2010/ZA5500_v3-0-0.dta", keepusing(country c_alphan)
	keep if _merge 	== 3
	drop _merge
	
	//keep only those needed -- i.e. those that are also surveyed in 2020
	keep country c_alphan CASEID v5 v6 v11 v13 v15 v16 v22 v23 v24 v27 v29 v30 v31 v33 v35 v36 v37 v38 v39 v40 v41 v42 v43 v44 v45 v60 v62 v63 VOTE_LE PARTY_LR ATTEND RELIGGRP SEX AGE DEGREE ISCO88 URBRURAL TOPBOT  INC_decile HHCHILDR HHTODD
	
	//Label variables
	la var c_alphan "Country Prefix ISO 3166 Code - alphanumeric"
	la var	v5 "Q1a Most important issues for [R's COUNTRY] today? V5"
	la var	v6 "Q1b Next most important issue? V6"
	la var	v11 "Q4a Amount of trust in most people V11"
	la var	v13 "Q5a Trust in people in government V13"
	la var	v15 "Q6 How concerned in environmental issues? V15"
	la var	v16 "Q7a Most important problem for [Country] as a whole? V16"
	la var	v22 "Q9c Science: solve environmental problems V22"
	la var	v23 "Q10a Worry about future environment V23"
	la var	v24 "Q10b Modern life harms the environment V24"
	la var	v27 "Q11b  Economic growth: harms the environment V27"
	la var	v29 "Q12a Protect environment: pay much higher prices V29"
	la var	v30 "Q12b Protect environment: pay much higher taxes V31"
	la var  v31 "Q12c Protect environment: cut your standard of living"
	la var	v33 "Q13b Do what is right costs money takes time V33"
	la var	v35 "Q13d No point unless others do the same V35"
	la var	v36 "Q13e Many about environment exaggerated V36"
	la var	v37 "Q13f Hard to know whether the way I live is helpful or harmful to the environmen V37"
	la var	v38 "Q13g Environmental problems have a direct effect on my everyday life. V38"
	la var	v39 "Q14a Air pollution caused by cars is for environment V39"
	la var	v40 "Q14b Air pollution caused by industry is for environment V40"
	la var	v41 "Q14c Pesticides and chemicals used in farming are for environment V41"
	la var	v42 "Q14d Pollution river, lake - how dangerous for environment V42"
	la var	v43 "Q14e A rise in world's temperature caused by climate change V43"
	la var	v44 "Q14f Modifying the genes of certain crops is V44"
	la var	v45 "Q14g Nuclear power stations are V45"
	la var	v60 "Q20f Avoid buying certain products for environmental reasons V60"
	la var	v62 "Q22a Last five years: signed a petition <KR: active support> V62"
	la var	v63 "Q22b Last five years: given money to a environmental group"
	la var	SEX "Sex of Respondent SEX"
	la var	AGE "Age of respondent AGE"
	la var	DEGREE "Education II: Highest education level: categories DEGREE"
	la var	ISCO88 "Occupation ISCO/ ILO 1988 ISCO88"
	la var	RELIGGRP "Groups of religious affiliations (derived from nat_RELIG) RELIGGRP"
	la var	ATTEND "Attendance of religious services ATTEND"
	la var	TOPBOT "Top-Bottom self-placement TOPBOT"
	la var	PARTY_LR "R: Party affiliation: left-right scale PARTY_LR"
	la var	VOTE_LE "Did respondent vote in last general election? VOTE_LE"
	la var	URBRURAL "Place of living: urban - rural URBRURAL"
	la var	CASEID  "ID Number of Respondent"
	la var  INC_decile "Country specific household income decile"


	
********************************************************************************
*	Bring all Likert variables on the same scale							   *
********************************************************************************	
	
	
	* 0 very unwilling/strongly against/never/not at all – 1 very willing/strongy in favour/a lot/great extend
	foreach v in v13 v22 v23 v24 v27 v31 v33 v35 v36 v38  v37 v39 v40 v41 v42 v43 v44 v45 {
	recode `v' (1=1) (2=0.75) (3=0.5) (4=0.25) (5=0)
	}
	
	* Opposite sign, 5-point Likert scale
	foreach v in v11 v15 URBRURAL{
	recode `v' (1=0) (2=0.25) (3=0.5) (4=0.75) (5=1)
	}
	
	* 4-point Likert scale 
	foreach v in v60{
	recode `v' (1=1) (2=0.66) (3=0.33) (4=0) 
	}
	
	*Dummies 
	foreach v in v62 v63 VOTE_LE{
	recode `v' (2=0)
	}


	

********************************************************************************
*	Code new variables														   *
********************************************************************************	
	
	
// OUTCOME VARIABLE 
	
// OPPOSITION ENVIRONMENTAL TAXES
	gen nowtp_taxes = . 
	replace nowtp_taxes = 1 if v30==4|v30==5
	replace nowtp_taxes = 0 if v30==3|v30==1|v30==2|v30==8
	la var nowtp_taxes "Not willing to pay much higher taxes to protect the environment"
	
			gen nowtp_taxes_strong = . 
	replace nowtp_taxes_strong = 1 if v30==5
	replace nowtp_taxes_strong = 0 if v30==4|v30==3|v30==1|v30==2|v30==-8
	la var nowtp_taxes_strong "No willingness to pay much higher taxes to protect the environment strong"
	
			gen nowtp_taxes_weak = . 
	replace nowtp_taxes_weak = 1 if v30==-8|v30==3|v30==4|v30==5
	replace nowtp_taxes_weak = 0 if v30==1|v30==2
	la var nowtp_taxes_weak "No willingness to pay much higher taxes to protect the environment weak"
	
	
	
// OPPOSITION ENVIRONMENTAL PRICES
	gen nowtp_prices = . 
	replace nowtp_prices = 1 if v29==4|v29==5
	replace nowtp_prices = 0 if v29==3|v29==1|v29==2|v29==8
	la var nowtp_prices "Not willing to pay much higher prices to protect the environment"
	
// WILLINGNESS CUT STANDARD OF LIVING
	gen willing_cutsl = . 
	replace willing_cutsl = 0 if v31==1|v31==0.75|v31==0.5|v31==-8
	replace willing_cutsl = 1 if v31==0.25|v31==0
	la var willing_cutsl "Protect environment: cut your standard of living"	
	label define willing_cutsl_lb 1 "Very unwilling" 0 "Not very unwilling"
	la value willing_cutsl willing_cutsl_lb
	tab willing_cutsl

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
	replace priorization_env = 1 if v5 == 4
	replace priorization_env = 1 if v6 == 4
	la var priorization_env "Environment among most important issues in country"
	label define prio_lb 1 "Most important" 0 "Not most important"
	la value priorization_env prio_lb
	
	*KNOWLEDGE
	gen knowledge = v37 
	la var knowledge "Knowledge"
	label define knowledge_lb 1 "Easy to know" 0 " Hard to know"
	la value knowledge knowledge_lb
	
	*SCEPTICISM 
	gen scepticism = v36
	la var scepticism "Scepticism"
	label define scepticism_lb 1 "Agree strongly" 0 "Disagree strongly"
	la value scepticism scepticism_lb
	
	*SERIOUSNESS
		** not in survey 2010 ** alternative: v43
	gen seriousness = v43
	la var seriousness "Seriousness climate change"
 
	
	*BELIEF
		** not in survey 2010 **
	
	
	*RISK PERCEPTIONS 
	alpha v38 v39 v40 v41 v42 v43 v44 v45, item generate(risk_perception) // 0.78
	la var risk_perception "Risk perception environmental issues"
	label define risk_perception_lb 1 "Strong risk perceptions" 0 "Weak risk perceptions"
	la value risk_perception risk_perception_lb
	
	*DIRECT EXPERIENCE ENVIRONMENTAL ISSUES 
		** not in survey 2010 ** 
	
	*EVERY DAY LIFE AFFECTED BY ENVIRONMENTAL ISSUES
	gen everyday_life = v38 
	la var everyday_life "Everyday life affected"	
	
	*ENJOYS NATURE 
		** not in survey **
	
	*PROTECTED AREAS MORE IMPORTANT THAN GROWTH
	** not in survey **
	
// PSYCHOLOGICAL FACTORS/VALUES AND IDEOLOGIES 
	
	*TRUST IN PEOPLE
	gen trust_people = v11 
	la var trust_people "Trust in people"
	label define trust_people_lb 1 "High trust" 0 "No trust"
	la value trust_people trust_people_lb
	
	*TRUST IN INSTITUTIONS
	*different coding: only government instead of all institutions
	gen trust_institutions = v13
	la var trust_institutions "Trust in institutions"
	replace trust_institutions=round(trust_institutions, 0.1)


	*RECIPROCITY 
	gen reciprocity = v35
	la var reciprocity "Reciprocity"
	label define reciprocity_lb 1 "Agree strongly" 0 "Disagree strongly"
	la value reciprocity reciprocity_lb	
	
	*ENVIRONMENTAL COMMITMENT
	gen env_committment = v33
	la var env_committment "Environmental committment"

	
	*MODERN LIFE HARMS ENVIRONMENT 
	gen modern_life_harms = v24
	la var modern_life_harms "Modern life harms the environment"
	label define modern_life_harms_lb 1 "Strongly agree" 0 "Strongly disagree"
	la value modern_life_harms modern_life_harms_lb
	
	*SCIENCE SOLVE ENVIRONMENTAL PROBLEMS
	gen science_solution = v22
	la var science_solution "Science will solve environmental problems"
	label define science_solution_lb 1 "Strongly agree" 0 "Strongly disagree"
	la value science_solution science_solution_lb

	*WILLINGNESS CUT STANDARD OF LIVING

	
	*GROWTH HARMS ENVIRONMENT 
	gen growth_harms = v27
	la var growth_harms "Economic growth harms environment"
	label define growth_harms_lb 1 "Agree strongly" 0 "Disagree stronlgy"
	la value growth_harms growth_harms_lb	
	
	*WORRY ABOUT JOBS
	gen worry_jobs = v23
	label var worry_jobs "Jobs/prices more important than environment"
	label define worry_jobs_lb 1 "Agree strongly" 0 "Disagree stronly"
	la value worry_jobs worry_jobs_lb
	
	
	*ENVIRONMENTAL CONSUMPTION 
	gen env_consumption = v60
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
	recode VOTE_LE (7=0) (8=0) (9=0)
	gen vote_le = VOTE_LE
	la var vote_le "Voted in last election"
	label define vote_le_lb 1 "Yes" 0 "No"
	la value vote_le vote_le_lb
	
	
	*RELIGIOUS -- where to place missings
	gen attend_service = 1
	replace attend_service = 0 if ATTEND ==0
	replace attend_service = 0 if ATTEND ==8

	gen religious = . 
	replace religious = 1 if  RELIGGRP >0
	replace religious = 0 if  RELIGGRP ==0
	
	alpha attend_service religious, item generate(religiousness) // 0.71
	la var religiousness "Religiousness"
	label define religiousness_lb 1 "Religious" 0 "Not religious"
	la value religiousness religiousness_lb
	drop religious attend_service
	
	*VEGETARIAN 
		** not in survey 2010 **	
		
	*USED PLANE
		** not in survey 2010 **	

	
	*MORE ROOMS THAN ADULTS IN HH 
		** not in survey 2010 **	

	
	*TIME IN CAR/MOTOR VEHICLE // 
		** not in survey 2010 **	

	*SIGNED PETITION
	gen petition = v62
	la var petition "Signed environmental petition"
	la define pet_lb 1 "Yes" 0 "No"
	la val petition pet_lb
	
	*ENVIRONMENTAL DONATION
	gen donation = v63
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
	gen education = DEGREE
	la var education "Education level"

	
	*RECODE INCOME DECILES 
	recode INC_decile (2=1) (3=2) (4=3) (5=4) (6=5) (7=6) (8=7) (9=8) (10=9) (11=10) 

	label define inc_decile_lb 1 "Lowest decile" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "Highest decile"
	la value INC_decile inc_decile_lb
	
	*GAP PERCEPTION REALITY 
		*Top-Bottom self-placement minus relative household income decile
		gen gap_perception = TOPBOT-INC_decile
		la var gap_perception "Gap decile self-placement vs. decile household income"
		label define gap_perception_lb -9 "Higher selfplacement than reality" 0 "Correct placement" 9 "Lower selfplacement than reality"
		la value gap_perception gap_perception_lb
	
	*CHILDREN IN HOUSEHOLD
	gen children = 0 
	replace children = 1 if HHCHILDR>0
	replace children = 1 if HHTODD>0
	la var children "Household with children"
	label define children_lb 1 "Yes" 0 "No"
	la value children children_lb
	

	
	*GREEN JOB
		*** Generate categorization of jobs based on ISCO08 classification. We follow the recent OECD-ILO classification (OECD, 2023: The green side of productivit – An international classification of green and brown occupations https://www.oecd-ilibrary.org/economics/the-green-side-of-productivity_a363530f-en)
		
		gen brown_job = 0
		replace brown_job = 1 if ISCO88==3116|ISCO88==8152|ISCO88==8153|ISCO88==8154|ISCO88==8159|ISCO88==8122|ISCO88==8123|ISCO88==8124|ISCO88==6141|ISCO88==6142|ISCO88==8211|ISCO88==7243|ISCO88==8240|ISCO88==8240|ISCO88==8122|ISCO88==8123|ISCO88==8124|ISCO88==8262|ISCO88==8272|ISCO88==8273|ISCO88==8274|ISCO88==8275|ISCO88==8276|ISCO88==8277|ISCO88==8278|ISCO88==8279|ISCO88==8143|ISCO88==8139|ISCO88==8171|ISCO88==8172|ISCO88==8211|ISCO88==8221|ISCO88==8222|ISCO88==8223|ISCO88==8224|ISCO88==8229|ISCO88==8231|ISCO88==8232|ISCO88==8240|ISCO88==8251|ISCO88==8252|ISCO88==8253|ISCO88==8261|ISCO88==8262|ISCO88==8263|ISCO88==8264|ISCO88==8265|ISCO88==8266|ISCO88==8269|ISCO88==8271|ISCO88==8272|ISCO88==8273|ISCO88==8274|ISCO88==8275|ISCO88==8276|ISCO88==8277|ISCO88==8278|ISCO88==8279|ISCO88==8281|ISCO88==8282|ISCO88==8283|ISCO88==8284|ISCO88==8285|ISCO88==8286|ISCO88==8290|ISCO88==2113|ISCO88==2146|ISCO88==2147|ISCO88==3511|ISCO88==3211|ISCO88==8155|ISCO88==8121|ISCO88==1311|ISCO88==7122|ISCO88==7142|ISCO88==7211|ISCO88==7221|ISCO88==7222|ISCO88==7224|ISCO88==7314|ISCO88==7322|ISCO88==7241|ISCO88==7245|ISCO88==7242|ISCO88==7411|ISCO88==7414|ISCO88==7416|ISCO88==7421|ISCO88==7422|ISCO88==7423|ISCO88==7435|ISCO88==7436|ISCO88==7437|ISCO88==7112|ISCO88==8112|ISCO88==8121|ISCO88==8223|ISCO88==8231|ISCO88==8232|ISCO88==8253|ISCO88==8261|ISCO88==7432|ISCO88==8264|ISCO88==8271|ISCO88==8142|ISCO88==8141|ISCO88==8131|ISCO88==8290|ISCO88==8311|ISCO88==8332|ISCO88==8333|ISCO88==9142|ISCO88==9311|ISCO88==9153
	
	*Label variable
	lab var brown_job "Brown job"
	label define brown_job_lb 1 "Brown" 0 "Rest"
	label value brown_job brown_job_lb
		

	*URBAN RURAL 
	gen rural = URBRURAL
	la var rural "Living in rural area"
	label define rural_lb 0 "A big city" 1 "A farm or home in the country"
	la value rural rural_lb

	
//Merge in country-level predicotors

		
	merge m:1 country using "${datadir}/country_predictors/country_predictors_2010.dta" 
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
	
	
	
	**Keep only those newly created
	keep CASEID c_alphan INC_decile nowtp_taxes nowtp_taxes_strong nowtp_taxes_weak nowtp_prices willing_cutsl concern cc_env_problem priorization_env knowledge scepticism seriousness everyday_life risk_perception trust_people trust_institutions reciprocity env_committment modern_life_harms science_solution growth_harms worry_jobs env_consumption right_party vote_le religiousness petition donation female age education gap_perception rural children brown_job weighted_carbon_price revenue_recycling inc_tax env_tax  good_governance

	**Drop observations with missing outcome 
	
	**CLEAN ISSP 2010 DATASET
	save "$datadir/ISSP2010/ISSP2010_clean_wcountry.dta", replace
	


///Descriptive Table
	*ssc install estout
	
	eststo clear
	global vars worry_jobs growth_harms modern_life_harms science_solution    /// Relationship Environment Economy
	trust_people trust_institutions religiousness   right_party vote_le  env_committment reciprocity   /// VALUES 
	env_consumption donation petition /// lifestyle
	female age children rural education   INC_decile gap_perception  brown_job  /// DEMOGRAPHICS
	concern priorization_env cc_env_problem knowledge scepticism risk_perception   seriousness everyday_life     /// ENVIRONMENTAL VALUATIONS
	good_governance env_tax inc_tax weighted_carbon_price  revenue_recycling // country-level tax system
	eststo: estpost summarize $vars 
	esttab using "${outputdir}/descriptives_vars_2010.tex", cells("mean(fmt(a2)) sd(fmt(a2)) min(fmt(a2)) max(fmt(a2))") label replace  noobs 


	