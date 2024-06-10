/******************************************************************************* 	
*	Replication file: OLS and logistic estimate													   *
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
	
*** Standardize variables

foreach v of varlist env_committment worry_jobs trust_institutions donation env_consumption reciprocity trust_people petition right_party growth_harms modern_life_harms vote_le science_solution religiousness education gap_perception brown_job age rural INC_decile children female concern scepticism priorization_env everyday_life seriousness risk_perception cc_env_problem knowledge inc_tax good_governance env_tax weighted_carbon_price revenue_recycling {
		egen z_`v' = std(`v')
}

drop env_committment worry_jobs trust_institutions donation env_consumption reciprocity trust_people petition right_party growth_harms modern_life_harms vote_le science_solution religiousness education gap_perception brown_job age rural INC_decile children female concern scepticism priorization_env everyday_life seriousness risk_perception cc_env_problem knowledge inc_tax good_governance env_tax weighted_carbon_price revenue_recycling


*** Regressions

reg nowtp_taxes z_env_committment z_worry_jobs z_trust_institutions z_donation z_env_consumption z_reciprocity z_trust_people z_petition z_right_party z_growth_harms z_modern_life_harms z_vote_le z_science_solution z_religiousness z_education z_gap_perception z_brown_job z_age z_rural z_INC_decile z_children z_female z_concern z_scepticism z_priorization_env z_everyday_life z_seriousness z_risk_perception z_cc_env_problem z_knowledge z_inc_tax z_good_governance z_env_tax z_weighted_carbon_price z_revenue_recycling, cluster(c_alphan)                           
est store tab1


	
	estout tab1 using "$graphdir\OLS_estimate.tex", ///
                                style(tex) cells(b(star fmt(4)) se(par fmt(4))) posthead() ///
                starlevels( * 0.1 ** 0.05 *** 0.01)  ///
                stats(N, layout(@ @) labels("Number of observations"))  mlabels(none)   ///
                collabels(none) eql(none) notype label postfoot( \addlinespace) replace        
    eststo clear			
						