******************************************************************************* 	
*	Replication files: Leveraging machine learning to understand opposition to environmental tax increases across countries and over time *
********************************************************************************
*											 					               *
*	PURPOSE: 	Master do-file												   *
*					   										                   *
*											 					               *
*   OUTLINE:  	PART 1: Set user and paths for analysis					 	   *
*		   		PART 2: Run analyses		                                   *
*																			  *									
*******************************************************************************/


********************************************************************************
*	PART 1:  Set paths						                                   *
********************************************************************************

*** Standard settings go here
	clear all
	set more off

********************************************************************************
*	PART 1:  Set user and paths for analysis				                   *
********************************************************************************

**Set the user
local user=1
	
	** User 1
	else if `user'==1	{
	global maindir "set path to main folder"
	global datadir "${maindir}/data"
	global graphdir "${maindir}/figures"
	global dodir	"${maindir}/code"

						}
	
	** User 2
	else if `user'==2	{
	global maindir "set path to main folder"
	global datadir "${maindir}/data"
	global graphdir "${maindir}/figures"
	global dodir	"${maindir}/code"
					}
						
				
********************************************************************************
*	PART 2:  Run analyses					                                   *
********************************************************************************

*** Preparing 2010 data (for coherence with 2020 data)
	do "$dodir/01_1_data_cleaning_2010.do"
	
*** Impute missing variables using the MICE method for 2010
	*** In R with R-Script labelled "01_2_impute_data_2010"

*** Impute missing variables using the MICE method for 2020
	*** In R with R-Script labelled "01_3_impute_data_2020"

*** Generate relative income deciles for both years
	do "dodir/02_1_income_decile_2010"
	do "dodir/02_2_income_decile_2020"
	
*** Impute data on all important variables using MICE
	*** 2010: In R with R-Script labelled "03_1_impute_wholedata_2010"
	*** 2020: In R with R-Script labelled "03_2_impute_wholedata_2020"
	
*** Merge country predictors and bring all likert variables on the same scale for 2010
	do "$dodir/04_1_issp2010_data_prep.do" 

*** Merge country predictors and bring all likert variables on the same scale for 2020 and generate WTP map for descriptive statistics
	do "$dodir/04_2_issp2020_data_prep.do" 
	

*** Create charts for descriptives on the willingness to pay taxes
	*** WTP taxes Bar Plot 2010: In R with R-Script labelled "05_1_bar_plot_2010"
	*** WTP taxes Bar Plot 2020: In R with R-Script labelled "05_2_bar_plot_2020"	
	*** Map of WTP in 2020: In R with R-Script labelled "06_world_map"
	do "$dodir/07_1_gaps.do" 
  *** Change over time: In R with R-Script "07_2_gap_plot"
  
*** Run analysis with respect to regions for importance and dependence with random forest
  *** In R with R-Script labelled "08_1_alldependenceplots_byspace" // for direction of prediction
  *** In R with R-Script labelled "08_2_importance_plot_byspace" // for variable importance 
	*** In R with R-Script labelled "08_3_dependence_plot_byspace" // for certain direction of prediction

*** Run analysis with respect to each country for importance with random forest
  *** In R with R-Script labelled "09_heatmap_eachcountry"

*** Run analysis with respect to time for importance and dependence with random forest
  *** In R with R-Script labelled "10_1_alldependenceplots_bytime" // for direction of prediction
  *** In R with R-Script labelled "10_2_importance_plot_bytime" // for variable importance 
	*** In R with R-Script labelled "10_3_dependence_plot_bytime" // for certain direction of prediction
	
*** Run analysis with respect to the joint sample with random forest and OLS
  do "$dodir/11_1_appendcrosssections.do" 
  *** In R with R-Script labelled "11_2_alldependenceplots_fullsample"
  *** In R with R-Script labelled "11_3_importance_plot_fullsample"
  *** In R with R-Script labelled "11_4_alldependenceplots_fullsample_differentoutcome"
  *** In R with R-Script labelled "11_5_importance_plot_fullsample_differentoutcome"
  *** In R with R-Script labelled "11_6_cor_plot"
  do "$dodir/11_7_regressionestimates.do"
  *** In R with R-Script labelled "11_7_alldependenceplots_fullsample_differentoutcome2"
  *** In R with R-Script labelled "11_8_importance_plot_fullsample_differentoutcome2"


