**Replication guide „Leveraging machine learning to understand opposition to environmental tax increases across countries and over time"

**Data**

The research reported in the article „Leveraging machine learning to understand opposition to environmental tax increases across countries 
and over time” uses data from the ISSP's Environment III (2010) and Environment IV (2020) modules, which are freely accessible upon registration 
on the ISSP website (https://issp.org/data-download/by-year/). For the 2010 module version 3 (2019-06-13 Australia, Iceland, Netherlands and 
Portugal added, Errata corrected (current version) https://doi.org/10.4232/1.13271) is used and for the 2020 module version 2 is used (2023-08-25 
Full Release, 14 countries added (current version) https://doi.org/10.4232/1.14153). These are the latest available versions when the analysis was conducted.

The data files needed are: 
- 2010: ZA5500_v3-0-0
- 2020: ZA7650_v2-0-0

The results reported in the article and the supplementary material can be replicated using the raw data provided by ISSP.

The replication package also contains data which is merged to the ISSP data and used for separate analysis. The folder “data” contains the following files: 
- country_predictors.xlsx contains the country level data used as explanatory variables for the analysis. The country-level tax system variables are primarily from 2019 and 2009, the pre-survey years. Data on income tax were retrieved from the UNU-WIDER Government Revenue Dataset (UNU-WIDER, 2023) and the environmental taxes as percent of GDP from the OECD  (OECD, 2019). We obtained existing weighted carbon prices from Dolphin (2022), while data on revenue recycling schemes were self-collected and based on information from Postic and Fetet (2020) and European Environment Information and Observation Network (2019). 
- effect_direction_regions.xlsx contains information on the effect direction needed to replicate the importance plots by region and are based on "08_1_alldependenceplots_byspace.R".
- effect_direction_time.xlsx contains information on the effect direction needed to replicate the importance plots by time and are based on "10_1_alldependenceplots_bytime.R".
- effect_direction_fullsample.xlsx contains information on the effect direction needed to replicate the importance plot for the joint sample and are based on "11_2_alldependenceplots_fullsample.R".
- effect_direction_fullsample_differentoutcome.xlsx contains information on the effect direction needed to replicate the importance plot for the joint sample classifying only strong opposition and are based on "11_4_alldependenceplots_fullsample_differentoutcome.R".



**Software and System Requirements**

The research was conducted using Stata version 16.1 MP 64 and R version 4.3.1 for Windows.

We used the following packages in Stata: 
- egen, estout, egenmore
We used the following packages in R: 
- haven, mice, randomForestSRC, randomForest, data.tree, broom, dplyr, tidyverse, readr, stats, data.table, ggplot2,  sciplot, rworldmap, rworldxtra, extrafont, RColorBrewer, profvis, parallel, doParallel, readxl, writexl, stringr, classInt, sf, purrr

Installation guide:
- For Stata, the packages can be installed via - ssc install package_name 
- For R, the required packages can be installed via install.packages(‘package_name’) and then library(‘package_name’)
- Each package should install in less than a minute on a 4-core Intel-based laptop with Windows 11 version 22H2



**Instructions for use**

The replication package includes three folders: code, data, output. Code contains the do-files for Stata and R-Scripts necessary to clean the data and obtain the results and figures. 
1. Open the do-file “00_master.do”. At the beginning of the file, there are four global macros defined. These contain the file paths for main directory of the replication package, the raw data, the folder with the do-files, and the folder in which the output will be written. These four file paths need to be adjusted accordingly. 
2. Save the ISSP data in the ISSP2010 and ISSP2020 folders in the data folder. 
3. Run Do files and Scripts as in 00_master.do


References: 
UNU-WIDER. (2023). Unu-wider government revenue dataset (tech. rep.). UNU-WIDER.
	https://doi.org/10.35188/UNU-WIDER/GRD-2023
OECD. (2019). Taxing energy use 2019: Using taxes for climate action (tech. rep.). OECD.
	https://doi.org/10.1787/058ca239-en
Dolphin, G. (2022). Emissions-weighted carbon price: Sources and methods. Resources for
	the Future Working Paper 22-6.
Postic, S., & Fetet, M. (2020). Global carbon accounts 2020. IC4E Institute for Climate
	Economics.
European Environment Information and Observation Network. (2019). Deliveries for use of
	auctioning revenue and project credits. https://rod.eionet.europa.eu/obligations/
	698/deliveries

