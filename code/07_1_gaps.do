/******************************************************************************* 	
*	Replication file: gaps													   *
********************************************************************************
*/


*** Standard settings go here
	clear all
	set more off


					
					
*********************				
*** taxes				
use "$datadir/ISSP2010/ISSP2010_clean_wcountry.dta", clear
generate year = 2010
append using "$datadir/ISSP2020/ISSP2020_clean_wcountry.dta"
replace year = 2020 if missing(year)

keep if nowtp_taxes != .


*** descriptives and data preparation for plot
bysort c_alphan year: sum nowtp_taxes


bysort c_alphan: egen m2010_nowtp_taxes = mean(nowtp_taxes) if year == 2010
bysort c_alphan: egen mean2010_nowtp_taxes = max(m2010_nowtp_taxes)

bysort c_alphan: egen m2020_nowtp_taxes = mean(nowtp_taxes) if year == 2020
bysort c_alphan: egen mean2020_nowtp_taxes = max(m2020_nowtp_taxes)

generate gap = mean2020_nowtp_taxes - mean2010_nowtp_taxes // negative values will mean a decrease

duplicates drop c_alphan, force 


bysort c_alphan: sum mean2010_nowtp_taxes mean2020_nowtp_taxes

keep if c_alphan == "AT" | c_alphan == "AU" | c_alphan == "CH" | c_alphan == "DE" | c_alphan == "DK" | c_alphan == "ES" | c_alphan == "FI" | c_alphan == "FR" | c_alphan == "HR" | c_alphan == "IS" | c_alphan == "JP" | c_alphan == "KR" | c_alphan == "LT" | c_alphan == "NO" | c_alphan == "NZ" | c_alphan == "SE" | c_alphan == "SI" | c_alphan == "SK" | c_alphan == "TW" | c_alphan == "US"


sum mean2010_nowtp_taxes  // 51.9%
sum mean2020_nowtp_taxes  // 47.9%


tab c_alphan gap 

save "$datadir/gaps.dta", replace


