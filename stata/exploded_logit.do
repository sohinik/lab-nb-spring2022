log using logit_choice.smcl, replace

clear
set more off
* change working directory
* cd "C:/Users/Huihan/Desktop/logit choice"
import delimited "choice_data.csv"
* install sequence package to generate new id variables for duplicated data sets
ssc install sequence

* generate a numerical variable for rankabc
gen rankabc_num = substr(rankabc,-1,.) 
destring rankabc_num, replace
* generate a factor variable for jobabc
encode jobabc, gen(jobabc_factor)
* rename variables for better visualization in regression tables
rename answerapplicationinfoshown_bin app_shown
rename applicationsabc_bin app_bin
rename colorabc_bin color
* generate a dummy variable whether an application is one of the first or 0-4
gen is_first = 1 if app_bin >= 0 & app_bin <= 1
replace is_first = 0 if is_first == .
* generate a dummy variable whether a job is ranked the first (used for conditional logit regressions)
gen is_chosen = 1 if rankabc_num == 1
replace is_chosen = 0 if is_chosen == .

* With original data set
cmset id_unique jobabc
* rank ordered logit choice regression
cmrologit rankabc_num wageabc commute_timeabc, reverse 
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_bin if app_shown == 1, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_shown##i.app_bin, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown##i.color, reverse
* conditional logit regression
cmclogit is_chosen wageabc commute_timeabc i.app_bin

* With duplicated data set 
* Repeat the same data set twice
expand 2
egen new_id = seq(), f(1) t(800) b(3)
cmset new_id jobabc
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_shown##i.app_bin, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown##i.color, reverse


* Repeat the same data set four times
expand 2
* generate new id variables such that all individuals are distinct in the duplicated data set
egen new_id2 = seq(), f(1) t(1600) b(3)
cmset new_id2 jobabc
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_shown##i.app_bin, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown##i.color, reverse



* Repeat the same data set eight times
expand 2
* generate new id variables such that all individuals are distinct in the duplicated data set
egen new_id3 = seq(), f(1) t(3200) b(3)
cmset new_id3 jobabc
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_shown##i.app_bin, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown##i.color, reverse



* Repeat the same data set sixteen times
expand 2
* generate new id variables such that all individuals are distinct in the duplicated data set
egen new_id4 = seq(), f(1) t(6400) b(3)
cmset new_id4 jobabc
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.app_shown##i.app_bin, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown, reverse
cmrologit rankabc_num i.jobabc_factor wageabc commute_timeabc i.is_first##i.app_shown##i.color, reverse


log close
translate logit_choice.smcl logit_choice.pdf, replace


