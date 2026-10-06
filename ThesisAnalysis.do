* =========================================================================
* Academic Stress and Suicide Risk Among South Korean Adolescents
* Pakeeza Abid
* Master of Public Policy thesis, KDI School of Public Policy and Management
* =========================================================================
*
* Data: mortality microdata, 1997-2023, Microdata Integrated Service (MDIS)
*   merged_data_translatedvnames.dta   all deaths
*   suicide_only_data.dta              suicide deaths
*
* Contents
*   Part 1  Data preparation
*   Part 2  Table 1: Summary statistics
*   Part 3  Figures 1-2: Annual and monthly suicide deaths
*   Part 4  Table 2: Suicide deaths by occupation
*   Part 5  Figures 5-6: Daily suicide pattern across the year
*   Part 6  Table 3: Difference-in-differences regressions
*   Part 7  Figure 7: Event study around March 2
*   Part 8  Figure 8: Day-of-week pattern among students
*
* Treatment: occupation code 13, age 11-19. Control: all others.
* Post: on or after March 2. Window: February 20 to March 12.
* =========================================================================

* -------------------------------------------------------------------------
* Packages
* -------------------------------------------------------------------------

ssc install reghdfe

ssc install ftools

ssc install estout

ssc install regsave

ssc install asdoc

* -------------------------------------------------------------------------
* PART 1. Data preparation
* -------------------------------------------------------------------------

use "merged_data_translatedvnames.dta", clear

tabulate cause1_code if cause104_code == 102
tabulate cause1_code if cause57_code == 55

* Create binary suicide indicator from cause104_code and cause57_code
gen suicide = 0
replace suicide = 1 if cause104_code == 102 | cause57_code == 55

* Label the new variable
label variable suicide "Death by suicide (constructed from cause104_code and cause57_code)"

* Tabulate to confirm creation
tabulate suicide

* Keep only suicide cases in the dataset
keep if suicide == 1

* Save this filtered dataset with a new name to keep your original intact
save "suicide_only_data.dta", replace

save "/Users/pakeeza/Documents/Thesis_Alldatafiles/suicide_only_data.dta", replace

* -------------------------------------------------------------------------
* PART 2. Table 1: Summary statistics
* -------------------------------------------------------------------------

use "suicide_only_data.dta", clear

gen female = sex == 2

tab female
gen married = marital_status == 1
tab married

gen student_unemployed = occupation_code == 13

tab student_unemployed

tab age if age==999

replace age = . if age == 999

estpost tabstat age female married student_unemployed report_year, ///
statistics(count mean sd) columns(statistics)

esttab using summary_table.tex, ///
cells("count mean sd") ///
label ///
nonumber ///
replace

esttab using summary_table.tex, ///
cells("count(fmt(0)) mean(fmt(3)) sd(fmt(3))") ///
label ///
nonumber ///
noobs ///
unstack ///
replace

* -------------------------------------------------------------------------
* PART 3. Figures 1-2: Annual and monthly suicide deaths
* -------------------------------------------------------------------------

************************************************************
* Figure 1. Annual suicide deaths (1997-2023)
************************************************************
use "suicide_only_data.dta", clear

collapse (count) suicide, by(year)

twoway ///
(line suicide year, lcolor(gray) lwidth(medthick)), ///
xtitle("Year", size(small)) ///
ytitle("Number of Suicide Deaths", size(small)) ///
xlabel(1997(2)2023, labsize(small) nogrid) ///
ylabel(, labsize(small) angle(horizontal) nogrid) ///
scheme(s2color) ///
graphregion(color(white)) ///
plotregion(color(white))

graph export suicide_trend_Annual.pdf, replace

************************************************************
* Figure 2. Average suicide deaths by month
************************************************************
use "/Users/pakeeza/Documents/Thesis_Alldatafiles/merged_data_translatedvnames copy.dta", clear

keep if cause57_code == 55
 
* gen suicide = 1
gen suicide = cause57_code == 55

tostring death_date, replace

drop year
gen year = substr(death_date, 1, 4)
gen month = substr(death_date, 5, 2)
gen day = substr(death_date, -2, 2)

foreach v of var year month day {
	destring `v', replace
}

* collapse to monthly totals within each year
collapse (sum) suicide, by(year month)

* compute average across years
collapse (mean) suicide, by(month)

two (connected suicide month)

* -------------------------------------------------------------------------
* PART 4. Table 2: Suicide deaths by occupation
* -------------------------------------------------------------------------

use "suicide_only_data.dta", clear

tab occupation_code

asdoc tab occupation_code, replace

* -------------------------------------------------------------------------
* PART 5. Figures 5-6: Daily suicide pattern across the year
* -------------------------------------------------------------------------

************************************************************
* Figure 5. All individuals
************************************************************
use suicide_only_data, clear
drop report_*

* --- Extract year, month, day ---
gen str8 death_str = string(death_date, "%08.0f")
gen death_year  = real(substr(death_str,1,4))
gen death_month = real(substr(death_str,5,2))
gen death_day   = real(substr(death_str,7,2))
drop death_str

* Keep 1997–2023
keep if death_year >= 1997 & death_year <= 2023

* Collapse suicides by month & day
collapse (sum) suicide, by(death_month death_day)

* Drop impossible days
drop if death_day > 28 & death_month == 2
drop if death_day > 30 & inlist(death_month,4,6,9,11)

* ---- Create continuous day-of-year (DOY) ----
gen doy = mdy(death_month, death_day, 2000) - mdy(1,1,2000) + 1

sort doy

	* ---- One single graph with dotted vertical lines at month starts ----
twoway (line suicide doy, lcolor(black) lwidth(medthick)) ///
       , xline(1 32 60 91 121 152 182 213 244 274 305 335, lpattern(dash) lcolor(gs12)) ///
         xtitle("Months") ///
         ytitle("Number of Suicides") ///
         title("Daily Suicide Pattern (1997–2023)") ///
         xlabel(1 "Jan" 32 "Feb" 60 "Mar" 91 "Apr" 121 "May" 152 "Jun" ///
                182 "Jul" 213 "Aug" 244 "Sep" 274 "Oct" 305 "Nov" 335 "Dec") ///
         scheme(s1color) graphregion(color(white)) plotregion(color(white))

************************************************************
* Figure 6. Students
************************************************************
use suicide_only_data, clear
drop report_*

* --- Extract year, month, day ---
gen str8 death_str = string(death_date, "%08.0f")
gen death_year  = real(substr(death_str,1,4))
gen death_month = real(substr(death_str,5,2))
gen death_day   = real(substr(death_str,7,2))
drop death_str

* Keep 1997–2023
keep if death_year >= 1997 & death_year <= 2023

* --- Filter for age <= 18 and occupation_code = 13 ---
keep if age <= 19 & occupation_code == 13

* Collapse suicides by month & day
collapse (sum) suicide, by(death_month death_day)

* Drop impossible days
drop if death_day > 28 & death_month == 2
drop if death_day > 30 & inlist(death_month,4,6,9,11)

* ---- Create continuous day-of-year (DOY) ----
gen doy = mdy(death_month, death_day, 2000) - mdy(1,1,2000) + 1

sort doy

	* ---- One single graph with dotted vertical lines at month starts ----
twoway (line suicide doy, lcolor(black) lwidth(medthick)) ///
       , xline(1 32 60 91 121 152 182 213 244 274 305 335, lpattern(dash) lcolor(gs12)) ///
         xtitle("Months") ///
         ytitle("Number of Suicides") ///
         title("Daily Suicide Pattern- Students (1997–2023)") ///
         xlabel(1 "Jan" 32 "Feb" 60 "Mar" 91 "Apr" 121 "May" 152 "Jun" ///
                182 "Jul" 213 "Aug" 244 "Sep" 274 "Oct" 305 "Nov" 335 "Dec") ///
         scheme(s1color) graphregion(color(white)) plotregion(color(white))

graph export "yearly_suicide_trend_under18_occ13.png", replace width(2400)

* -------------------------------------------------------------------------
* PART 6. Table 3: Difference-in-differences regressions
* -------------------------------------------------------------------------

use "merged_data_translatedvnames.dta", clear

gen suicide = 0
replace suicide = 1 if cause104_code == 102 & cause57_code == 55

tab suicide
tab cause104_code cause57_code if suicide == 1

gen treat = 0
replace treat = 1 if occupation_code == 13 & age >= 11 & age <= 19

tostring death_date, gen(date_str)
gen date = date(date_str,"YMD")
format date %td

list death_date date in 1/10

drop year

gen year  = year(date)
gen month = month(date)
gen day   = day(date)

tab month

gen window = 0
replace window = 1 if month == 2 & day >= 20
replace window = 1 if month == 3 & day <= 12
keep if window == 1

gen post = 0
replace post = 1 if month == 3 & day >= 2

gen treatXpost = treat*post

eststo clear

eststo m1: reghdfe suicide treatXpost, ///
a(addr_region_code year) vce(cluster addr_region_code age)

eststo m2: reghdfe suicide treatXpost, ///
a(addr_region_code occupation_code year) vce(cluster addr_region_code age)

eststo m3: reghdfe suicide treatXpost, ///
a(addr_region_code occupation_code year month) vce(cluster addr_region_code age)

eststo m4: reghdfe suicide treatXpost, ///
a(addr_region_code occupation_code year#month) vce(cluster addr_region_code age)

estadd local region_fe "Yes": m1
estadd local occupation_fe "No": m1
estadd local year_fe "Yes": m1
estadd local month_fe "No": m1

estadd local region_fe "Yes": m2
estadd local occupation_fe "Yes": m2
estadd local year_fe "Yes": m2
estadd local month_fe "No": m2

estadd local region_fe "Yes": m3
estadd local occupation_fe "Yes": m3
estadd local year_fe "Yes": m3
estadd local month_fe "Yes": m3

estadd local region_fe "Yes": m4
estadd local occupation_fe "Yes": m4
estadd local year_fe "Yes": m4
estadd local month_fe "Yes": m4

esttab m1 m2 m3 m4 using regression_results.tex, ///
replace se label ///
star(* 0.10 ** 0.05 *** 0.01) ///
mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
stats(region_fe occupation_fe year_fe month_fe N, ///
labels("Region FE" "Occupation FE" "Year FE" "Month FE" "Observations")) ///
compress

* -------------------------------------------------------------------------
* PART 7. Figure 7: Event study around March 2
* -------------------------------------------------------------------------

use "merged_data_translatedvnames.dta", clear

gen suicide = 0
replace suicide = 1 if cause104_code == 102 & cause57_code == 55

tab suicide
tab cause104_code cause57_code if suicide == 1

tostring death_date, gen(date_str)
gen date = date(date_str,"YMD")
format date %td

list death_date date in 1/10

drop year

gen year  = year(date)
gen month = month(date)
gen day   = day(date)

tab month

gen window = 0
replace window = 1 if month == 2 & day >= 20
replace window = 1 if month == 3 & day <= 12
keep if window == 1

gen treat = 0
replace treat = 1 if occupation_code == 13 & age >= 11 & age <= 19

gen post = 0
replace post = 1 if month == 3 & day >= 2

* define event date (March 2 each year)
gen event_date = mdy(3,2,year)

* days relative to the event
gen event_time = date - event_date

* keep event window (about ±10 days)
keep if event_time >= -10 & event_time <= 10

* shift so values are positive (needed for factor variables)
gen event_time_shift = event_time + 10

reghdfe suicide i.treat##ib9.event_time_shift, ///
a(addr_region_code occupation_code year#month) ///
vce(cluster addr_region_code age)

regsave using event_results, ci level(95) replace

use event_results, clear

drop if strpos(var,"0b.treat")
drop if var=="_cons"

gen time = real(regexs(1)) if regexm(var,"#([0-9]+)\.")
replace time = time - 10

sort time

drop if time == -1

twoway ///
(rcap ci_lower ci_upper time, lc(black)) ///
(scatter coef time, mc(black) msize(small)), ///
yline(0, lpattern(dash)) ///
xline(0, lpattern(dash) lcolor(gs8)) ///
xtitle("Days Relative to Start of Academic Year (March 2)") ///
ytitle("Effect on Suicide Incidence") ///
xlabel(-10 -8 -6 -4 -2 0 2 4 6 8 10) ///
legend(off) ///
graphregion(color(white))

* -------------------------------------------------------------------------
* PART 8. Figure 8: Day-of-week pattern among students
* -------------------------------------------------------------------------

use suicide_only_data, clear

gen str8 death_str = string(death_date, "%08.0f")

gen death_year  = real(substr(death_str, 1, 4))
gen death_month = real(substr(death_str, 5, 2))
gen death_day   = real(substr(death_str, 7, 2))

gen death_date1 = mdy(death_month, death_day, death_year)
format death_date1 %td

* --- Create day-of-week variable (1=Monday, ..., 7=Sunday) ---
gen death_dow = mod(dow(death_date1) + 6, 7) + 1
label define dow_lbl 1 "Monday" 2 "Tuesday" 3 "Wednesday" 4 "Thursday" 5 "Friday" 6 "Saturday" 7 "Sunday"
label values death_dow dow_lbl

keep if occupation_code == 13
keep if age>=11 & age<=19
keep if death_year >= 1997 & death_year <= 2023
collapse (sum) suicide, by(death_dow occupation_code)

line suicide death_dow if occupation_code==13, ///
xlabel(1 "Mon" 2 "Tue" 3 "Wed" 4 "Thu" 5 "Fri" 6 "Sat" 7 "Sun", labsize(vsmall) nogrid) ///
ylabel(, labsize(vsmall) angle(horizontal) nogrid) ///
xtitle("Day of Week", size(vsmall)) ///
ytitle("Number of Suicide Deaths", size(vsmall)) ///
scheme(plotplain) ///
graphregion(color(white)) ///
plotregion(color(white))

graph export weekly_trend_students.pdf, replace
