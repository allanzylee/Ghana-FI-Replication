global root "`c(pwd)'"
global data "$root/data/raw"
global derived "$root/data/derived"

use "$data/Enrolment & Caregiver Survey_depii.dta", clear

keep hr13 careid

merge 1:m careid using "$data/02_PNP_Midline_CaregiverSurvey.dta"

keep if _merge==3

drop _merge

sum ps*
tab region

encode region, gen (Region)

gen ps1_region = .

replace ps1_region = 8 if Region < 4

replace ps1_region = 9 if Region == 4

replace ps1_region = 10 if Region == 5

label define ps1_hh_region_lbl                                         ///
    1  "Western"                                                       ///
    2  "Central"                                                       ///
    3  "Greater Accra"                                                 ///
    4  "Volta"                                                         ///
    5  "Eastern"                                                       ///
    6  "Ashanti"                                                       ///
    7  "Brong Ahafo"                                                   ///
    8  "Northern"                                                      ///
    9  "Upper East"                                                    ///
    10 "Upper West"

label values ps1_region ps1_hh_region_lbl
label var ps1_region "Region of household (10-region PPI classification)"

tab ps1_region

gen ps1_hh_region = .
replace ps1_hh_region = 17 if ps1_region == 1
replace ps1_hh_region = 25 if ps1_region == 2
replace ps1_hh_region = 25 if ps1_region == 3
replace ps1_hh_region = 10 if ps1_region == 4
replace ps1_hh_region = 29 if ps1_region == 5
replace ps1_hh_region = 16 if ps1_region == 6
replace ps1_hh_region = 14 if ps1_region == 7
replace ps1_hh_region =  1 if ps1_region == 8
replace ps1_hh_region =  9 if ps1_region == 9
replace ps1_hh_region =  0 if ps1_region == 10
label var ps1_hh_region "PPI points: Region (Indicator 1)"

tab ps2

sort careid

gen tot_hh_member = hr13 + 1 + ps2

label var tot_hh_member "Reconstructed total household size"

gen ps2_new = 1 if tot_hh_member < 5
replace ps2_new = 2 if tot_hh_member > 4 & tot_hh_member < 8
replace ps2_new = 3 if tot_hh_member > 7
label var ps2_new "Household size bucket (1=1-4, 2=5-7, 3=8+)"

gen ps2_hh = .
replace ps2_hh = 24 if ps2_new == 1
replace ps2_hh =  5 if ps2_new == 2
replace ps2_hh =  0 if ps2_new == 3
label var ps2_hh "PPI points: Household size (Indicator 2)"

tab ps3
gen ps3_eggs = .
replace ps3_eggs = 4 if ps3 == 1
replace ps3_eggs = 0 if ps3 == 0
label var ps3_eggs "PPI points: Purchased eggs in past month (Indicator 3)"

tab ps4
gen ps4_beef = .
replace ps4_beef = 9 if ps4 == 1
replace ps4_beef = 0 if ps4 == 0
label var ps4_beef "PPI points: Purchased beef in past month (Indicator 4)"

tab ps5
gen ps5_wall = .
replace ps5_wall = 0 if ps5 == 1
replace ps5_wall = 0 if ps5 == 0
label var ps5_wall "PPI points: Outer wall material (Indicator 5) - always 0"

tab ps6
gen ps6_fuel = .
replace ps6_fuel = 0 if ps6 == 1
replace ps6_fuel = 9 if ps6 == 2
label var ps6_fuel "PPI points: Main cooking fuel (Indicator 6)"

tab ps7
gen ps7_stove = .
replace ps7_stove = 14 if ps7 == 1
replace ps7_stove =  0 if ps7 == 0
label var ps7_stove "PPI points: Owns a gas stove (Indicator 7)"

tab ps8
gen ps8_fridge = .
replace ps8_fridge = 7 if ps8 == 1
replace ps8_fridge = 0 if ps8 == 0
label var ps8_fridge "PPI points: Owns a refrigerator (Indicator 8)"

tab ps9
gen ps9_fan = .
replace ps9_fan = 2 if ps9 == 1
replace ps9_fan = 0 if ps9 == 0
label var ps9_fan "PPI points: Owns a fan (Indicator 9)"

tab ps10
gen ps10_tv = .
replace ps10_tv = 3 if ps10 == 1
replace ps10_tv = 0 if ps10 == 0
label var ps10_tv "PPI points: Owns a television (Indicator 10)"

egen wealth_bl = rowtotal(ps1_hh_region ps2_hh ps3_eggs ps4_beef ps5_wall ///
    ps6_fuel ps7_stove ps8_fridge ps9_fan ps10_tv) , missing
label var wealth_bl "Caregiver-reported HH wealth / PPI score (0-100) - LEAD Midline"

tab wealth_bl

gen ppi_poverty_prob = .
label var ppi_poverty_prob "PPI Poverty Probability (% likelihood below the National Poverty Line)"

replace ppi_poverty_prob = 89.7 if wealth_bl == 0
replace ppi_poverty_prob = 88.9 if wealth_bl == 1
replace ppi_poverty_prob = 88.0 if wealth_bl == 2
replace ppi_poverty_prob = 87.1 if wealth_bl == 3
replace ppi_poverty_prob = 86.1 if wealth_bl == 4
replace ppi_poverty_prob = 85.0 if wealth_bl == 5
replace ppi_poverty_prob = 83.9 if wealth_bl == 6
replace ppi_poverty_prob = 82.8 if wealth_bl == 7
replace ppi_poverty_prob = 81.5 if wealth_bl == 8
replace ppi_poverty_prob = 80.2 if wealth_bl == 9
replace ppi_poverty_prob = 78.9 if wealth_bl == 10
replace ppi_poverty_prob = 77.4 if wealth_bl == 11
replace ppi_poverty_prob = 75.9 if wealth_bl == 12
replace ppi_poverty_prob = 74.3 if wealth_bl == 13
replace ppi_poverty_prob = 72.7 if wealth_bl == 14
replace ppi_poverty_prob = 71.0 if wealth_bl == 15
replace ppi_poverty_prob = 69.2 if wealth_bl == 16
replace ppi_poverty_prob = 67.4 if wealth_bl == 17
replace ppi_poverty_prob = 65.5 if wealth_bl == 18
replace ppi_poverty_prob = 63.6 if wealth_bl == 19
replace ppi_poverty_prob = 61.6 if wealth_bl == 20
replace ppi_poverty_prob = 59.6 if wealth_bl == 21
replace ppi_poverty_prob = 57.5 if wealth_bl == 22
replace ppi_poverty_prob = 55.4 if wealth_bl == 23
replace ppi_poverty_prob = 53.3 if wealth_bl == 24
replace ppi_poverty_prob = 51.2 if wealth_bl == 25
replace ppi_poverty_prob = 49.1 if wealth_bl == 26
replace ppi_poverty_prob = 47.0 if wealth_bl == 27
replace ppi_poverty_prob = 44.9 if wealth_bl == 28
replace ppi_poverty_prob = 42.8 if wealth_bl == 29
replace ppi_poverty_prob = 40.8 if wealth_bl == 30
replace ppi_poverty_prob = 38.8 if wealth_bl == 31
replace ppi_poverty_prob = 36.8 if wealth_bl == 32
replace ppi_poverty_prob = 34.8 if wealth_bl == 33
replace ppi_poverty_prob = 32.9 if wealth_bl == 34
replace ppi_poverty_prob = 31.1 if wealth_bl == 35
replace ppi_poverty_prob = 29.3 if wealth_bl == 36
replace ppi_poverty_prob = 27.6 if wealth_bl == 37
replace ppi_poverty_prob = 25.9 if wealth_bl == 38
replace ppi_poverty_prob = 24.4 if wealth_bl == 39
replace ppi_poverty_prob = 22.8 if wealth_bl == 40
replace ppi_poverty_prob = 21.4 if wealth_bl == 41
replace ppi_poverty_prob = 20.0 if wealth_bl == 42
replace ppi_poverty_prob = 18.7 if wealth_bl == 43
replace ppi_poverty_prob = 17.4 if wealth_bl == 44
replace ppi_poverty_prob = 16.2 if wealth_bl == 45
replace ppi_poverty_prob = 15.1 if wealth_bl == 46
replace ppi_poverty_prob = 14.1 if wealth_bl == 47
replace ppi_poverty_prob = 13.1 if wealth_bl == 48
replace ppi_poverty_prob = 12.2 if wealth_bl == 49
replace ppi_poverty_prob = 11.3 if wealth_bl == 50
replace ppi_poverty_prob = 10.5 if wealth_bl == 51
replace ppi_poverty_prob =  9.7 if wealth_bl == 52
replace ppi_poverty_prob =  9.0 if wealth_bl == 53
replace ppi_poverty_prob =  8.3 if wealth_bl == 54
replace ppi_poverty_prob =  7.7 if wealth_bl == 55
replace ppi_poverty_prob =  7.1 if wealth_bl == 56
replace ppi_poverty_prob =  6.6 if wealth_bl == 57
replace ppi_poverty_prob =  6.1 if wealth_bl == 58
replace ppi_poverty_prob =  5.6 if wealth_bl == 59
replace ppi_poverty_prob =  5.2 if wealth_bl == 60
replace ppi_poverty_prob =  4.8 if wealth_bl == 61
replace ppi_poverty_prob =  4.4 if wealth_bl == 62
replace ppi_poverty_prob =  4.1 if wealth_bl == 63
replace ppi_poverty_prob =  3.8 if wealth_bl == 64
replace ppi_poverty_prob =  3.5 if wealth_bl == 65
replace ppi_poverty_prob =  3.2 if wealth_bl == 66
replace ppi_poverty_prob =  2.9 if wealth_bl == 67
replace ppi_poverty_prob =  2.7 if wealth_bl == 68
replace ppi_poverty_prob =  2.5 if wealth_bl == 69
replace ppi_poverty_prob =  2.3 if wealth_bl == 70
replace ppi_poverty_prob =  2.1 if wealth_bl == 71
replace ppi_poverty_prob =  1.9 if wealth_bl == 72
replace ppi_poverty_prob =  1.8 if wealth_bl == 73
replace ppi_poverty_prob =  1.6 if wealth_bl == 74
replace ppi_poverty_prob =  1.5 if wealth_bl == 75
replace ppi_poverty_prob =  1.4 if wealth_bl == 76
replace ppi_poverty_prob =  1.3 if wealth_bl == 77
replace ppi_poverty_prob =  1.2 if wealth_bl == 78
replace ppi_poverty_prob =  1.1 if wealth_bl == 79
replace ppi_poverty_prob =  1.0 if wealth_bl == 80
replace ppi_poverty_prob =  0.9 if wealth_bl == 81
replace ppi_poverty_prob =  0.8 if wealth_bl == 82
replace ppi_poverty_prob =  0.8 if wealth_bl == 83
replace ppi_poverty_prob =  0.7 if wealth_bl == 84
replace ppi_poverty_prob =  0.7 if wealth_bl == 85
replace ppi_poverty_prob =  0.6 if wealth_bl == 86
replace ppi_poverty_prob =  0.6 if wealth_bl == 87
replace ppi_poverty_prob =  0.5 if wealth_bl == 88
replace ppi_poverty_prob =  0.5 if wealth_bl == 89
replace ppi_poverty_prob =  0.4 if wealth_bl == 90
replace ppi_poverty_prob =  0.4 if wealth_bl == 91
replace ppi_poverty_prob =  0.4 if wealth_bl == 92
replace ppi_poverty_prob =  0.3 if wealth_bl == 93
replace ppi_poverty_prob =  0.3 if wealth_bl == 94
replace ppi_poverty_prob =  0.3 if wealth_bl == 95
replace ppi_poverty_prob =  0.3 if wealth_bl == 96
replace ppi_poverty_prob =  0.2 if wealth_bl == 97
replace ppi_poverty_prob =  0.2 if wealth_bl == 98
replace ppi_poverty_prob =  0.2 if wealth_bl == 99

replace ppi_poverty_prob = 0.2 if wealth_bl >= 100 & wealth_bl < .

tabstat ppi_poverty_prob, by(wealth_bl) stat(mean) format(%9.1f)

summarize ppi_poverty_prob , detail

keep childid careid ppi_poverty_prob

save "$derived/poverty.dta", replace
