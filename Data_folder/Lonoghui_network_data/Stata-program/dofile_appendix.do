/*===============================================================================

                            Appendix
							
Description: Analyze the experimental data collected in Longhui. This code is 
only used to produce tables in the appendix.

*==============================================================================*/
clear all
cd "d:\program files\stata16\ado\personal\aej2021"

*=======================Table A1================================================
use final, clear

//column 1
local var ="chn1 math1 gender age height weight hukou_rd1 nationality_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car neur1 extra1 open1 agree1 cons1"

foreach v of local var{
	qui sum `v' if noncomplier==1
	local m = r(mean) 
	disp %6.2f `m' 
	qui sum `v' if noncomplier==1
	local sd =r(sd)
	disp "(" %6.2f `sd' ")"

}

//column 2
local var ="chn1 math1 gender age height weight hukou_rd1 nationality_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car neur1 extra1 open1 agree1 cons1"

foreach v of local var{
	qui sum `v' if noncomplier==1 & real_non!=0 & real_non2!=0
	local m = r(mean) 
	disp %6.2f `m' 
	qui sum `v' if noncomplier==1 & real_non!=0 & real_non2!=0
	local sd =r(sd)
	disp "(" %6.2f `sd' ")"

}

//column 3
local var ="chn1 math1 gender age height weight hukou_rd1 nationality_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car neur1 extra1 open1 agree1 cons1"

foreach v of local var{
	qui sum `v' if real_non2==1
	local m = r(mean) 
	disp %6.2f `m' 
	qui sum `v' if real_non2==1
	local sd =r(sd)
	disp "(" %6.2f `sd' ")"

}

//column 4
local var ="chn1 math1 gender age height weight hukou_rd1 nationality_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car neur1 extra1 open1 agree1 cons1"

foreach v of local var{
	qui sum `v' if noncomplier==1 & real_non!=1 & real_non2!=1
	local m = r(mean) 
	disp %6.2f `m' 
	qui sum `v' if noncomplier==1 & real_non!=1 & real_non2!=1
	local sd =r(sd)
	disp "(" %6.2f `sd' ")"

}

//column 5
local var ="chn1 math1 gender age height weight hukou_rd1 nationality_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car neur1 extra1 open1 agree1 cons1"

foreach v of local var{
	qui sum `v' if noncomplier!=1
	local m = r(mean) 
	disp %6.2f `m' 
	qui sum `v' if noncomplier!=1
	local sd =r(sd)
	disp "(" %6.2f `sd' ")"

}

*=======================Table A2================================================

use final0, clear

local var ="chn1 math1 gender age height health nationality_rd1  hukou_rd1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui reg `v'  treat1 if treat2!=1 & att1==1, cluster(class1)

	local b = _b[treat1]
	local s = _se[treat1]
	
	qui reg `v'  treat2 if treat1!=1 & att1==1, cluster(class1)
	local b2 = _b[treat2]
	local s2 = _se[treat2]
	
	qui sum `v' if control==1 & att1==1
	local m = r(mean) 
	disp %6.2f `m' "," %6.2f `b' "," "["%6.2f `s' "]" "," %6.2f `b2' "," "["%6.2f `s2' "]"

}

reg att1 treat1 treat2 if att2!=1, cluster(class1)
est store m1
reg att1 treat1 treat2 if hsco==0 & att2!=1, cluster(class1)
est store m2
reg att1 treat1 treat2 if hsco==1 & att2!=1, cluster(class1)
est store m3

reg att2 treat1 treat2 if att1!=1, cluster(class1)
est store m4
reg att2 treat1 treat2 if hsco==0 & att1!=1, cluster(class1)
est store m5
reg att2 treat1 treat2 if hsco==1 & att1!=1, cluster(class1)
est store m6

reg attrition treat1 treat2, cluster(class1)
est store m7
reg attrition treat1 treat2 if hsco==0, cluster(class1)
est store m8
reg attrition treat1 treat2 if hsco==1, cluster(class1)
est store m9

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8 m9] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


*==========================Table A3=============================================
use final, clear

// Columns 1
keep if control==1  //select treat2, treat1 or control

sort desk hsco

bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 health sib interest_chn1 interest_math1 fa_eduy mo_eduy pc car"

keep class1 psid `xvar' desk hsco

pairdata `xvar' desk hsco, fam(class1) ind(psid)

use single, clear

gen desk = (desk_1 == desk_2)

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local xvar{
	gen d`v' = abs(`v'_2 - `v'_1)
}

reg desk dpctil-dcar i.class, cluster(class1)
est store m1

outreg2 [m1] using ols.rtf, replace word sdec(6) bdec(6) br drop(cls*) see title(RESULT)


// Columns 2
use final, clear
keep if treat1==1  //select treat2, treat1 or control

sort desk hsco

bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 health sib interest_chn1 interest_math1 fa_eduy mo_eduy pc car"

keep class1 psid `xvar' desk hsco

pairdata `xvar' desk hsco, fam(class1) ind(psid)

use single, clear

gen desk = (desk_1 == desk_2)

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local xvar{
	gen d`v' = abs(`v'_2 - `v'_1)
}

reg desk dpctil-dcar i.class, cluster(class1)
est store m1

outreg2 [m1] using ols.rtf, replace word sdec(6) bdec(6) br drop(cls*) see title(RESULT)

// Columns 3
use final, clear
keep if treat2==1  //select treat2, treat1 or control

sort desk hsco

bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 health sib interest_chn1 interest_math1 fa_eduy mo_eduy pc car"

keep class1 psid `xvar' desk hsco

pairdata `xvar' desk hsco, fam(class1) ind(psid)

use single, clear

gen desk = (desk_1 == desk_2)

local xvar = "pctil gender age  height weight nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local xvar{
	gen d`v' = abs(`v'_2 - `v'_1)
}

reg desk dpctil-dcar i.class, cluster(class1)
est store m1

outreg2 [m1] using ols.rtf, replace word sdec(6) bdec(6) br drop(cls*) see title(RESULT)

*==========================Table A4=============================================
use final, clear

gen lsco = 1 - hsco

bys class1: egen p25 = pctile(ave1), p(25)
bys class1: egen p50 = pctile(ave1), p(50)
bys class1: egen p75 = pctile(ave1), p(75)

gen p1 = (ave1<=p25)
gen p2 = (ave1>p25 & ave1<=p50)
gen p3 = (ave1>p50 & ave1<=p75)
gen p4 = (ave1>=p75)

gen lowt1 = treat1*lsco
gen lowt2 = treat2*lsco

gen hit1 = treat1*hsco
gen hit2 = treat2*hsco

gen pt11 = treat1*p1
gen pt12 = treat1*p2
gen pt13 = treat1*p3
gen pt14 = treat1*p4

gen pt21 = treat2*p1
gen pt22 = treat2*p2
gen pt23 = treat2*p3
gen pt24 = treat2*p4

local xvar2 ="pt11 pt12 pt13 pt14 pt21 pt22 pt23 pt24  gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg ave3 ave1 `xvar2', cluster(class1)
est store m1 
reg tchn3 tchn1 `xvar2' , cluster(class1)
est store m2
reg tmath3 tmath1 `xvar2' , cluster(class1)
est store m3

outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*==========================Table A5=============================================
use final, clear

//Deskmates are top10 in the classes: columns 1-3
bys class1: egen rk1 = rank(adesk1), f		//deskmate top
bys class1: egen rk2 = rank(ave1), t	//bottom

gen top10 = (rk1<=10)
gen bot10 = (rk2<=10)

gen top_treat1 = top10*treat1
gen top_treat2 = top10*treat2

gen bot_treat1 = bot10*treat1
gen bot_treat2 = bot10*treat2

local xvar1 ="treat1 treat2 top_treat1 top_treat2 top10 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

qui reg ave3 ave1  `xvar1' if hsco==0, cluster(class1)
est store m1

qui reg tchn3 tchn1  `xvar1' if hsco==0, cluster(class1)
est store m2

qui reg tmath3 tmath1 `xvar1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Baseline score gaps: columns 4-6
egen dif_ave1 = rowmean(dif_chn1 dif_math1)
keep if hsco==0

bys class1: egen dif_ave1_m = mean(dif_ave1)
gen AD_`v' = (dif_ave1>dif_ave1_m)

gen xx1 = dif_ave1*treat1
gen xx2 = dif_ave1*treat2

local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

qui reg ave3 ave1 dif_ave1 xx1 xx2 `xvar1' if hsco==0, cluster(class1)
est store m1

qui reg tchn3 tchn1 dif_chn1 xx1 xx2 `xvar1' if hsco==0, cluster(class1)
est store m2

qui reg tmath3 tmath1 dif_math1 xx1 xx2 `xvar1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*==========================Table A6=============================================
use final, clear

//non-cognitive abilies interacted with baseline personality traits (by median)
local var ="extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	qui sum `v', d
	local p50 = r(p50)
	disp `p50'
	gen l_`v' = (`v'<`p50')

}
// Panel A
local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local lo{
	gen int1_`v' = l_`v'*treat1
	gen int2_`v' = l_`v'*treat2
	reg `v'2 `v'1 int1_`v' int2_`v' l_`v' `var1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

// Panel B
use final, clear

local var ="extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	qui sum `v', d
	local p50 = r(p50)
	disp `p50'
	gen l_`v' = (`v'<`p50')

}
local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local lo{
	gen int1_`v' = l_`v'*treat1
	gen int2_`v' = l_`v'*treat2
	reg `v'2 `v'1 int1_`v' int2_`v' l_`v' `var1' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*==========================Table A7=============================================
use final, clear

local lo1 = "extra1 agree1 open1 neur1 cons1"
foreach v of local lo1{
	qui sum `v'
	local m=r(mean)
	gen `v'_high = (`v'>=`m')
	gen `v'_low = (`v'<`m')
	
	gen treat2_`v'_high = treat2* `v'_high
	gen treat2_`v'_low = treat2* `v'_low

}

local lo_high = "extra1_high agree1_high open1_high neur1_high cons1_high"
local var1 = "treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg  ave3 ave1  treat2_*_high `lo_high' `var1' if hsco==0, cluster(class1)
est store m1
reg  tchn3 tchn1 treat2_*_high `lo_high' `var1' if hsco==0, cluster(class1)
est store m2
reg  tmath3 tmath1 treat2_*_high `lo_high' `var1' if hsco==0, cluster(class1)
est store m3

reg  ave3 ave1  treat2_*_high `lo_high' `var1' if hsco==1, cluster(class1)
est store m4
reg  tchn3 tchn1 treat2_*_high `lo_high' `var1' if hsco==1, cluster(class1)
est store m5
reg  tmath3 tmath1 treat2_*_high `lo_high' `var1' if hsco==1, cluster(class1)
est store m6

outreg2 [m1 m2 m3 m4 m5 m6] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*==========================Table A8=============================================
use final, clear

gen l_edu = (fa_eduy<=9 & mo_eduy<=9)

gen edu_treat1 = l_edu*treat1
gen edu_treat2 = l_edu*treat2

// Panel A
local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
local j=4
foreach v of local lo{
	reg `v'2 `v'1 edu_treat1 edu_treat2 l_edu `var1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg  ave3 ave1 edu_treat1 edu_treat2 l_edu `var1' if hsco==0, cluster(class1)
est store m1
reg  tchn3 tchn1 edu_treat1 edu_treat2 l_edu `var1' if hsco==0, cluster(class1)
est store m2
reg  tmath3 tmath1 edu_treat1 edu_treat2 l_edu `var1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

// Panel B
local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
local j=4
foreach v of local lo{
	reg `v'2 `v'1 edu_treat1 edu_treat2 l_edu `var1' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg  ave3 ave1 edu_treat1 edu_treat2 l_edu `var1' if hsco==1, cluster(class1)
est store m1
reg  tchn3 tchn1 edu_treat1 edu_treat2 l_edu `var1' if hsco==1, cluster(class1)
est store m2
reg  tmath3 tmath1 edu_treat1 edu_treat2 l_edu `var1' if hsco==1, cluster(class1)
est store m3

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*==========================Table A9=============================================
use final, clear

//Panel A
gen l_inc = (car==1 & pc==1)

gen edu_treat1 = l_inc*treat1
gen edu_treat2 = l_inc*treat2

local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
local j=4
foreach v of local lo{
	reg `v'2 `v'1 edu_treat1 edu_treat2 l_inc `var1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg  ave3 ave1 edu_treat1 edu_treat2 l_inc `var1' if hsco==0, cluster(class1)
est store m1
reg  tchn3 tchn1 edu_treat1 edu_treat2 l_inc `var1' if hsco==0, cluster(class1)
est store m2
reg  tmath3 tmath1 edu_treat1 edu_treat2 l_inc `var1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Panel B
local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
local j=4
foreach v of local lo{
	reg `v'2 `v'1 edu_treat1 edu_treat2 l_inc `var1' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg  ave3 ave1 edu_treat1 edu_treat2 l_inc `var1' if hsco==1, cluster(class1)
est store m1
reg  tchn3 tchn1 edu_treat1 edu_treat2 l_inc `var1' if hsco==1, cluster(class1)
est store m2
reg  tmath3 tmath1 edu_treat1 edu_treat2 l_inc `var1' if hsco==1, cluster(class1)
est store m3

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*=============================Table A10=========================================

*--------Columns 1-6
use final, clear

keep if treat1==1

// re-calculate the standarized score for MC class
drop tchn* tmath* ave* de_* dif_* adesk*

forvalues i=1/3{
	egen ave`i' = rowmean (chn`i' math`i')
}

local var ="chn math ave"
foreach v of local var{
	forvalues i=1/3{
		qui sum `v'`i' 
		local m = r(mean)
		local sd = r(sd)
		gen t`v'`i' = (`v'`i'-`m')/`sd'
	}
}
bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

replace pctil = pctil-0.5
gen d = (pctil < 0)

gen pctil2 = pctil*pctil
gen pctil3 = pctil2*pctil

drop rk size

xtset desk hsco
local var = "tchn1 tchn3 tmath1 tmath3 tave1 tave3 gender extra2 agree2 open2 neur2 cons2 extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	gen de_`v' = l.`v'
	replace de_`v' = f.`v' if de_`v' == .
}

xtset initial_desk hsco
gen dein_tave1 = l.tave1
replace dein_tave1 = f.tave1 if dein_tave1==.

// genereate class-height group
egen clsh = group (class1 hgroup)

gen lsco = 1- hsco
gen TTD = (d==1 & noncomplier==0)


//Column 1, Panels A-B
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local de_ncog = "de_extra1 de_agree1 de_open1 de_neur1 de_cons1"

reg tave3 tave1 `de_ncog' `xvar1' if hsco==0, cluster(class1)
est store m1
reg tave3 tave1 `de_ncog' `xvar1' if hsco==1, cluster(class1)
est store m2

outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//non-cognitive, Columns 2-6, Panel A
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_tave1 `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j=`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//non-cognitive, Columns 2-6, Panel B
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_tave1 `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j=`j'+1
}

outreg2 [m1 m2 m3 m4 m5 ] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


*--------Columns 7-12
use final, clear

keep if treat2==1	 //re-stanadrize the score in MS or MSR classes

// re-calculate the standarized score for MC class
drop tchn* tmath* ave* de_* dif_* adesk*

forvalues i=1/3{
	egen ave`i' = rowmean (chn`i' math`i')
}

local var ="chn math ave"
foreach v of local var{
	forvalues i=1/3{
		qui sum `v'`i' 
		local m = r(mean)
		local sd = r(sd)
		gen t`v'`i' = (`v'`i'-`m')/`sd'
	}
}
bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

replace pctil = pctil-0.5
gen d = (pctil < 0)

gen pctil2 = pctil*pctil
gen pctil3 = pctil2*pctil

drop rk size

xtset desk hsco
local var = "tchn1 tchn3 tmath1 tmath3 tave1 tave3 gender extra2 agree2 open2 neur2 cons2 extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	gen de_`v' = l.`v'
	replace de_`v' = f.`v' if de_`v' == .
}

xtset initial_desk hsco
gen dein_tave1 = l.tave1
replace dein_tave1 = f.tave1 if dein_tave1==.

// genereate class-height group
egen clsh = group (class1 hgroup)

gen lsco = 1- hsco
gen TTD = (d==1 & noncomplier==0)


//Column 7, Panels A-B
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local de_ncog = "de_extra1 de_agree1 de_open1 de_neur1 de_cons1"

reg tave3 tave1 `de_ncog' `xvar1' if hsco==0, cluster(class1)
est store m1
reg tave3 tave1 `de_ncog' `xvar1' if hsco==1, cluster(class1)
est store m2

outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//non-cognitive, Columns 8-12, Panel A
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_tave1 `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j=`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//non-cognitive, Columns 8-12, Panel B
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_tave1 `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j=`j'+1
}

outreg2 [m1 m2 m3 m4 m5 ] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*=============================Table A11=========================================
use final, clear

local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg change_d_fri `xvar1' if uptrack==0, cluster(class1)
est store m1
test treat1=treat2

reg change_d_fri `xvar1' if uptrack==1, cluster(class1)
est store m2
test treat1=treat2

outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*=============================Table A12=========================================
// Open a network data
use net, clear
nwset f2_1-f2_1802, directed name(stunet2)
local xvar1 ="track x1 x2 treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
nwqap stunet2 `xvar1', permutations(100) type(reg)
// NOTE: there is no option to set seed in nwqap command, the p-values may be different, but they are not significantly differnt.
*=============================Table A13=========================================

use final, clear
drop x1 x2
quietly{
merge 1:1 psid using neighbor_girl

gen x1 = treat1*fr_girl
gen x2 = treat2*fr_girl
}

// Panel A, Columns 1-3
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

qui reg ave3 ave1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m1
qui reg tchn3 tchn1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m2
qui reg tmath3 tmath1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

// Panel A, Columns 4-8
local lo = "extra agree open neur cons"
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*-------------------Dropping first and last row---------------------------------
// Panel B, Columns 1-3
drop x1 x2
drop if fr_girl==0.33|fr_girl==0.67

gen x1 = treat1*fr_girl
gen x2 = treat2*fr_girl

local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

qui reg ave3 ave1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m1
qui reg tchn3 tchn1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m2
qui reg tmath3 tmath1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
est store m3

outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

// Panel B, Columns 4-8
local lo = "extra agree open neur cons"
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 x1 x2 fr_girl `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*=============================Table A14=========================================

/*Results are not included
Table A14 is a table introducing the questions of the student evaluation of the 
class head teacher*/

*=============================Table A15=========================================
use final, clear

do "d:\program files\stata16\ado\personal\aej2021\teacher_clean.do"


*------------------------------OLS----------------------------------------------
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

test treat1=treat2
qui reg dtstudy `xvar1' if hsco==0, cluster(class1)
est store m1
test treat1=treat2
qui reg dtstudy `xvar1' if hsco==1, cluster(class1)
est store m2
test treat1=treat2

test treat1=treat2
qui reg dtcare `xvar1' if hsco==0, cluster(class1)
est store m3
test treat1=treat2
qui reg dtcare `xvar1' if hsco==1, cluster(class1)
est store m4
test treat1=treat2

test treat1=treat2
qui reg dtequal `xvar1' if hsco==0, cluster(class1)
est store m5
test treat1=treat2
qui reg dtequal `xvar1' if hsco==1, cluster(class1)
est store m6
test treat1=treat2

outreg2 [m1 m2 m3 m4 m5 m6] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*===========================Table A16===========================================
*---------------Columns 1-6
use final, clear

// merge with teacher effect
//decode school, gen(schoolx)
merge m:1 school grade class using teacher_info
keep if _merge==3 
drop _merge

keep if treat1==1

// re-calculate the standarized score for MC class
drop tchn* tmath* ave* de_* dif_* adesk*

forvalues i=1/3{
	egen ave`i' = rowmean (chn`i' math`i')
}

local var ="chn math ave"
foreach v of local var{
	forvalues i=1/3{
		qui sum `v'`i' 
		local m = r(mean)
		local sd = r(sd)
		gen t`v'`i' = (`v'`i'-`m')/`sd'
	}
}
bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

replace pctil = pctil-0.5
gen d = (pctil < 0)

gen pctil2 = pctil*pctil
gen pctil3 = pctil2*pctil

drop rk size

xtset desk hsco
local var = "tchn1 tchn3 tmath1 tmath3 tave1 tave3 gender extra2 agree2 open2 neur2 cons2 extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	gen de_`v' = l.`v'
	replace de_`v' = f.`v' if de_`v' == .
}

xtset initial_desk hsco
gen dein_tave1 = l.tave1
replace dein_tave1 = f.tave1 if dein_tave1==.

// genereate class-height group
egen clsh = group (class1 hgroup)

gen lsco = 1- hsco
gen TTD = (d==1 & noncomplier==0)

gen de_teavl_tea = de_tave1*tedu

local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
reg tave3 tave1 de_tave1 de_teavl_tea `xvar1' if hsco==0, cluster(class1)
est store m1

reg tave3 tave1 de_tave1 de_teavl_tea `xvar1' if hsco==1, cluster(class1)
est store m2
//outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

foreach v of local lo{
	gen `v'1_tea = de_`v'1*tedu
}

local j=3
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `v'1_tea tedu `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local j=8
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `v'1_tea tedu `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*---------------Columns 7-12
use final, clear

// merge with teacher effect
//decode school, gen(schoolx)
merge m:1 school grade class using teacher_info
keep if _merge==3 
drop _merge

keep if treat2==1	 //re-stanadrize the score in MS or MSR classes

// re-calculate the standarized score for MC class
drop tchn* tmath* ave* de_* dif_* adesk*

forvalues i=1/3{
	egen ave`i' = rowmean (chn`i' math`i')
}

local var ="chn math ave"
foreach v of local var{
	forvalues i=1/3{
		qui sum `v'`i' 
		local m = r(mean)
		local sd = r(sd)
		gen t`v'`i' = (`v'`i'-`m')/`sd'
	}
}
bys class1: egen rk = rank(ave1), track	
bys class1: egen size = count(psid)
gen pctil = rk/size 	// low value means bottom

replace pctil = pctil-0.5
gen d = (pctil < 0)

gen pctil2 = pctil*pctil
gen pctil3 = pctil2*pctil

drop rk size

xtset desk hsco
local var = "tchn1 tchn3 tmath1 tmath3 tave1 tave3 gender extra2 agree2 open2 neur2 cons2 extra1 agree1 open1 neur1 cons1"

foreach v of local var{
	gen de_`v' = l.`v'
	replace de_`v' = f.`v' if de_`v' == .
}

xtset initial_desk hsco
gen dein_tave1 = l.tave1
replace dein_tave1 = f.tave1 if dein_tave1==.

// genereate class-height group
egen clsh = group (class1 hgroup)

gen lsco = 1- hsco
gen TTD = (d==1 & noncomplier==0)

gen de_teavl_tea = de_tave1*tedu

local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
reg tave3 tave1 de_tave1 de_teavl_tea `xvar1' if hsco==0, cluster(class1)
est store m1

reg tave3 tave1 de_tave1 de_teavl_tea `xvar1' if hsco==1, cluster(class1)
est store m2
//outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

foreach v of local lo{
	gen `v'1_tea = de_`v'1*tedu
}

local j=3
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `v'1_tea tedu `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}

local j=8
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `v'1_tea tedu `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*===========================Table A17===========================================
use final, clear

tab grade1, gen(grd)

local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"

//lower track: Panel A, Columns 1-3
moulton ave3 ave1  `xvar1' grd1-grd12 if hsco==0, cluster(class1)

moulton tchn3 tchn1  `xvar1' grd1-grd12 if hsco==0, cluster(class1)

moulton tmath3 tmath1  `xvar1' grd1-grd12 if hsco==0, cluster(class1)

//non-cog: Panel A, Columns 4-8
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"
local lo = "extra agree open neur cons"

local j =1
foreach v of local lo{
	moulton `v'2 `v'1 `xvar1' grd1-grd12 if hsco==0, cluster(class1)

}

// upper track: Panel B, Columns 1-3
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"

moulton ave3 ave1  `xvar1' grd1-grd12 if hsco==1, cluster(class1)

moulton tchn3 tchn1  `xvar1' grd1-grd12 if hsco==1, cluster(class1)

moulton tmath3 tmath1  `xvar1' grd1-grd12 if hsco==1, cluster(class1)

//non-cog: Panel B, Columns 4-8
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"
local lo = "extra agree open neur cons"

local j =1
foreach v of local lo{
	moulton `v'2 `v'1 `xvar1' grd1-grd12 if hsco==1, cluster(class1)

}

*=============================Table A18=========================================

/*Table A18 is not included
Table A18 is a table comparing our results with the existing literature*/
