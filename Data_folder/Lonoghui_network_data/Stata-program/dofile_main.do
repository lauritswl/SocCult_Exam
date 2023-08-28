/*===============================================================================

                            Main Results
							
Description: Analyze the experimental data collected in Longhui. This code is 
only used to produce tables in the text.

*==============================================================================*/

cd "d:\program files\stata16\ado\personal\aej2021"

*==========================Table 1==============================================
use final, clear

//Columns 1-2
local var ="chn1 math1 extra1 agree1 open1 neur1 cons1 chn3 math3 extra2 agree2 open2 neur2 cons2 gender age height health nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui sum `v' if control==1, d

	local m = r(mean)
	local sd = r(sd)

	disp %6.2f `m'
	disp  "["%6.2f `sd' "]" 

}
count if control==1

//Columns 3-4
local var ="chn1 math1 extra1 agree1 open1 neur1 cons1 chn3 math3 extra2 agree2 open2 neur2 cons2 gender age height health nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui sum `v' if treat1==1, d

	local m = r(mean)
	local sd = r(sd)

	disp %6.2f `m'
	disp  "["%6.2f `sd' "]" 
	//disp %6.2f `m'  
}
count if treat1==1

//Columns 5-6
local var ="chn1 math1 extra1 agree1 open1 neur1 cons1 chn3 math3 extra2 agree2 open2 neur2 cons2 gender age height health nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui sum `v' if treat2==1, d

	local m = r(mean)
	local sd = r(sd)

	disp %6.2f `m'
	disp  "["%6.2f `sd' "]" 
}
count if treat2==1

*========================Table 2================================================
use final, clear

preserve
keep if hsco==0

local var ="chn1 math1 extra1 agree1 open1 neur1 cons1 gender age height health nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui reg `v'  treat1 treat2, cluster(class1)

	local b = _b[treat1]
	local s = _se[treat1]
	local p = (2 * ttail(e(df_r), abs(_b[treat1]/_se[treat1])))
	
	local b2 = _b[treat2]
	local s2 = _se[treat2]
	local p2 = (2 * ttail(e(df_r), abs(_b[treat2]/_se[treat2])))
	qui sum `v' if control==1
	local m = r(mean) 
	disp %6.2f `m' "," %6.2f `b' "," "( "%6.2f `p' ")" "," %6.2f `b2' "," "( "%6.2f `p2' ")"

}

restore
// Columns 5-8
keep if hsco==1

local var ="chn1 math1 extra1 agree1 open1 neur1 cons1 gender age height health nationality_rd1 hukou_rd1 interest_chn1 interest_math1 fa_eduy mo_eduy pc car"
foreach v of local var{
	qui reg `v'  treat1 treat2, cluster(class1)

	local b = _b[treat1]
	local s = _se[treat1]
	local p = (2 * ttail(e(df_r), abs(_b[treat1]/_se[treat1])))
	
	local b2 = _b[treat2]
	local s2 = _se[treat2]
	local p2 = (2 * ttail(e(df_r), abs(_b[treat2]/_se[treat2])))
	qui sum `v' if control==1
	local m = r(mean) 
	disp %6.2f `m' "," %6.2f `b' "," "( "%6.2f `p' ")" "," %6.2f `b2' "," "( "%6.2f `p2' ")"

}

*========================Table 3================================================
use final, clear

//Panel A
local ovar1 = "treat1 treat2"
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg ave3 ave1  `ovar1' if hsco==0, cluster(class1)
est store m1
test treat1=treat2
reg ave3 ave1  `xvar1' if hsco==0, cluster(class1)
est store m2
test treat1=treat2

reg tchn3 tchn1  `ovar1' if hsco==0, cluster(class1)
est store m3
test treat1=treat2
reg tchn3 tchn1  `xvar1' if hsco==0, cluster(class1)
est store m4
test treat1=treat2

reg tmath3 tmath1 `ovar1' if hsco==0, cluster(class1)
est store m5
test treat1=treat2
reg tmath3 tmath1 `xvar1' if hsco==0, cluster(class1)
est store m6
test treat1=treat2

outreg2 [m1 m2 m3 m4 m5 m6] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


//Panel B
local ovar1 = "treat1 treat2"
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg ave3 ave1  `ovar1' if hsco==1, cluster(class1)
est store m1
test treat1=treat2
reg ave3 ave1  `xvar1' if hsco==1, cluster(class1)
est store m2
test treat1=treat2

reg tchn3 tchn1  `ovar1' if hsco==1, cluster(class1)
est store m3
test treat1=treat2
reg tchn3 tchn1  `xvar1' if hsco==1, cluster(class1)
est store m4
test treat1=treat2

reg tmath3 tmath1 `ovar1' if hsco==1, cluster(class1)
est store m5
test treat1=treat2
reg tmath3 tmath1 `xvar1' if hsco==1, cluster(class1)
est store m6
test treat1=treat2

outreg2 [m1 m2 m3 m4 m5 m6] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*========================Table 4================================================
use final, clear

local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
 
//Panel A
local j=1
foreach v of local lo{
	sum `v'2 if hsco==0
}

local j=1
foreach v of local lo{
	reg `v'2 `v'1 `var1' if hsco==0, cluster(class1)
	test treat1 = treat2 
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


local lo = "extra agree open neur cons"
local var1 = "treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
//Panel B
local j=1
foreach v of local lo{
	sum `v'2 if hsco==1
}

local j=1
foreach v of local lo{
	reg `v'2 `v'1 `var1' if hsco==1, cluster(class1)
	test treat1 = treat2 
	est store m`j'
	local j =`j'+1
}

outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


*======================Table 5==================================================
//Columns 1-6
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

//Column 1
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
reg tave3 tave1 de_tave1 `xvar1' if hsco==0, cluster(class1)
est store m1

reg tave3 tave1 de_tave1 `xvar1' if hsco==1, cluster(class1)
est store m2
outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


//Panel A. Columns 2-6
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j=`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Panel B. Columns 2-6
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j=`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 7-12
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

//Column 7
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
reg tave3 tave1 de_tave1 `xvar1' if hsco==0, cluster(class1)
est store m1

reg tave3 tave1 de_tave1 `xvar1' if hsco==1, cluster(class1)
est store m2
outreg2 [m1 m2] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


//Panel A. Columns 8-12 
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `xvar1' if hsco==0, cluster(class1)
	est store m`j'
	local j=`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Panel B. Columns 8-12 
local xvar1 ="gender de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.clsh"
local lo = "extra agree open neur cons"

local j=1
foreach v of local lo{
	reg `v'2 `v'1 de_`v'1 `xvar1' if hsco==1, cluster(class1)
	est store m`j'
	local j=`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


*======================Table 6==================================================
use final, clear

//Column 1
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg dif_desk_fri `xvar1' if hsco==0, cluster(class1)
est store m1
test treat1=treat2

reg dif_desk_fri `xvar1' if hsco==1, cluster(class1)
est store m2
test treat1=treat2


//Column 2
local xvar1 ="treat1 treat2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

reg change_d_fri `xvar1' if hsco==0, cluster(class1)
est store m3
test treat1=treat2

reg change_d_fri `xvar1' if hsco==1, cluster(class1)
est store m4
test treat1=treat2

//Column 3
reg change_lownum_fri `xvar1' if hsco==0, cluster(class1)
est store m5
test treat1=treat2
reg change_lownum_fri `xvar1' if hsco==1, cluster(class1)
est store m6
test treat1=treat2

//Column 4
reg change_upnum_fri `xvar1' if hsco==0, cluster(class1)
est store m7
test treat1=treat2
reg change_upnum_fri `xvar1' if hsco==1, cluster(class1)
est store m8
test treat1=treat2

outreg2 [m1 m2 m3 m4 m5 m6 m7 m8] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)


*======================Table 7==================================================
use final, clear

gen gdt1 = treat1 * gender
gen gdt2 = treat2 * gender

local xvar3 ="treat1 treat2 gdt1 gdt2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
//Columns 1-3, Panel A
reg ave3 ave1 `xvar3' if hsco==0, cluster(class1)
est store m1
reg tchn3 tchn1 `xvar3' if hsco==0, cluster(class1)
est store m2
reg tmath3 tmath1 `xvar3' if hsco==0, cluster(class1)
est store m3

//Columns 1-3, Panel B
reg ave3 ave1 `xvar3' if hsco==1, cluster(class1)
est store m4
reg tchn3 tchn1 `xvar3' if hsco==1, cluster(class1)
est store m5
reg tmath3 tmath1 `xvar3' if hsco==1, cluster(class1)
est store m6

outreg2 [m1 m2 m3 m4 m5 m6] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 4-8, Panel A
local yvar = "extra agree open neur cons"
local xvar3 ="treat1 treat2 gdt1 gdt2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local yvar{
	reg `v'2 `v'1  `xvar3' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 4-8, Panel B
local yvar = "extra agree open neur cons"
local xvar3 ="treat1 treat2 gdt1 gdt2 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local yvar{
	reg `v'2 `v'1  `xvar3' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*======================Table 8==================================================
use final, clear
drop de_gender

xtset desk hsco
//br desk hsco gender

gen de_gender = gender - l.gender
replace de_gender= 1 if de_gender==-1

replace de_gender = f.de_gender if de_gender==.

gen dfgt1 = treat1 * de_gender
gen dfgt2 = treat2 * de_gender


local xvar3 ="treat1 treat2 dfgt1 dfgt2 de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
//Columns 1-3, Panel A
reg ave3 ave1 `xvar3' if hsco==0, cluster(class1)
est store m1
reg tchn3 tchn1 `xvar3' if hsco==0, cluster(class1)
est store m2
reg tmath3 tmath1 `xvar3' if hsco==0, cluster(class1)
est store m3
outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 4-8, Panel A
local yvar = "extra agree open neur cons"
local xvar3 ="treat1 treat2 dfgt1 dfgt2 de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local yvar{
	reg `v'2 `v'1  `xvar3' if hsco==0, cluster(class1)
	est store m`j'
	local j =`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 1-3, Panel B
local yvar = "extra agree open neur cons"
local xvar3 ="treat1 treat2 dfgt1 dfgt2 de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
reg ave3 ave1 `xvar3' if hsco==1, cluster(class1)
est store m1
reg tchn3 tchn1 `xvar3' if hsco==1, cluster(class1)
est store m2
reg tmath3 tmath1 `xvar3' if hsco==1, cluster(class1)
est store m3
outreg2 [m1 m2 m3] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

//Columns 4-8, Panel B
local yvar = "extra agree open neur cons"
local xvar3 ="treat1 treat2 dfgt1 dfgt2 de_gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"

local j=1
foreach v of local yvar{
	reg `v'2 `v'1  `xvar3' if hsco==1, cluster(class1)
	est store m`j'
	local j =`j'+1
}
outreg2 [m1 m2 m3 m4 m5] using ols.rtf, replace word sdec(3) bdec(3) br drop(cls*) see title(RESULT)

*=======================Table 9=================================================
use final, clear

gen ave3_bl =ave1
gen tchn3_bl = tchn1
gen tmath3_bl = tmath1
gen extra2_bl = extra1
gen agree2_bl = agree1
gen open2_bl = open1
gen neur2_bl = neur1
gen cons2_bl = cons1
// Results in columns 1 and 5 are obtained by the oritinal estimates

// Columns 2 and 6
local controls0 = "gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"
local controls1 = "tchn1 tmath1 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade"

rwolf tchn3 tmath3 extra2 agree2 open2 neur2 cons2 dif_desk_fri change_d_fri change_lownum_fri change_upnum_fri if hsco==0, /// 
indepvar(treat1 treat2) controls(`controls1') reps(200) seed(36) 


local controls0 = "gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car"
local controls1 = "tchn1 tmath1 gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade"

rwolf tchn3 tmath3 extra2 agree2 open2 neur2 cons2 dif_desk_fri change_d_fri change_lownum_fri change_upnum_fri if hsco==1, /// 
indepvar(treat1 treat2) controls(`controls1') reps(200) seed(36) 

// Columns 3, 4, 7, and 8
local var1 = "gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
randcmd ((treat1 treat2) reg tmath3 tmath1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg tchn3 tchn1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg extra2 extra1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg agree2 agree1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg open2 open1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg neur2 neur1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg cons2 cons1 treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg dif_desk_fri treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg change_d_fri treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg change_lownum_fri treat1 treat2 `var1' if hsco==0, cluster(class1)) ///
	((treat1 treat2) reg change_upnum_fri treat1 treat2 `var1' if hsco==0, cluster(class1)), ///
	 treatvars(treat1 treat2) strata(grade1) reps(2000) seed(4)

local var1 = "gender age height hukou_rd1 nationality_rd1 health sib fa_eduy mo_eduy pc car i.grade1"
randcmd ((treat1 treat2) reg tmath3 tmath1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg tchn3 tchn1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg extra2 extra1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg agree2 agree1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg open2 open1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg neur2 neur1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg cons2 cons1 treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg dif_desk_fri treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg change_d_fri treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg change_lownum_fri treat1 treat2 `var1' if hsco==1, cluster(class1)) ///
	((treat1 treat2) reg change_upnum_fri treat1 treat2 `var1' if hsco==1, cluster(class1)), ///
	 treatvars(treat1 treat2) strata(grade1) reps(2000) seed(4)

