*===============================================================================

*                       Clean data of evaluation of teacher


*===============================================================================
quietly{
//translate 0-3 to 1-4
local allvar = "impatient math progress question existence praise unconcerned criticize care suspect talk ignore friendly partial clothes appearance despise fair same"
forvalues i=1/2{
	foreach v of local allvar{
		replace `v'_rd`i' = `v'_rd`i'+1
	}
}

// transfer negative assessment variable
local vr "impatient math existence unconcerned criticize partial suspect clothes appearance despise ignore"
forvalues i=1/2{
	foreach v of local vr{
		replace `v'_rd`i' = 5 -`v'_rd`i'
	}
}

local tstudy = "impatient math progress question"
local tcare = "existence praise unconcerned criticize care suspect talk ignore friendly" 
local tequal = "partial clothes appearance despise fair same"

//standarized each variable
local zzz = "tstudy tcare tequal"
foreach z of local zzz{
  forvalues i=1/2{
	foreach v of local `z'{
		local `z'_s`i' "``z'_s`i'' `v'_rd`i'"
		
		qui sum `v'_rd`i'
		local m = r(mean)
		local sd = r(sd)
		replace `v'_rd`i' = (`v'_rd`i'-`m')/`sd'
	}
  disp "``z'_s`i''"
  egen `z'_rd`i' = rowtotal(``z'_s`i'')
  }
}
}

foreach v of varlist tstudy_rd1 - tequal_rd2{
	qui sum `v'
	local m = r(mean)
	local sd = r(sd)
	replace `v' = (`v'-`m')/`sd'
	

}
gen dtstudy = tstudy_rd2 - tstudy_rd1
gen dtcare  = tcare_rd2 - tcare_rd1
gen dtequal = tequal_rd2 - tequal_rd1

drop tstudy_rd1 - tequal_rd2