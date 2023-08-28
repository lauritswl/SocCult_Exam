/*===============================================================================

                            Figure
							
Description: This code is used to produce figures in the text

*==============================================================================*/

*==============================================================================*/
clear all
cd "d:\program files\stata16\ado\personal\aej2021"

*=======================Figure 1================================================

*Figure 1 is a timeline of the experiment
*No need data to draw this figure

*=======================Figure 2================================================

*Figure 2 is a sample of desk arrangement in a typical classroom
*No need data to draw this figure

*=======================Figure 3================================================
use final, clear
collapse (mean) ave1 height, by(treatmentcontrol row_rd2)

twoway line ave1 row_rd2 if treatmentcontrol ==0, yaxis(1) ytitle("Average baseline z-score") lpattern(dash) lcolor(navy)  ///
	|| line height row_rd2 if treatmentcontrol ==0, yaxis(2) xtitle("Row number of student's seat")  ytitle("Height(Inch)", axis(2)) ylabel(50(1)55, axis(2)) ///
	ylabel(-0.15(0.05)0.1, axis(1) nogrid) xlabel(1(1)8) graphregion(color(white)) ///
	legend(region(fcolor(none) lcolor(none)) label(2 "Height" ) label(1 "Average baseline z-score") position(11) ring(0) symx(8) cols(1)) ///
	title("A.Control class", color(black)) saving(g101, replace)
	
twoway line ave1 row_rd2 if treatmentcontrol ==1, yaxis(1) ytitle("Average baseline z-score") lpattern(dash) lcolor(navy)  ///
	|| line heigh row_rd2 if treatmentcontrol ==1, yaxis(2) xtitle("Row number of student's seat")  ytitle("Height(Inch)", axis(2)) ylabel(50(1)55, axis(2)) ///
	ylabel(-0.15(0.05)0.1, axis(1) nogrid) xlabel(1(1)8) graphregion(color(white)) ///
	legend(region(fcolor(none) lcolor(none)) label(2 "Height" ) label(1 "Average baseline z-score") position(11) ring(0) symx(8) cols(1)) ///
	title("B.MS class", color(black)) saving(g102, replace)

twoway line ave1 row_rd2 if treatmentcontrol ==2, yaxis(1) ytitle("Average baseline z-score") lpattern(dash) lcolor(navy)  ///
	|| line height row_rd2 if treatmentcontrol ==2, yaxis(2) xtitle("Row number of student's seat")  ytitle("Height(Inch)", axis(2)) ylabel(50(1)55, axis(2)) ///
	ylabel(-0.15(0.05)0.1, axis(1) nogrid) xlabel(1(1)8) graphregion(color(white)) ///
	legend(region(fcolor(none) lcolor(none)) label(2 "Height" ) label(1 "Average baseline z-score") position(11) ring(0) symx(8) cols(1)) ///
	title("C.MSR class", color(black)) saving(g103, replace)
	
graph combine g101.gph g102.gph g103.gph , col(1) xsize(6) ysize(15) iscale(*1.2) graphregion(color(white))		

*=======================Figure 4================================================
use final, clear

bys class1: egen median = median(ave1) 
replace med = 1 if ave1>=median
replace med = 0 if ave1<median

xtset desk hsco
gen desk_ave1 = l.ave1
replace desk_ave1 = f.ave1 if desk_ave1==.

graph twoway hist desk_ave1 if treat1==1 & hsco==0, width(0.2) ///
	  fcolor(navy%50) lcolor(navy) fintensity(inten60)	///
      ||  hist desk_ave1 if control==1 & median==0, width(0.2) ///
	  fcolor(white%1) lcolor(maroon) lpattern(dash) fintensity(inten70) ///
	  ylabel(0(0.2)1, nogrid) ytitle("Density", size(medium)) xtitle("Deskmates' score", size(medium)) graphregion(color(white))  ///
	  legend(region(fcolor(none) lcolor(none)) label(1 "MS" ) label(2 "Control") position(10) ring(0) symx(4) cols(1)) ///
	  saving(g41, replace)

graph twoway hist desk_ave1 if treat2==1 & hsco==0, width(0.2) ///
	  fcolor(dkgreen%50) lcolor(dkgreen) fintensity(inten50)	///
      ||  hist desk_ave1 if control==1 & median==0, width(0.2) ///
	  fcolor(white%1) lcolor(maroon)  lpattern(dash) fintensity(inten70) ///
	  ylabel(0(0.2)1, nogrid) ytitle("Density", size(medium)) xtitle("Deskmates' score", size(medium)) graphregion(color(white))  ///
	  legend(region(fcolor(none) lcolor(none)) label(1 "MSR" ) label(2 "Control") position(10) ring(0) symx(4) cols(1)) ///
	  saving(g42, replace)

graph combine g41.gph g42.gph , col(2) xsize(15) ysize(5) iscale(*1.2) graphregion(color(white))
*=======================Figure A1===============================================

*Figure A1 is a photo of a desk
*No need data to draw this figure

*=======================Figure A2===============================================

*Figure A2 is drawn by Gephi
*We introduce it in the Readme file