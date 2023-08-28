cd "d:\program files\stata16\ado\personal\aej2021"

use psidmerge, clear

drop if var2==""

tab treatmentcontrol, gen(treat)
forvalues i=1/3{
	local j = `i'-1
	rename treat`i'  treat`j' 
}
rename treat0 control
label var control "control group"
label var treat1 "MS group"
label var treat2 "MSR group"

qui sum
local n1 = r(N)
keep if _merge==5

drop _merge*

drop group

egen grade1 = group(school grade)

drop eng2 eng3
order chn2 chn3, after(chn1)
order math2 math3, after(math1)

*=======================Individual Characteristics==============================
replace  nationality_rd1=0 if  nationality_rd1==2		//non-minority==1

replace year_rd1 = 2005 if year_rd1 ==2055 | year_rd1==20005 
replace year_rd1 = 2006 if year_rd1 ==20006 | year_rd1==20060 |year_rd1==20066 |year_rd1==20067
replace year_rd1 = 2007 if year_rd1 ==2007011 |year_rd1==2007050| year_rd1==2007082
replace year_rd1 =. if year_rd1>2010 | year_rd1<2001

replace year_rd1 = 2005 if year_rd1==. & grade==5
replace year_rd1 = 2006 if year_rd1==. & grade==4
replace year_rd1 = 2007 if year_rd1==. & grade==3

bys class1: egen p33 = pctile (height), p(33)
bys class1: egen p66 = pctile (height), p(66)

/*
gen hgroup1 = 0 if height <=p33
replace hgroup1 = 1 if height>p33 & height<=p66
replace hgroup1 = 2 if height>p66
order hgroup1, after(hgroup)

egen ch = group(class1 hgroup1)
label var ch "class height group"
*/


rename weight_rd1 weight

// transform to years of education
foreach v of varlist  momeducation_rd1 dadeducation_rd1{
	replace `v'=19 if `v'==7
	replace `v'=16 if `v'==6
	replace `v'=15 if `v'==5
	replace `v'=12 if `v'==4
	replace `v'=9 if `v'==3
	replace `v'=6 if `v'==2
	replace `v'=3 if `v'==1

}
rename momeducation_rd1 mo_eduy
rename dadeducation_rd1 fa_eduy

egen totinc = rowtotal(dadincome_rd1 momincome_rd1)

rename computer_rd1 pc
rename car_rd1 car

foreach v of varlist  elderbrother_rd1 eldersister_rd1 brother_rd1 sister_rd1{
	replace `v'=0 if `v'>4

}

egen sib = rowtotal ( elderbrother_rd1 eldersister_rd1 brother_rd1 sister_rd1)
replace sib = 0 if sib>6

foreach v of varlist  interest1_rd1 interest2_rd1{
	replace `v' = 0 if `v'<=3
	replace `v' = 1 if `v'==4|`v'==5

}
rename interest1_rd1 interest_chn1
rename interest2_rd1 interest_math1

replace  health_rd1 = 0  if  health_rd1<=3
replace  health_rd1 = 1 if  health_rd1==4| health_rd1==5

rename health_rd1 health

bys desk: egen adesk1 = mean(ave1)
label var adesk1 "deskmate's average score of rd1 exam"
bys desk: egen adesk2 = mean(ave2)
label var adesk2 "deskmate's average score of rd2 exam"
bys desk: egen adesk3 = mean(ave3)
label var adesk3 "deskmate's average score of rd3 exam"

*-----------------Deskmate's Characteristics-----------
duplicates drop desk hsco, force
xtset desk hsco
foreach v of varlist chn1 math1 tchn1 tmath1 tchn2 tmath2 tchn3 tmath3 {
	gen de_`v' = l.`v'
	replace de_`v' = f.`v' if de_`v'==.
	gen dif_`v' = de_`v'-`v'
}


drop if math3==.&math2==.
drop if mo_eduy==.|fa_eduy==.

drop npdesk
bys desk: egen npdesk =count(desk)

foreach v of varlist ave1 ave2 ave3 {
	qui sum `v' if control==1
	local m = r(mean)
    local sd = r(sd)
	replace `v' = (`v'-`m')/`sd'
}

foreach v of varlist adesk1 adesk2 adesk3{
	qui sum `v' if control==1
	local m = r(mean)
    local sd = r(sd)
	replace `v' = (`v'-`m')/`sd'
}

*==========================Noncomplier==========================================
//generate variables for 2sls (the average treatment effect)

gen atreat1 = (treat1==1)&(noncomplier==0)
gen atreat2 = (treat2==1)&(noncomplier==0)

label var atreat1 "actural treat in MS"
label var atreat2 "actural treat in mSR"

*======================Big Five=================================================

egen extra1 = rowtotal(bx2_1 bx7_1 bx12_1 bx18_1 bx22_1 bx27_1 bx32_1 bx37_1 bx42_1 bx47_1 bx52_1 bx57_1 )
egen extra2 = rowtotal(bx2_2 bx7_2 bx12_2 bx18_2 bx22_2 bx27_2 bx32_2 bx37_2 bx42_2 bx47_2 bx52_2 bx57_2 )

egen agree1 = rowtotal(bx4_1 bx9_1 bx14_1 bx19_1 bx24_1 bx29_1 bx34_1 bx39_1 bx44_1 bx49_1 bx54_1 bx59_1 )
egen agree2 = rowtotal(bx4_2 bx9_2 bx14_2 bx19_2 bx24_2 bx29_2 bx34_2 bx39_2 bx44_2 bx49_2 bx54_2 bx59_2 )

egen open1 = rowtotal(bx3_1 bx8_1 bx13_1 bx17_1 bx23_1 bx28_1 bx33_1 bx38_1 bx43_1 bx48_1 bx53_1 bx58_1 )
egen open2 = rowtotal(bx3_2 bx8_2 bx13_2 bx17_2 bx23_2 bx28_2 bx33_2 bx38_2 bx43_2 bx48_2 bx53_2 bx58_2 )

egen neur1 = rowtotal(bx1_1 bx6_1 bx11_1 bx16_1 bx21_1 bx26_1 bx31_1 bx36_1 bx41_1 bx46_1 bx51_1 bx56_1 )
egen neur2 = rowtotal(bx1_2 bx6_2 bx11_2 bx16_2 bx21_2 bx26_2 bx31_2 bx36_2 bx41_2 bx46_2 bx51_2 bx56_2 )

egen cons1 = rowtotal(bx5_1 bx10_1 bx15_1 bx20_1 bx25_1 bx30_1 bx35_1 bx40_1 bx45_1 bx50_1 bx55_1 bx60_1 )
egen cons2 = rowtotal(bx5_2 bx10_2 bx15_2 bx20_2 bx25_2 bx30_2 bx35_2 bx40_2 bx45_2 bx50_2 bx55_2 bx60_2 )


local lo = "extra open agree neur cons"
foreach v of local lo{
	replace `v'2 = 60 if `v'2>60
	replace `v'2 = `v'1 if `v'2==0
}

label var extra1 "extraversion_rd1"
label var extra2 "extraversion_rd12"

label var agree1 "agreeableness_rd1"
label var agree2 "agreeableness_rd2"

label var open1 "openness_rd1"
label var open2 "openness_rd2"

label var neur1 "neuroticism_rd1"
label var neur2 "neuroticism_rd2"

label var cons1 "conscientiousness_rd1"
label var cons2 "conscientiousness_rd2"


*=============================Label var=========================================

drop var2 position_rd1

drop p33 p66

label var psid "number of student"
label var school "school name"
label var grade "grade number"
label var class "class number"
label var year_rd1 "birth year"
label var month_rd1 "birth month"
label var stnumber_rd1 "student number"
label var gender "gender"
label var nationality_rd1 "race"
label var height "height"
label var weight "weight"
label var hukou_rd1 "hukou registation status"
label var vehicle_rd1 "vehicle from home to school"      
label var time_rd1 "time from home to school using this vehicle"
label var distance_rd1 "distance from home to head teacher's home"
label var transfer_rd1 "transfer from other school?"
label var tryear_rd1 "the year you transfer from other school"
label var health "health status"
label var studentcadre1_rd1 "student cadre"
label var interest_chn1 "interest in Chinese"
label var interest_math1 "interest in math"
label var attention_rd1 "can pay attention to study"
label var factor1_rd1 "the most important factor succeed"
label var factor2_rd1 "the 2nd important factor succeed"
label var factor3_rd1 "the 3rd important factor succeed"
label var satisfaction_rd1 "do you satisfy the seat re-assignment"
label var concern_rd1 "parents care about your study"
label var chntutor_rd1 "have someone tutor your Chinese"
label var chntime_rd1 "tutoring time"
label var mathtutor_rd1 "have someone tutor your math"
label var mathtime_rd1 "tutoring time"
label var studyday_rd1 "studying time in school(Monday to Friday)"
label var studyend_rd1 "studying time in school(Saturday & sunday)"
label var readday_rd1 "studying time at home(Monday to Friday)"
label var readend_rd1 "studying time at home(Saturday & sunday)"
label var surfday_rd1 "browing internet time(Monday to Friday)"
label var surfend_rd1 "browing internet time(Saturday & sunday)"
label var tvday_rd1 "watching TV time(Monday to Friday)"
label var tvend_rd1 "watching TV time(Saturday & sunday)"
label var houseworkday_rd1 "doing housework time(Monday to Friday)"
label var houseworkend_rd1 "doing housework time(Saturday & sunday)"
label var farmworkday_rd1 "doing farmwork time(Monday to Friday)"
label var farmworkend_rd1 "doing farmwork time(Saturday & sunday)"
label var book_rd1 "number of book(Saturday & sunday)"
label var dadoutside_rd1 "father absent from home last semester"
label var momoutside_rd1 "mother absent from home last semester"
label var elderbrother_rd1 "number of big brothers"
label var eldersister_rd1 "number of big sisters"
label var brother_rd1 "number of big sisters"
label var sister_rd1 "number of little sisters"
label var dadoccupation_rd1 "father job"
label var fa_eduy "father education"
label var dadlocation_rd1 "father job location"
label var mo_eduy "mother education"
label var momoccupation_rd1 "mother job"
label var momlocation_rd1 "mother job location"
label var dadincome_rd1 "father annual income"
label var momincome_rd1 "monther annual income"
label var pc "have computer in your home"
label var car "have care in your home"

label var bx1_1 "Big-Five_rd1: I am not a worrier"
label var bx2_1 "like to have a lot of people around me"
label var bx3_1 "donot like to wast my time daydreaming"
label var bx4_1 "try to be courteous to everyone I meet"
label var bx5_1 "keep my belongings neat and clean"
label var bx6_1 "often feel inferior to others"
label var bx7_1 "laugh easily"
label var bx8_1 "find the righ way to do, I stick to it"
label var bx9_1 "get into arguments with my family and co-workers"
label var bx10_1 "pretty godd about pacing myself so as to get things done on time"
label var bx11_1 "under a great deal of stress, feel like go to pieces"
label var bx12_1 "consider my self light hearted"
label var bx13_1 "intrigued by the patterns in art and nature"
label var bx14_1 "Some people think I am selfish and egotistical"
label var bx15_1 "not a methodical person"
label var bx16_1 "rarely feel lonely or blue"
label var bx17_1 "enjoy talking to people"
label var bx18_1 "letting students hear controversial speakers can confuse and mislead them"
label var bx19_1 "prefer cooperate than compete"
label var bx20_1 "try to perform all the tasks"
label var bx21_1 "feel tense and jittery"
label var bx22_1 "like to be where the action is"
label var bx23_1 "poetry has little or no effect on me"
label var bx24_1 "be cynical and skeptical of others' intentions"
label var bx25_1 "have a clear set of gols and work toward them in an orderly fashion"
label var bx26_1 "feel worthless"
label var bx27_1 "prefer to do thins alone"
label var bx28_1 "try new and foreign goods"
label var bx29_1 "believe most people will take advantage of you if you let them"
label var bx30_1 "waste a lot of time before settling down to work"
label var bx31_1 "rarely feel fearful or anxious"
label var bx32_1 "feel as if I am bursting with energy"
label var bx33_1 "seldom notice the moods of feelings that different environments produce"
label var bx34_1 "people Iknow like me"
label var bx35_1 "work hard to accomplish my goals"
label var bx36_1 "get angry at the way people treat me"
label var bx37_1 "cheerful, high-spirited person"
label var bx38_1 "believe we should look to our religious authorities for decisioins on moral issues"
label var bx39_1 "people think of me as cold and calculating"
label var bx40_1 "when Imake a commitment, I can be counted on to follow through"
label var bx41_1 "too ofter when things go wrong, Iget discouraged and feel like giving up"
label var bx42_1 "not a cheerful optimist"
label var bx43_1 "when Iam reading poetry or looking at a work of art, I feel a chill or wave of exitement"
label var bx44_1 "hard-headed"
label var bx45_1 "I am not as dependabel or reliable"
label var bx46_1 "I am seldom sad or depressed"
label var bx47_1 "my life is fast-paced"
label var bx48_1 "have little interest in speculating on the nature of the universe or human"
label var bx49_1 "try to be thoughful and considerate"
label var bx50_1 "I am a productive person"
label var bx51_1 "feel helpless and want someone to sovel my problems"
label var bx52_1 "very active person"
label var bx53_1 "have a lot of intellectual curiosity"
label var bx54_1 "I I do not like people, I let them know it"
label var bx55_1 "never seem to be able to get organized"
label var bx56_1 "I have so ashamed and want to hide"
label var bx57_1 "would rather go my onw way"
label var bx58_1 "enjoy playing with theories or abstract ideas"
label var bx59_1 "willing to manipulate people to get what I want"
label var bx60_1 "strive for excellence in everything"

label var bx1_2 "Big-Five_rd2: I am not a worrier"
label var bx2_2 "like to have a lot of people around me"
label var bx3_2 "donot like to wast my time daydreaming"
label var bx4_2 "try to be courteous to everyone I meet"
label var bx5_2 "keep my belongings neat and clean"
label var bx6_2 "often feel inferior to others"
label var bx7_2 "laugh easily"
label var bx8_2 "find the righ way to do, I stick to it"
label var bx9_2 "get into arguments with my family and co-workers"
label var bx10_2 "pretty godd about pacing myself so as to get things done on time"
label var bx11_2 "under a great deal of stress, feel like go to pieces"
label var bx12_2 "consider my self light hearted"
label var bx13_2 "intrigued by the patterns in art and nature"
label var bx14_2 "Some people think I am selfish and egotistical"
label var bx15_2 "not a methodical person"
label var bx16_2 "rarely feel lonely or blue"
label var bx17_2 "enjoy talking to people"
label var bx18_2 "letting students hear controversial speakers can confuse and mislead them"
label var bx19_2 "prefer cooperate than compete"
label var bx20_2 "try to perform all the tasks"
label var bx21_2 "feel tense and jittery"
label var bx22_2 "like to be where the action is"
label var bx23_2 "poetry has little or no effect on me"
label var bx24_2 "be cynical and skeptical of others' intentions"
label var bx25_2 "have a clear set of gols and work toward them in an orderly fashion"
label var bx26_2 "feel worthless"
label var bx27_2 "prefer to do thins alone"
label var bx28_2 "try new and foreign goods"
label var bx29_2 "believe most people will take advantage of you if you let them"
label var bx30_2 "waste a lot of time before settling down to work"
label var bx31_2 "rarely feel fearful or anxious"
label var bx32_2 "feel as if I am bursting with energy"
label var bx33_2 "seldom notice the moods of feelings that different environments produce"
label var bx34_2 "people Iknow like me"
label var bx35_2 "work hard to accomplish my goals"
label var bx36_2 "get angry at the way people treat me"
label var bx37_2 "cheerful, high-spirited person"
label var bx38_2 "believe we should look to our religious authorities for decisioins on moral issues"
label var bx39_2 "people think of me as cold and calculating"
label var bx40_2 "when Imake a commitment, I can be counted on to follow through"
label var bx41_2 "too ofter when things go wrong, Iget discouraged and feel like giving up"
label var bx42_2 "not a cheerful optimist"
label var bx43_2 "when Iam reading poetry or looking at a work of art, I feel a chill or wave of exitement"
label var bx44_2 "hard-headed"
label var bx45_2 "I am not as dependabel or reliable"
label var bx46_2 "I am seldom sad or depressed"
label var bx47_2 "my life is fast-paced"
label var bx48_2 "have little interest in speculating on the nature of the universe or human"
label var bx49_2 "try to be thoughful and considerate"
label var bx50_2 "I am a productive person"
label var bx51_2 "feel helpless and want someone to sovel my problems"
label var bx52_2 "very active person"
label var bx53_2 "have a lot of intellectual curiosity"
label var bx54_2 "I I do not like people, I let them know it"
label var bx55_2 "never seem to be able to get organized"
label var bx56_2 "I have so ashamed and want to hide"
label var bx57_2 "would rather go my onw way"
label var bx58_2 "enjoy playing with theories or abstract ideas"
label var bx59_2 "willing to manipulate people to get what I want"
label var bx60_2 "strive for excellence in everything"

label var impatient_rd1 "He looks impatient when I ask him questions"
label var math_rd1 "He criticizes me when I cannot solve math problems well."
label var existence_rd1 "He praises me when I make progress in my study."
label var progress_rd1 "He gives answers patiently when I ask him questions."
label var praise_rd1 "I feel he does not concern my learning situation"
label var question_rd1 "I feel he does not care too much on my existence"
label var unconcerned_rd1 "He praises me when I do well in other things except for study"
label var criticize_rd1 "He criticizes me when I do not do well"
label var care_rd1 "He is very concerned about my feelings"
label var partial_rd1 "He shows partiality to some students"
label var clothes_rd1 "He despises me because of my dress"
label var appearance_rd1 "He despises me because of my appearance"
label var despise_rd1 "He despises me because of my family background"
label var talk_rd1 "He communicated with me."
label var ignore_rd1 "I feel he does not emphasize on me."
label var friendly_rd1 "He is very friendly to me"
label var fair_rd1 "He scores very fair"
label var same_rd1 "He does no distinguish between boys and girls"
label var suspect_rd1 "He suspect me"

label var hsco "high track"

label var control "contrl class"
label var treat1 "MS class"
label var treat2 "MSR class"
label var hgroup "height group"
label var desk "deskmate dummies"
label var class1 "class dummies"
label var grade1 "grade dummies"
label var sib "number of siblings"
label var age "age"
label var tchn1 "stanarized chinese score in final exam(last semester)"
label var tchn2 "stanarized chinese score in mid-term exam"
label var tchn3 "stanarized chinese score in final exam(this semester)"
label var tmath1 "stanarized math score in final exam(last semester)"
label var tmath2 "stanarized math score in mid-term exam"
label var tmath3 "stanarized math score in final exam(this semester)" 
label var totinc "total fimily income"

label var ave1 "average score baseline"
label var ave2 "average score midterm"
label var ave3 "average score endline"

label var row_rd1 "row number_rd1"
label var row_rd2 "row number_rd2"
label var col_rd1 "column number_rd1"
label var col_rd2 "column number_rd2"

label var chn1 "Chinese score baseline"
label var chn2 "Chinese score midterm"
label var chn3 "Chinese score endline"

label var math1 "Math score baseline"
label var math2 "Math score midterm"
label var math3 "Mathscore endline"

label var treatmentcontrol "treatment control status"

label var noncomplier "all non complier"
label var real_non "complier1"
label var real_non2 "complier2"

label var initial_desk "initial desk"
label var de_gender "deskmate's gender"

save final_bak, replace
