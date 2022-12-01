cd "..\..\Stillbirth"

********************************************************************************
*Stata code for "Antepartum and intrapartum stillbirth rates across gestation: 
*				 a cross-sectional study using the revised foetal death reporting
*				 system in the U.S." by Ncube, CN et al. (2022)
* Download 2014 public use fetal death data file
* Import the ASCII file into Stata
* Use the User Guide to the 2014 fetal death public use file to create code for 
*		variable labels
* Save file as fetaldeaths2014_labeled.dta
********************************************************************************


use fetaldeaths2014_labeled.dta, clear
********************************************************************************
**** Apply inclusion/exclusion criteria: 
***** Step 1: remove foreign residents, deaths <20 weeks
*****		  remove observations from places not reporting timing of fetal death 
*****		  result is N=20,953 observations which matches # in public use file
***** Step 2: keep singletons
*****		  keep those with obstetric estimate of gestation 24-43 weeks
********************************************************************************

drop if restatus==4 /*exclude foreign residents*/
keep if oe_tabflg==2 /*all fetal deaths are >=20 weeks gestation*/
keep if f_estofd==1 /*estimate of timing of fetal death reported*/

keep if dplural==1 /*singletons*/
keep if oegest_comb >=24 & oegest_comb<=43 /*OE combined estimate of gest 24-43*/

save fetaldeaths2014_labeled2.dta, replace

********************************************************************************
**** A) Recode and create new variables
********************************************************************************

* Set up 
clear
set more 1
cap log close

* Open dataset
use fetaldeaths2014_labeled2.dta,clear

* Create ID for each individual obvservation 
gen id = _n

* Review categorical variables 
tab1 estofd_2 mracerec umhisp precare_rec rf_diab rf_gest rf_phyp rf_ghyp rf_eclam bfacil ubfacil bfacil3 attend, m

gen estofd_3 = estofd_2
label variable estofd_3 "estimated time of fetal death 3"
recode estofd_3 (3=2) (2=3) (4=3)
#delimit ;
label define estofd_3_l 
	1 "Antepartum death; not in labor at time of presentation with fetal death" 
	2 "Intrapartum death; baby alive at initial assessment then died in labor" 
	3 "Not determined" ;
#delimit cr
label values estofd_3 estofd_3_l

gen estofd_4 = estofd_3
replace estofd_4 = . if estofd_3==3
recode estofd_4 (1=0) (2=1) 
label define estofd_4_l 0 "antepartum" 1 "intrapartum"
label variable estofd_4 "Antepartum and intrapartum time of death"
label values estofd_4 estofd_4_l

* Sociodemographic & behavioral variables 
gen mrace_hisp = .
replace mrace_hisp = 0 if mracerec==1
replace mrace_hisp = 1 if mracerec==2
replace mrace_hisp = 2 if mracerec==3
replace mrace_hisp = 3 if mracerec==4
replace mrace_hisp = 4 if umhisp>=1 & umhisp !=9
label define mrace_hisp_l 0 "white" 1 "black" 2 "AIAN" 3 "API" 4 "Hispanic"
label variable mrace_hisp "Mother's race/ethnicity"
label values mrace_hisp mrace_hisp_l
tab mrace_hisp

egen mager4_2 = cut(mager), at(11, 20, 25, 35, 51) icodes 
tabstat mager, by(mager4_2) stat(n min mean max) 
label variable mager4_2 "Mother's age recode 4.2"
label define mager4_2_l 0 "<20" 1 "20-24" 2 "25-34" 3 ">= 35"
label values mager4_2 mager4_2_l

egen meduc4 = cut(meduc), at(0, 3, 4, 6, 9, 10) icodes
tab meduc4 meduc 
recode meduc4 (.=4)
label variable meduc4 "Mother's education recode 4, revised"
label define meduc4_l 0 "< High school" 1 "High school/GED" 2 "Some college/Associate's" ///
	3 "At least a Bachelor's" 4 "Unknown"
label values meduc4 meduc4_l

gen meduc5 = meduc4
recode meduc5 (3=0) (0=1) (2=1) (4=2)
label define meduc5_l 0 "At least a Bachelor's" 1 "Less than a Bachelor's" 2 "Unknown"
label values meduc5 meduc5_l
label variable meduc5 "Maternal education, Bachelors versus none"

gen priorlive_2 = priorlive
recode priorlive_2 (99=.)
gen priordead_2 = priordead
recode priordead_2 (99=.)
egen livebirths = rowtotal(priorlive_2 priordead_2), missing
replace livebirths = . if priorlive_2==. & priordead_2==0
replace livebirths = . if priordead_2==. & priorlive_2==0 
recode livebirths (.=99)
egen livebirths2 = cut(livebirths), at(0, 1, 20, 100) icodes 
tabstat livebirths, by(livebirths2) stat(n min mean max) missing 
label variable livebirths2 "Live births recode 2"
label define livebirths2_l 0 "no prior live birth" 1 "1+ prior live birth" 2 "Unknown"
label values livebirths2 livebirths2_l

egen precare3 = cut(precare_rec), at(1, 2, 4, 5, 6) icodes 
tab precare3 precare_rec
recode precare3 (.=3)
label variable precare3 "Month prenatal care began recode 3, revised"
label define precare3_l 0 "1st trimester" 1 "2nd trimester+" 2 "No care" 3 "Unknown"
label values precare3 precare3_l

gen cig_rec_yes=cig_rec=="Y" if cig_rec!="U"

* Clinical indicator variables 
gen iicod_3 = .
label variable iicod_3 "ICD-PM initiating cause of death"
#delimit ;
replace iicod_3 = 1 if
	iicod=="Q000" | iicod=="Q002" | iicod=="Q019" | iicod=="Q02" | iicod=="Q030" |
	iicod=="Q031" | iicod=="Q038" | iicod=="Q039" | iicod=="Q040" | iicod=="Q042" |
	iicod=="Q043" | iicod=="Q046" | iicod=="Q048" | iicod=="Q049" | iicod=="Q054" | iicod=="Q059" |
	iicod=="Q070" | iicod=="Q079" | iicod=="Q174" | iicod=="Q188" | iicod=="Q189" |
	iicod=="Q200" | iicod=="Q203" | iicod=="Q210" | iicod=="Q211" | iicod=="Q212" |
	iicod=="Q213" | iicod=="Q222" | iicod=="Q224" | iicod=="Q225" | iicod=="Q230" |
	iicod=="Q234" | iicod=="Q241" | iicod=="Q246" | iicod=="Q248" | iicod=="Q249" |
	iicod=="Q251" | iicod=="Q253" | iicod=="Q255" | iicod=="Q270" | iicod=="Q279" |
	iicod=="Q283" | iicod=="Q289" | iicod=="Q301" | iicod=="Q309" | iicod=="Q321" |
	iicod=="Q330" | iicod=="Q336" | iicod=="Q339" | iicod=="Q348" | iicod=="Q359" |
	iicod=="Q369" | iicod=="Q378" | iicod=="Q379" | iicod=="Q390" | iicod=="Q402" |
	iicod=="Q403" | iicod=="Q410" | iicod=="Q438" | iicod=="Q439" | iicod=="Q512" |
	iicod=="Q513" | iicod=="Q519" | iicod=="Q564" | iicod=="Q600" | iicod=="Q601" |
	iicod=="Q602" | iicod=="Q605" | iicod=="Q606" | iicod=="Q611" | iicod=="Q613" |
	iicod=="Q614" | iicod=="Q615" | iicod=="Q619" | iicod=="Q620" | iicod=="Q623" |
	iicod=="Q633" | iicod=="Q639" | iicod=="Q641" | iicod=="Q642" |
	iicod=="Q643" | iicod=="Q645" | iicod=="Q647" | iicod=="Q648" | iicod=="Q668" |
	iicod=="Q672" | iicod=="Q675" | iicod=="Q678" | iicod=="Q688" | iicod=="Q692" |
	iicod=="Q703" | iicod=="Q713" | iicod=="Q740" | iicod=="Q743" | iicod=="Q749" |
	iicod=="Q752" | iicod=="Q758" | iicod=="Q759" | iicod=="Q764" | iicod=="Q766" |
	iicod=="Q771" | iicod=="Q774" | iicod=="Q780" | iicod=="Q782" | iicod=="Q785" |
	iicod=="Q789" | iicod=="Q790" | iicod=="Q791" | iicod=="Q792" | iicod=="Q793" |
	iicod=="Q795" | iicod=="Q796" | iicod=="Q798" | iicod=="Q799" | iicod=="Q851" |
	iicod=="Q870" | iicod=="Q871" | iicod=="Q874" | iicod=="Q878" | iicod=="Q891" |
	iicod=="Q892" | iicod=="Q894" | iicod=="Q897" | iicod=="Q898" | iicod=="Q899" |
	iicod=="Q909" | iicod=="Q913" | iicod=="Q917" | iicod=="Q921" | iicod=="Q927" |
	iicod=="Q928" | iicod=="Q929" | iicod=="Q933" | iicod=="Q935" |
	iicod=="Q938" | iicod=="Q939" | iicod=="Q960" | iicod=="Q968" | iicod=="Q969" |
	iicod=="Q970" | iicod=="Q971" | iicod=="Q991" | iicod=="Q998" | iicod=="Q999" ;

replace iicod_3 = 2 if 
	iicod=="A50" | iicod=="P351" | iicod=="P353" | iicod=="P358" | 
	iicod=="P394" | iicod=="P398" | iicod=="P399" ;

replace iicod_3 = 3 if 
	estofd_3==1 & iicod=="P209" ;
	
replace iicod_3 = 4 if 
	estofd_3==2 & iicod=="P209" ;
	
replace iicod_3 = 5 if
	iicod=="P500" | iicod=="P501" | iicod=="P504" | iicod=="P509" | 
	iicod=="P524" | iicod=="P526" | iicod=="P529" |
	iicod=="P550" | iicod=="P551" | iicod=="P558" | iicod=="P559" |
	iicod=="P610" | iicod=="P614" |
	iicod=="P700" | iicod=="P701" | iicod=="P702" |
	iicod=="P832" |	iicod=="P833" |
	iicod=="P964" ;

replace iicod_3 = 6 if
	estofd_3==1 & (iicod=="P051" | iicod=="P052" | iicod=="P059" |
	iicod=="P080" | iicod=="P081" | iicod=="P082") ;

replace iicod_3 = 7 if 
	estofd_3==2 & (iicod=="P051" | iicod=="P052" | iicod=="P059" |
	iicod=="P072" | iicod=="P073" |
	iicod=="P080" | iicod=="P081" | iicod=="P082") ;

replace iicod_3 = 8 if 
	iicod=="P95" ;
	
replace iicod_3 = 9 if 
	estofd_3==2 & (iicod=="P104" | iicod=="P108" | iicod=="P128" | iicod=="P158") ;

replace iicod_3 = 10 if
	iicod=="P020" | iicod=="P021" | iicod=="P022" | iicod=="P023" | iicod=="P024" |
	iicod=="P025" | iicod=="P026" | iicod=="P027" | iicod=="P028" ;

replace iicod_3 = 11 if 
	iicod=="P010" | iicod=="P011" | iicod=="P012" | iicod=="P013" | iicod=="P014" |
	iicod=="P015" | iicod=="P016" | iicod=="P017" | iicod=="P018" ;

replace iicod_3 = 12 if
	iicod=="P030" | iicod=="P031" | iicod=="P035" | iicod=="P036" | iicod=="P038" |
	iicod=="P039" ;

replace iicod_3 = 13 if 
	iicod=="P000" | iicod=="P001" | iicod=="P002" | iicod=="P003" | 
	iicod=="P004" | iicod=="P005" | iicod=="P006" | iicod=="P007" |
	iicod=="P008" | iicod=="P009" ;

label define icd10_pm_l 
	1 "Congenital malformations, deformations & chromosomal abnormalities"
	2 "Infections"
	3 "Antepartum hypoxia"
	4 "Acute intrapartum event"
	5 "Other specified antepartum/intrapartum disorder"
	6 "Disorders related to fetal growth"
	7 "Disorders related to fetal growth"
	8 "Death of unspecified cause"
	9 "Birth trauma"
	10 "Complications of placenta, cord & membranes"
	11 "Maternal complications of pregnancy"
	12 "Other complications of labor & delivery"
	13 "Maternal medical & surgical conditions" ;
	
label values iicod_3 icd10_pm_l ; 
#delimit cr

gen     prepreg_diab = 0 if rf_diab == "N"
replace prepreg_diab = 1 if rf_diab == "Y"
tab  rf_diab prepreg_diab, m

gen     gest_diab = 0 if rf_gest == "N"
replace gest_diab = 1 if rf_gest == "Y"
tab  rf_gest gest_diab, m

gen     prepreg_hyp = 0 if rf_phyp == "N"
replace prepreg_hyp = 1 if rf_phyp == "Y"
tab  rf_phyp prepreg_hyp, m

gen     gest_hyp = 0 if rf_ghyp == "N"
replace gest_hyp = 1 if rf_ghyp == "Y"
tab  rf_ghyp gest_hyp, m

gen     eclamps = 0 if rf_eclam == "N"
replace eclamps = 1 if rf_eclam == "Y"
tab  rf_eclam eclamps, m

* Facility type variable 
gen inhospital = 0 if bfacil3 == 2
replace inhospital = 1 if bfacil3 == 1
tab inhospital 

tab bfacil3 inhospital, m

* Attendant variable
gen     attendance = . 
replace attendance = 1 if attend == 1
replace attendance = 2 if attend == 2
replace attendance = 3 if (attend == 3 | attend == 4 | attend == 5)

label define attendance 1 "Doctor of Medicine" 2 "Doctor of Osteopathy" 3 "Midwife or other"
label values attendance attendance 

tab attendance
tab attend attendance, m

* Set non responses to missing 
replace meduc=. if meduc==9
replace meduc5=. if meduc5==2

replace livebirths=. if livebirths==99
replace livebirths2=. if livebirths2==2

replace precare=. if precare==99
replace precare3=. if precare3==3

replace bmi=. if bmi==99.9
replace bmi_r=. if bmi_r==9

* Save as tempfile
tempfile data
save `data'

************************************************
**** A.1) Run Univariate Models
************************************************
foreach var in sex_2 mager4_2 meduc5 livebirths2 precare3 cig_rec_yes bmi_r mrace_hisp prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps inhospital attendance {
tab `var' estofd_2, col
}

********************************************************************************
**** B) Mulitple Imputation of Categorical Variables
********************************************************************************

/* First, create a new variable with a copy of the outcome- the point 
   of this is to be able to see which observations had missing outcome data 
   after imputation since imputation will fill in the missingess */
gen estofd_4_copy = estofd_4
tab estofd_4_copy, m

* Preserve dataset and keep variables of interest
preserve
keep id meduc5 livebirths2 precare3 bmi_r estofd_4 mager4_2 sex_2 mrace_hisp 	///
     cig_rec_yes estofd_4_copy prepreg_diab gest_diab prepreg_hyp gest_hyp 		///
	 eclamps inhospital attendance

* Run imputation 
mi set flong
set seed 29390
mi register imputed meduc5 livebirths2 precare3 bmi_r cig_rec_yes estofd_4 		///
                    prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
					inhospital attendance

mi impute chained (ologit) meduc5 bmi_r  (mlogit) precare3 attendance  			///
         (logit) estofd_4 livebirths2 cig_rec_yes prepreg_diab gest_diab 		///
		 prepreg_hyp gest_hyp eclamps inhospital = mager4_2 sex_2 mrace_hisp, 	///
		 add(100) augment

/* Exclude observations originally missing the outcome from each imputed dataset
   Note: This final analytic population is the number of individuals with 
   non-missing outcomes, but all individuals were included in the imputation. */
drop if estofd_4_copy == . 

* Save dataset 
save "..\..\Data\mi_impute2.dta", replace


************************************************
**** B.1) Run Bivariate Models
************************************************

mi estimate, post: glm estofd_4 i.sex_2, family(bin) link(log) eform	
outreg2 using "..\..\Output\Table2.xls", ci noparen eform
				
mi estimate, post: glm estofd_4 ib1.mager4_2, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.meduc5,  family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib1.livebirths2, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib0.precare3, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 cig_rec_yes, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib2.bmi_r, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.mrace_hisp, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 prepreg_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 gest_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 prepreg_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 gest_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 eclamps, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 inhospital, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.attendance, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform
   

************************************************
**** B.2) Run Multivariate Models
************************************************
																			
mi estimate, post: glm estofd_4 i.sex_2 ib1.mager4_2 i.meduc5 ib1.livebirths2 	///
                          ib0.precare3 cig_rec_yes ib2.bmi_r i.mrace_hisp 		///
						  prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 	///
						  inhospital i.attendance, family(bin) link(log) eform
outreg2 using "..\..\Output\Table2.xls", ci noparen eform

restore


********************************************************************************
**** C) Sensitivity Analysis 1: Impute for outcome & covariates
********************************************************************************
* Use dataset
use `data',clear

* Review outcome data
tab estofd_4, m // 5403 missing 

* Run imputation 
preserve
keep id meduc5 livebirths2 precare3 bmi_r estofd_4 mager4_2 sex_2 mrace_hisp 	///
        cig_rec_yes prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
		inhospital attendance
		
mi set flong
set seed 29390
mi register imputed meduc5 livebirths2 precare3 bmi_r cig_rec_yes estofd_4 		///
                    prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
					inhospital attendance
					
mi impute chained (ologit) meduc5 bmi_r  (mlogit) precare3 attendance 			///
		   (logit) estofd_4 livebirths2 cig_rec_yes  prepreg_diab gest_diab 	///
		   prepreg_hyp gest_hyp eclamps = mager4_2 sex_2 mrace_hisp, 			///
		   add(100) augment 

* Save dataset 
save "..\..\Data\mi_impute3.dta", replace
*use "..\..\Data\mi_impute3.dta", clear

************************************************
**** C.1) Run Bivariate Models
************************************************

mi estimate, post: glm estofd_4 i.sex_2, family(bin) link(log) eform	
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib1.mager4_2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.meduc5,  family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib1.livebirths2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib0.precare3, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 cig_rec_yes, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 ib2.bmi_r, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.mrace_hisp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 prepreg_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 gest_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 prepreg_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 gest_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 eclamps, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 inhospital, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

mi estimate, post: glm estofd_4 i.attendance, family(bin) link(log) eform
outreg2 using "..\..\Output\SA1.xls", ci noparen eform
   

************************************************
**** C.2) Run Multivariate Models
************************************************
																			
mi estimate, post: glm estofd_4 i.sex_2 ib1.mager4_2 i.meduc5 ib1.livebirths2 	///
                          ib0.precare3 cig_rec_yes ib2.bmi_r i.mrace_hisp  		///
						  prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 	///
						  inhospital i.attendance, family(bin) link(log) eform
						  
outreg2 using "..\..\Output\SA1.xls", ci noparen eform

restore


********************************************************************************
**** D) Sensitivity Analysis 2: Collapse unknown into AP and impute for covariates
********************************************************************************

* Use original dataset
use `data', clear

* Recode outcome
recode estofd_4 (. 0 = 0 "Antepartum") (1 = 1 "Intrapartum"), gen(estofd_5) 
tab estofd_4 estofd_5, m

* Run imputation
preserve
keep id meduc5 livebirths2 precare3 bmi_r estofd_5 mager4_2 sex_2 mrace_hisp 	///
        cig_rec_yes prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
		inhospital attendance
		
mi set flong
set seed 29390
mi register imputed meduc5 livebirths2 precare3 bmi_r cig_rec_yes prepreg_diab  ///
                    gest_diab prepreg_hyp gest_hyp eclamps inhospital attendance

mi impute chained (ologit) meduc5 bmi_r  (mlogit) precare3 attendance			///
         (logit) livebirths2 cig_rec_yes  prepreg_diab gest_diab prepreg_hyp    ///
		 gest_hyp eclamps =  estofd_5 mager4_2 sex_2 mrace_hisp, 				///
		 add(100) augment

* Save dataset 
save "..\..\Data\mi_impute4.dta", replace


************************************************
**** D.1) Run Bivariate Models
************************************************
mi estimate, post: glm estofd_5 i.sex_2,family(bin) link(log) eform			
outreg2 using "..\..\Output\SA2.xls", ci noparen eform
		
mi estimate, post: glm estofd_5 ib1.mager4_2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 i.meduc5,  family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 ib1.livebirths2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 ib0.precare3, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 cig_rec_yes, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 ib2.bmi_r, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 i.mrace_hisp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 prepreg_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 gest_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 prepreg_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 gest_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 eclamps, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 inhospital, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform

mi estimate, post: glm estofd_5 i.attendance, family(bin) link(log) eform
outreg2 using "..\..\Output\SA2.xls", ci noparen eform
   

************************************************
**** D.2) Run Multivariate Models
************************************************ 
																		
mi estimate, post: glm estofd_5 i.sex_2 ib1.mager4_2 i.meduc5 ib1.livebirths2 	///
                          ib0.precare3 cig_rec_yes ib2.bmi_r i.mrace_hisp		///
						  prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 	///
						  inhospital i.attendance, 								///
						  family(bin) link(log) eform
						  
outreg2 using "..\..\Output\SA2.xls", ci noparen eform



********************************************************************************
**** E) Sensitivity Analysis 3: Remove CM from all deaths and run with MI 
********************************************************************************

* Use original dataset
use `data', clear

* Remove congenital malformations from all deaths 
gen estofd_6_nocm = 0 if estofd_4 == 0
	* Just include IP deaths not due to CM
	replace estofd_6_nocm = 1 if estofd_4 == 1 & iicod_3 != 1
	* Set to missing AP deaths due to CM
	replace estofd_6_nocm = . if estofd_4 == 0 & iicod_3 == 1
	replace estofd_6_nocm = . if estofd_4 == .
tab estofd_6_nocm
tab estofd_6_nocm estofd_4, m
tab estofd_6_nocm iicod_3 , m

tab estofd_6_nocm, m

gen estofd_6_nocm_copy = estofd_6_nocm

* Run imputation
preserve
keep id meduc5 livebirths2 precare3 bmi_r estofd_6_nocm mager4_2 sex_2 			///
        mrace_hisp cig_rec_yes estofd_6_nocm_copy prepreg_diab gest_diab 		///
		prepreg_hyp gest_hyp eclamps inhospital attendance

* Run imputation
mi set flong
set seed 29390
mi register imputed meduc5 livebirths2 precare3 bmi_r cig_rec_yes estofd_6_nocm ///
                    prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
					inhospital attendance


mi impute chained (ologit) meduc5 bmi_r (mlogit) precare3 attendance 			///
    (logit) estofd_6_nocm livebirths2 cig_rec_yes prepreg_diab 					///
	gest_diab prepreg_hyp gest_hyp eclamps  = mager4_2 sex_2 mrace_hisp, 		///
	add(100) augment 

/* Exclude observations originally missing the outcome from each imputed dataset
   Note: This final analytic population is the number of women with 
   non-missing outcomes, but all women were included in the imputation. */
drop if estofd_6_nocm_copy == . // 545,703 observations dropped

* Save dataset 
save "..\..\Data\mi_impute5.dta", replace


************************************************
**** E.1) Run Bivariate Models
************************************************

mi estimate, post: glm estofd_6_nocm i.sex_2,family(bin) link(log) eform	
outreg2 using "..\..\Output\SA3.xls", ci noparen eform
				
mi estimate, post: glm estofd_6_nocm ib1.mager4_2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm i.meduc5,  family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm ib1.livebirths2, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm ib0.precare3, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm cig_rec_yes, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm ib2.bmi_r, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm i.mrace_hisp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm prepreg_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm gest_diab, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm prepreg_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm gest_hyp, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm eclamps, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm inhospital, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

mi estimate, post: glm estofd_6_nocm i.attendance, family(bin) link(log) eform
outreg2 using "..\..\Output\SA3.xls", ci noparen eform


************************************************
**** E.2) Run Multivariate Models
************************************************
																			
mi estimate, post: glm estofd_6_nocm i.sex_2 ib1.mager4_2 i.meduc5 ib1.livebirths2 ///
                       ib0.precare3 cig_rec_yes ib2.bmi_r i.mrace_hisp			///
					   prepreg_diab gest_diab prepreg_hyp gest_hyp eclamps 		///
					   inhospital i.attendance, family(bin) link(log) eform
					   
outreg2 using "..\..\Output\SA3.xls", ci noparen eform

restore



