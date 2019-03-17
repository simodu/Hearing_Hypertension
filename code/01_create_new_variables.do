 
* 01 
* by Simo 
 * di "$S_DATE" "  " "$S_TIME"

 // current time 
   di c(current_date)  "  "  c(current_time)
 // set up 
   clear all 
   set more off
   
 // root 
	global root "C:\Users\sdu7\Box Sync\miscellaneous\Hearing-Hypertension\Dataset\" 
 //open master dataset 
	 use "$root\20181101_Hearing_V6.dta" ,clear
	 
	 
	 
********************************************************
********************************************************
 *1) HEARING VARS


capture drop  pta ptar ptal bpta bptacat bptacat4 ///
bptacat5 upta uptacat uptacat4 uptacat5 // drop potential existing vars with the same name 

  
 

// generate average PTA for right & left ear 

gen ptar=(aud4a3 + aud4a7 + aud4a9 + aud4a13)/4				//
	gen ptal=(aud4b3 + aud4b7 + aud4b9 + aud4b13)/4				//
		label var 												///
		ptar 													///
		"PTA Right"												//
	label var 													///
		ptal 													///
		"PTA Left"												//
 
codebook aud4a3	aud4a7 aud4a9 aud4a13 ptar ptal,com // check the vars
	
	
************** Bilateral Hearing Loss (better ear hearing  )*************
	
// generate continuous variable
gen bpta = min(ptar, ptal) ///
if !missing(ptar) & !missing(ptal)
label var bpta "better ear hearing"

/*
  method2_ those without right ear missing will be classied as better hearing 

// gen continuous var  

gen bpta=ptar
	replace bpta=ptal if ptal<ptar & !missing(ptar)
label var bpta "better ear hearing"
 */
 

// gen binary var 
gen bptacat = (bpta >25 ) if!missing(bpta)
		label var 												///
			bptacat 											///
			"PTA binary <=25 vs. >25"							//
label values bptacat bcat
label define bcat 1 "Hearing Impairment" 0 "Normal Hearing"



// gen 3& 4 & 5 categorical vars for better ear hearing 



	gen bptacat3 = 0 if bpta <=25 
	replace bptacat3 =1 if bpta >25  & bpta <= 40 
	replace bptacat3 =2 if bpta >40 & !missing(bpta) 
	
	label var bptacat3 "PTA 3 categories, better ear "
	label values bptacat3 cat3
	label define cat3  0 "<=25 db" 										///
			1 ">25 & <=40 db" 									///
			2 ">40 " 									///
			
	codebook bptacat3
	tab bptacat3
	
	
	
	
	gen bptacat4 = 0 if bpta <=25 
	replace bptacat4 =1 if bpta >25  & bpta <= 40 
	replace bptacat4 =2 if bpta >40 & bpta <=60 
	replace bptacat4 =3 if bpta > 60 & !missing(bpta)
	
	label var bptacat4 "PTA 4 categories, better ear "
	label values bptacat4 cat4
	label define cat4  0 "<=25 db" 										///
			1 ">25 & <=40 db" 									///
			2 ">40 & <=60 db" 									///
			3 ">60 db"	
	codebook bptacat4
	tab bptacat4
	
	
	
	
*** Hearing aid use ***		
gen																///
	hearaiduse=0												//
	replace														///
		hearaiduse=1											///
			if													///
			hne12=="Y"											///
			|													///
			hne15=="Y"											//		
	
	
********************************************************
********************************************************

	
	
/* (2) BLOOD PRESSURE	*/	


*** Systolic BP (Continuous) ***	
	rename sbpa21 sbpv1 										// 	
	rename sbpb21 sbpv2											//
	rename sbpc22 sbpv3											//
	rename sbpd19 sbpv4											//

*** Diastolic BP (Continuous) ***	
	rename sbpa22 dbpv1											// 	
	rename sbpb22 dbpv2											//
	rename sbpc23 dbpv3											//
	rename sbpd20 dbpv4 										//
	
*** Pulse pressure (Continuous) ***		
	forvalues i 												///
		=														///
		1/6														///
{																//
	gen															///
		pulsev`i'												///
		=														///
		(sbpv`i' - dbpv`i')										//
}
		
*** Medication (Self-report) ***	
	rename msra08a htnmedv1 									// 
	rename msrb24a htnmedv2										//
	rename msrc24a htnmedv3										//
	rename msrd24a htnmedv4										//	

*** Hypertension (ARIC defined, binary) ***	
	rename hypert05 hypertv1 									// 
	rename hypert25 hypertv2									//
	rename hypert35 hypertv3						            //
	rename hypert45 hypertv4                                    //
	rename hypert55 hypertv5                                    //
	rename hypert65 hypertv6       //

	
	
********************************************************
********************************************************
	
	* 3) MEDICATION 
 foreach v in msra1 msra2 msra3 msra4 msra5 msra6 {
 capture drop `v'
 } 
 
	* Not taking medication 
	
 rename msra02  msra1
 rename msrb02  msra2  	
 rename msrc2   msra3
 rename msrd2   msra4
 gen    msra5 = msra4
 rename msrf2   msra6
 
 
 	
********** generate medication use variables**********


  // gen medication use var 
   
  
forvalues i = 1(1)6 {
gen meduse`i' = 1 if  htnmedv`i' ==  "Y"  
replace meduse`i'  = 0 if  htnmedv`i' ==  "N" | msra`i' == "T"
sum meduse`i'
}	
	

********************************************************
********************************************************
	* Datea, time and baseline vars 
	
 */ 
	**** generate baseline variables
	
	 capture drop basesbp 
	 capture drop basedbp 
	 capture drop basepulse  

	gen basesbp  = sbpv1    
	gen basedbp  =  dbpv1  
	gen basepulse =   pulsev1 
	 
	
 
 
 rename v1date01 vdate1 
 rename v2date21 vdate2
 rename v3date31 vdate3 
 rename v4date41 vdate4 
 rename v5date51 vdate5 
 rename v6date61 vdate6
 
 
 
 
 
 
 //  generate baseline visit date for future use
gen basevisit = vdate1 
label var basevisit "baseline visit date "
format basevisit %td 
codebook basevisit
   
   
// gen mean date for each visit

/*


forvalues i = 1/6 {
egen mvdate`i' = mean(vdate`i')
}

  sum mvdate1 - mvdate6
  
  
  
// generate new vars 
*year 
*gen year = year(vdate)
*three groups based on baseline hypertension 


// gen mean sbpv
foreach v in sbpv dbpv pulsev {
bys  visit group: egen m`v' = mean(`v')
 } 

  
  
  */ 
  
********************************************************
********************************************************
	
	
/* (4) COVARIATES	*/	

*** Age ***
/*
gen v1date11 = v1date01
foreach i in 1 2 3 4 5 6 {
gen age`i'=(v`i'date`i'1-birthdat51)/365.25
}

*/ 


** age ****
gen vage1 = v1age01      
gen vage2 = v2age22      
gen vage3 = v3age31      
gen vage4 = v4age41      
gen vage5 = v5age51       
gen vage6 = v6age61   
 


*** Race-center ***
gen racecenter = .
	replace racecenter = 0 if center=="M" & racegrp =="W" 		// Minneapolis whites
	replace racecenter = 1 if center=="J"                       // Jackson blacks
	replace racecenter = 2 if center=="W" & racegrp =="W" 		// Washington Co whites
	replace racecenter = 3 if center=="F" & racegrp=="B"  		// Forsyth blacks
	replace racecenter = 4 if center=="F" & racegrp=="W"  		// Forsyth whites
		label values 											///
			racecenter 											///
			rclbl												//
		label define 											///
			rclbl 												///
			0 "Minneapolis W" 									///
			1 "Jackson B" 										///
			2 "Washington Co W" 								///
			3 "Forsyth B" 										///
			4 "Forsyth W"										//

* Delete if non-whites from Minneapolis and Washington Co:  
	replace racecenter=. if racecenter == 0 & racegrp == "B" 	// Minnesota
	replace racecenter=. if racecenter == 2 & racegrp == "B" 	// Wash co 

gen black=0 if racecenter==0 | racecenter==2 | racecenter==4
replace black=1 if racecenter==1 | racecenter==3

*** Sex ***
gen female = (gender == "F") if !missing(gender)

*** Education ***
gen education = elevel02 - 1

*** Noise Exposure ***
gen anynoise=0

replace anynoise=1 if 	///
	hne9=="Y" 	| 		///	job
	hne11=="Y"	|		///	leisure
	hne7=="Y"			//	firearm
tab anynoise 


*** BMI ***
rename bmi01 bmi1
rename bmi21 bmi2
rename bmi32 bmi3
rename bmi41 bmi4
rename bmi51 bmi5
rename bmi61 bmi6

*** Smoking Status ***
* Recode drinking and smoking status for missing
rename cigt01 cigt1
rename cigt21 cigt2
rename cigt31 cigt3
rename cigt41 cigt4
rename cigt52 cigt5

forvalue i=1/5 {
	recode cigt`i' (4=.)							//unknown
	replace cigt`i'=4 if cigt`i'==1					//current 1->4								
	replace cigt`i'=1 if cigt`i'==3					//never 3->1
	replace cigt`i'=3 if cigt`i'==4					//current (1->)4->3
	replace cigt`i'=cigt`i'-1						//unknown ., never 0, former 1, current 2
}
											
gen cigt6=1 if (cigt5==1 | cigt5==2) & cursmk62==0		//former smoker
replace cigt6=2 if cursmk62==1								//current smoker
replace cigt6=0 if missing(cigt6) & !missing(cursmk62)		//never

forvalue i=1/6 {

 label values cigt`i' cigt 
 }
 label define cigt 0 "Never Smoker"  1 "Former Smoker" 2 "Current Smoker"
 
 
 

/*
for reference:
never smoker - now smoking *** smoker (2)
never smoker - now not smoking *** never smoker (3)
never smoker - missing

former smoker - now smoking *** smoker (2)
former smoker - now not smoking *** former smoker (1)
former smoker - missing

current smoker - now smoking *** smoker (2)
current smoker - now not smoking *** former smoker (1)
current smoker - missing

missing - now smoking *** smoker (2)
missing - now not smoking *** never smoker (3) 
missing - missing
*/

rename drnkr01 drnkr1
rename drnkr21 drnkr2
rename drnkr31 drnkr3
rename drnkr41 drnkr4
rename drnkr51 drnkr5
rename drnkr61 drnkr6

forvalues i=1/6 {
recode drnkr`i' (4=.)							//unknown
replace drnkr`i'=4 if drnkr`i'==1				//current 1->4
replace drnkr`i'=1 if drnkr`i'==3				//never 3->1
replace drnkr`i'=3 if drnkr`i'==4				//current (1->)4->3
replace drnkr`i'=drnkr`i'-1						//unknown ., never 0, former 1, current 2
}


 // cluster  into 2 categories 
 
 capture drop drinkcat2 
 capture label drop drinkcat2
 recode drnkr4 ( 2 = 0) ( 0 = 1 ) , gen(drinkcat2)   // 
 
 
  label values drinkcat2 drinkcat2 
 
 label define drinkcat2 0 "Current Drinker"  1 "Former/Never Drinker" 

 
  tab drinkcat2
  
  
  
forvalue i=1/6 {

 label values drnkr`i' drnkr 
 }
 label define drnkr 0 "Never Drinker"  1 "Former Drinker" 2 "Current Drinker"
 
 

*** Diabetes ***
//We are just concerned about a cutoff of 126
gen diabts1 = diabts03 
gen diabts2 = diabts23 
gen diabts3 = diabts34 
gen diabts4 = diabts42 
gen diabts5 = diabts54 
gen diabts6 = diabts64 

replace diabts1= . if missing( diabts03  ) 
replace diabts2= . if missing( diabts23  )
replace diabts3= . if missing( diabts34  )
replace diabts4= . if missing( diabts42  )
replace diabts5= . if missing( diabts54  )
replace diabts6= . if missing( diabts64  )








replace pulsev2 = . if pulsev2 <0 

********************************************************
********************************************************


* 5) Quicksin 
**gen quicksin var 
capture drop SNRloss14 SNRloss17 SNRloss
/// Note: Formula (25.5 - the total scores) , for those who are curious about where 25.5 comes from, please check
///  https://drive.google.com/file/d/1fHaD-fsR7hXEEEgLHcqqfhLD66N_pLqw/view?usp=sharing

// track 14  & list 21
gen SNRloss14 = 25.5 - (aud5a+ aud5b+ aud5c+ aud5d + aud5e + aud5f)
//track 17  & list 15
gen SNRloss17 = 25.5 - (aud5g+ aud5h+ aud5i+ aud5j + aud5k + aud5l)

// average of lsit 12 & 15 _ final quicksin score
gen SNRloss = (SNRloss14 + SNRloss17)/2
label var SNRloss "Signal to noise ratio loss scores"

// check the variables
sum SNRloss SNRloss14 SNRloss17


 

save  "$root\full_sample_01.dta" ,replace 




 
 	 
 
 /*
 ******************************************************
 * NEW AHA Guidelines
 
 
*** Hypertension, 3-level category (Normo-, Pre-, Hypertension) ***	
	forvalues i 												///
		=														///
		1/6														///
{																//
	gen 														///
		hyp_3lvlv`i'											///
		=0														///
		if														///
			dbpv`i'<80											///
			&													///
			sbpv`i'<120											///
			&													///
			(htnmedv`i'=="N" | msra`i'  == "T" )   // add variable "reason for not taking meds"
	replace 													///
		hyp_3lvlv`i'											///
		=1														///
		if														///												///
			dbpv`i'<80											///
			&													///	
			sbpv`i'>=120										///
			&													///
			sbpv`i'<130											///
			&													///		
			(htnmedv`i'=="N" | msra`i'  == "T" )  //		 			
	replace 													///
		hyp_3lvlv`i'											///
		=2														///
		if														///
			(dbpv`i'>=80 & !missing(dbpv`i'))											///
			|													///	
			(sbpv`i'>=130 & !missing(sbpv`i'))									///
			|													///	
			htnmedv`i'=="Y"										//
	/*
	replace														///
		hyp_3lvlv`i'=.											///
		if														///
		missing(dbpv`i')										///
		|														///
		missing(sbpv`i')										///
		|														///
		htnmedv`i'==""											///
		|														///
		htnmedv`i'=="U"		//
	*/
	
		label values											///
			hyp_3lvlv`i' hyp_3lvllbl							//		
}
	
	
	label define											///
			hyp_3lvllbl											///
				0 "No hypertension"								///
				1 "Prehypertension"								///
				2 "Hypertension"								//

 
 
 
	
*** old guidelines *** 
*** Hypertension, 3-level category (Normo-, Pre-, Hypertension) ***	
	forvalues i 												///
		=														///
		1/6														///
{																//
	gen 														///
		prev_hyp_3lvlv`i'											///
		=0														///
		if														///
			dbpv`i'<90											///
			&													///
			sbpv`i'<140											///
			&													///
			(htnmedv`i'=="N" | msra`i'  == "T" )   // add variable "reason for not taking meds"
	replace 													///
		prev_hyp_3lvlv`i'											///
		=1														///
		if														///
			dbpv`i'>=80											///
			&													///
			dbpv`i'<90											///
			&													///	
			sbpv`i'>=120										///
			&													///
			sbpv`i'<140											///
			&													///		
			(htnmedv`i'=="N" | msra`i'  == "T" )  //		 			
	replace 													///
		prev_hyp_3lvlv`i'											///
		=2														///
		if														///
			(dbpv`i'>=90 & !missing(dbpv`i'))											///
			|													///	
			(sbpv`i'>=140 & !missing(sbpv`i'))									///
			|													///	
			htnmedv`i'=="Y"										//
	/*
	replace														///
		prev_hyp_3lvlv`i'=.											///
		if														///
		missing(dbpv`i')										///
		|														///
		missing(sbpv`i')										///
		|														///
		htnmedv`i'==""											///
		|														///
		htnmedv`i'=="U"		//
	*/
	
		label values											///
			prev_hyp_3lvlv`i' hyp_3lvllbl							//		
}
	