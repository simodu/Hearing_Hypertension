*02_ 



*** Included in Analytic Sample ***
 use "$root\full_sample_01.dta",clear 
 
	
/* Remove participatns without variables: blood pressure at visit 6 (sbpv6) and visit 4 (sbpv4) complete audiometry (bptacat4) */
drop if missing(vdate6)  // 4003 

 
// missing exposure 
 
forvalues n = 1(1)5 { 
drop if missing(hypertv`n')
}
drop if missing(sbpv5)  
drop if missing(dbpv5) // missing visit 5 BP values 

 

// missing outcome 
*drop if missing(bpta)  // 3626 
* drop if missing(SNRloss) 



/* 

// missing covariates 
drop if missing(gender) 
drop if missing(racecenter)				 
drop if missing(education)    
drop if missing(meduse1)
drop if missing(cigt1)  
drop if missing(bmi1)   
drop if missing(drnkr1) 
drop if missing(diabts1)  
 
*/  
 
 
/*
drop if missing(meduse1) &  missing(meduse2) &  missing(meduse3) &  missing(meduse4) &  missing(meduse5) &  missing(meduse6)   // missing meds 0 
drop if missing(cigt1)   &  missing(cigt2) &  missing(cigt3) &  missing(cigt4) &  missing(cigt5) &  missing(cigt6)   // missing cig
drop if missing(bmi1)    &  missing(bmi2) &  missing(bmi3) &  missing(bmi4) &  missing(bmi5) &  missing(bmi6)   // missing cig
drop if missing(drnkr1)  &  missing(drnkr2) &  missing(drnkr3) &  missing(drnkr4) &  missing(drnkr5) &  missing(drnkr6)   // missing cig
drop if missing(diabts1) &  missing(diabts2) &  missing(diabts3) &  missing(diabts4) &  missing(diabts5) &  missing(diabts6)   // missing cig
*/ 
// 3420




 
// keep the relavant variables
keep id vdate1 vdate2 vdate3 vdate4 vdate5 vdate6  /// 
 dbpv1 dbpv2 dbpv3 dbpv4 dbpv5 dbpv6 ///
 sbpv1 sbpv2 sbpv3 sbpv4 sbpv5 sbpv6  ///    
   pulsev1 pulsev2 pulsev3 pulsev4 pulsev5 pulsev6 ///
   hypertv1  hypertv2 hypertv3 hypertv4 hypertv5 hypertv6 ///  hypertension defination by ARIC standard 
   vage1 vage2 vage3 vage4 vage5 vage6  ///
      bpta bptacat bptacat3 bptacat4  hearaiduse /// hearing vars
	  SNRloss14 SNRloss17		  SNRloss   /// quicksin
       racecenter racegrp female  education   /// demo
         cigt1    cigt2 cigt3 cigt4 cigt5 cigt6  /// cigt
		 bmi1    bmi2 bmi3 bmi4 bmi5 bmi6 /// bmi
		 drnkr1   drnkr2 drnkr3 drnkr4 drnkr5 drnkr6  drinkcat2  /// drink
		 diabts1 diabts2 diabts3 diabts4 diabts5 diabts6 /// diabetes 
		  meduse1 meduse2 meduse3 meduse4 meduse5 meduse6  ///
		  basesbp basedbp basepulse /// baseline 
		  anynoise
 	 
   
 save  "$root\selected_wide_02.dta" ,replace 

  
    
  use  "$root\selected_wide_02.dta" , clear     

   

  // reshape from wide to long
reshape long   sbpv  dbpv  pulsev  hypertv  vage cigt  bmi drnkr  diabts meduse vdate   , i(id)  j(visit) 



 

   save  "$root\selected_long_03.dta" , replace   






