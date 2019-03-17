* 03_ transition status model 

 
 
 // current time 
   di c(current_date)  "  "  c(current_time)
 // set up 
   clear all 
   set more off
   
 // root 
	global root "C:\Users\sdu7\Box Sync\miscellaneous\Hearing-Hypertension\Dataset\" 

  use  "$root\selected_wide_02.dta" , clear     

  
********************************************************
* 1hypertension variable checking *
********************************************************
* check bp status 



capture log close
log using bp_summary.log, replace 

**systolic hypertension   summary 

forvalues n = 1(1)5 {
sum sbpv`n'  , det 
}
**diastolic hypertension   summary 

forvalues n = 1(1)5 {
sum dbpv`n'  , det 
}

  
** Hypertensive status from v1 - v5 

forvalues n = 1(1)5 {
tab hypertv`n'  

} 
    capture log close 
  
  
  
 ** number of hypotensive individuals count  
 forvalues n = 1(1)5 { 
count   if sbpv`n'  < 90
} 

   forvalues n = 1(1)5 { 
count   if dbpv`n'  < 60
} 
  
   
  
  
  
********************************************************
* 2  hypotensive_sensitivity analysis
********************************************************

 
 
 
 *hypotensive 
 
 
 
 forvalues n = 1(1)6 { 
 capture drop hyper_hypo`n'
 gen hyper_hypo`n'  = hypertv`n' 
 replace hyper_hypo`n' = 2 if sbpv`n' < 90 | dbpv`n' < 60 & meduse`n' == 0 
 tab hyper_hypo`n'
 
 
 	label var hyper_hypo`n' " hyper_hypo tension status_v`n'"
	label values hyper_hypo`n'  hyper_hypo
	}
	
	
	label define  hyper_hypo  0 "normotensive" 										///
			1 "hypertensive" 									///
   2 "hypotensive"
 
 // check medicaiton use 
 
 
  
 // check medication use ? 
 
 forvalues n= 1(1)6 {
 tab meduse`n' ,missing 
 }
  
  
  
  capture log close
  log using hypo_hyper_drug.log , replace 
  
  
  * hypert and hypotension status and drug use 
  
  
   forvalues n= 1(1)6 {
 tab meduse`n' hyper_hypo`n', col   
 }
  
   
  
 capture log close 
 
 
 
 
 // midlife hypotensive 
 
   capture drop mid_hyper_hypo
  gen mid_hyper_hypo = 1 if ( hypertv1 ==1 & hypertv2 == 1)  | ( hypertv2 ==1 & hypertv3 == 1)  | ( hypertv3 ==1 & hypertv4 == 1) 
 replace mid_hyper_hypo`n' = 2 if  ( hyper_hypo1 == 2 & hyper_hypo2 == 2)  | ( hyper_hypo2 ==2 & hyper_hypo3 == 2)  | ( hyper_hypo3 ==2 & hyper_hypo4 == 2) 
  replace mid_hyper_hypo = 0 if missing(mid_hyper_hypo)   // all other scenarios goes to normotensive  

    
    	label var mid_hyper_hypo "midlife hypertion status"
	label values mid_hyper_hypo mid_hyper_hypo
	label define mid_hyper_hypo  0 "normotensive" 										///
			1 "hypertensive" 									///
   2 "hypotensive"
 
 
 
  tab mid_hyper_hypo 
  
  
  // check the health characteristics
  
  
   
 table1_mc, by(mid_hyper_hypo)  ///
 vars( vage1  contn  \  female bin \ racecenter cat\  education  cat\ diabts4 bin \ bmi4 contn \   cigt4 cat \ drnkr4 cat    )  ///
 format(%4.2f) saving(char_hypo.xls , replace)
  
  
  
  
 
  
  
  codebook mid_hypert 
  
    	label var mid_hypert "midlife hypertion status"
	label values mid_hypert mid_hypert
	label define late_hypert  0 "normotensive" 										///
			1 "hypertensive" 									///
 
 
 
  
 
********************************************************
*************** 3  BP classification ********
********************************************************

 
 
 
 
 	global root "C:\Users\sdu7\Box Sync\miscellaneous\Hearing-Hypertension\Dataset\" 

  use  "$root\selected_wide_02.dta" , clear     

  
 // midlife hypertensive  
   capture drop mid_hypert
     capture label drop mid_hypert
  gen mid_hypert = 1 if ( hypertv1 ==1 & hypertv2 == 1)  | ( hypertv2 ==1 & hypertv3 == 1)  | ( hypertv3 == 1 & hypertv4 == 1) 
  replace mid_hypert = 0 if missing(mid_hypert)   // all other scenarios goes to normotensive  
 
  
  tab mid_hypert 
  
  codebook mid_hypert 
  
    	label var mid_hypert "midlife hypertion status"
	label values mid_hypert mid_hypert
	label define mid_hypert  0 "normotensive" 										///
			1 "hypertensive" 									///
 
 
 
  
  
  // latelife hypertension status 
  capture label drop late_hypert
  capture drop late_hypert 
  gen late_hypert  =0 if hypertv5 ==0 
  replace late_hypert = 1 if hypertv5 ==1 
  replace late_hypert =2 if sbpv5 < 90 | dbpv5 < 60 
  
  codebook late_hypert 
  
  	label var late_hypert "late life hypertion status"
	label values late_hypert late_hypert
	label define late_hypert  0 "normotensive" 										///
			1 "hypertensive" 									///
			2 "hypotensive" 									///
			
  
  
  // gen groups 
  capture drop trans_pattern 
  egen trans_pattern =  group(mid_hypert late_hypert ) ,label 
  codebook trans_pattern 
  tab trans_pattern
  
  
  
********************************************************
*************** 3  Hypertension and PTA         ********
********************************************************

 
  
  //  BPTA mean 
  
  table(trans_pattern), c(mean bpta N bpta)
  
  graph hbox bpta, over(trans_pattern) 
  
  
  // model building 
  
  
 
 // only keep the pattern needed 
 
  keep if trans_pattern!= 4 
  

  regress bpta i.trans_pattern   
 est store bpta_model1 
 
  regress bpta i.trans_pattern   vage4  female i.racecenter i.education  
 est store bpta_model2 

 
 regress bpta i.trans_pattern   vage4  female   i.racecenter i.education  diabts4 bmi4  i.cigt4 drinkcat2    anynoise
 est store bpta_model3 

 estout bpta_model1 bpta_model2 bpta_model3 , ///
 	cells(   ( b(star fmt(%4.2f)) ci( fmt(%4.2f) )  p( fmt(%4.3f))  )  ) ///              
		stats(r2 N , fmt(3 0 1))   label   ///
		title("regression table for bpta and 5 hypertension transition patterns_ unadjusted & adjusted model ")
 
 
 
 
 
 
   set scheme s2color 
   
     
   // coefficient plot  
   
    coefplot (bpta_model1, offset(1)) (bpta_model2, offset(0))  (bpta_model3, offset( -1))  /// 
 ,xline(0)     ///
    drop(_cons)  msymbol(s , size(small))    ///
mlabel("beta = " + string(@b,"%9.2f" )  ) mlabposition(  2 )  legend(off) 

   graph export hearing_hypertension.png , replace 
   
   
   
   
	
 
    
 table1_mc, by(trans_pattern)  ///
 vars( vage1  contn  \  female bin \ racecenter cat\  education  cat\ diabts4 bin \ bmi4 contn \   cigt4 cat \ drnkr4 cat    )  ///
 format(%4.2f) saving(char_trans.xls , replace)
  
 
 
 
  
  /*
  
  discussion about med use, 
  hypertension value alone or binary hypertension ; 
  four tables : 1   2   3  4 
  
  
   
  
  */ 
  
   
  ********************************************************
***************  4 Quicksin & hearing loss  ********
********************************************************

  // QuickSIN 
  
    sum SNRloss ,det
  hist SNRloss, freq normal 
  
  ////////////
  
  table(trans_pattern),c(mean SNRloss N SNRloss ) 
  
  
  
  
  graph hbox SNRloss, over(trans_pattern) 
  
  
  // model building 
  
  
 
 // only keep the pattern needed 
 
 codebook  diabts4 bmi4  cigt4  drnkr4 
 
  keep if trans_pattern!= 4 
  

regress SNRloss i.trans_pattern   bpta
 est store model1 
   
 
   regress SNRloss i.trans_pattern bpta  vage4  female i.racecenter i.education    
 est store model2 

 
 regress SNRloss i.trans_pattern bpta  vage4  female i.racecenter i.education  diabts4 bmi4  i.cigt4 drinkcat2     anynoise  
 est store model3 
 
 
 /*
 
// matrix

mat list e(b) 
mat list e(V)
ereturn list all 
// macro 
 
 codebook trans_pattern
 global trans_label:  value label trans_pattern
 
macro list trans_label 

macro dir 

 
 
*continuous variables only:
sysuse auto, clear 

    . regress mpg weight price   headroom
    . regplot
	graph save reg, replace
	scatter mpg weight  || lowess mpg weight  || lfit mpg weight , legend(off) 
	graph save scatter, replace
	graph combine "reg" "scatter", altshrink    
	
	
    . gen weightsq = weight^2
    . regress mpg weight weightsq
    . regplot

*/



********************************************************
***************  model assumption check********
********************************************************
 
 
 
 // check residuals & model assumption
 
 capture drop res 
 predict res , residual 
 predict yhat 
 hist res, freq normal
 qnorm res 
 corr yhat SNRloss
 
 // 

  
		 
********************************************************
***************  5. Model results export into Excel ********
********************************************************
 
 // BPTA
				
				estout   bpta_model1 bpta_model2 bpta_model3  using Hypertension_BPTA_regression_table_results.xls ,   replace     ///
 	cells(   ( b(star fmt(%4.2f)) ci( fmt(%4.2f) )  p( fmt(%4.3f))  )  ) ///
	        stats(r2 N) label      ///		 
 		 			 varlabels(0b.education "Less than High school"  1.education "High School"  2.education "More than High School"    ///
1b.trans_pattern   "normotensive normotensive" ///
2.trans_pattern  "normotensive hypertensive" /// 
3.trans_pattern  "normotensive hypotensive"  ///
5.trans_pattern  "hypertensive hypertensive" ///
6.trans_pattern  "hypertensive hypotensive"  ///
0b.racecenter   "  Minneapolis W  "     ///                                  
1.racecenter    "     Jackson B   "     ///                                  
2.racecenter    "  Washington Co W"     ///                                  
3.racecenter    "     Forsyth B   "     ///                                  
4.racecenter    "     Forsyth W   "     ///                                  
0b.cigt4 "Never Smoker"   ///
1.cigt4  "Former Smoker"  ///
2.cigt4  "Current Smoker" ///
0b.drnkr4  "Never Drinker"  ///
1.drnkr4    "Former Drinker" ///
2.drnkr4    "Current Drinker" )
 
  
// SNR  
				
				estout  model1 model2  model3   using Hypertension_SNR_regression_table_results.xls ,   replace     ///
 	cells(   ( b(star fmt(%4.2f)) ci( fmt(%4.2f) )  p( fmt(%4.3f))  )  ) ///
	        stats(r2 N) label      ///		 
 		 			 varlabels(0b.education "Less than High school"  1.education "High School"  2.education "More than High School"    ///
1b.trans_pattern   "normotensive normotensive" ///
2.trans_pattern  "normotensive hypertensive" /// 
3.trans_pattern  "normotensive hypotensive"  ///
5.trans_pattern  "hypertensive hypertensive" ///
6.trans_pattern  "hypertensive hypotensive"  ///
0b.racecenter   "  Minneapolis W  "     ///                                  
1.racecenter    "     Jackson B   "     ///                                  
2.racecenter    "  Washington Co W"     ///                                  
3.racecenter    "     Forsyth B   "     ///                                  
4.racecenter    "     Forsyth W   "     ///                                  
0b.cigt4 "Never Smoker"   ///
1.cigt4  "Former Smoker"  ///
2.cigt4  "Current Smoker" ///
0b.drnkr4  "Never Drinker"  ///
1.drnkr4    "Former Drinker" ///
2.drnkr4    "Current Drinker" )
   /* 
 putexcel ///
 A3 = ( "normotensive normotensive" )   ///
 A4 = ( "normotensive hypertensive" ) ///
 A5 = ( "normotensive hypotensive"  ) ///
 A6 = ( "hypertensive hypertensive" ) ///
 A7 = ( "hypertensive hypotensive"  ) ///
 A8 = (  "Better PTA")  
 
 
  using SNRresults.xls
 
 */
********************************************************
***************  using outreg2 package ********
********************************************************
 
 
 // using outreg2 package 
 
 *  stats(..., labels(..., prefix(args)))
 
 
  xi: regress SNRloss i.trans_pattern bpta  vage1  female i.racecenter i.education  diabts4 bmi4  i.cigt4 drinkcat2     anynoise  
 est store model3 
 
  outreg2  using model3  ,  ///
  stats(   coef,   sd       ) label excel  // 
 


 * export excel  "$root\outreg_attempt2.xlsx",  sheet("outreg_attempt2")
   . putexcel set model3.xml,   modify  

  putexcel ///
 A3 = ( "normotensive normotensive" )   ///
 A4 = ( "normotensive hypertensive" ) ///
 A5 = ( "normotensive hypotensive"  ) ///
 A6 = ( "hypertensive hypertensive" ) ///
 A7 = ( "hypertensive hypotensive"  ) ///
 A8 = (  "Better PTA")  
 
 
 
  outreg2 using myfile, stats(coef, se, sd, cmd(variation: coefvar2, proportion))
  
  outreg2 using myfile, see excel: reg price mpg weight turn if price > 6100


 
 
   sysuse auto, clear
    reg price mpg rep78 head*
    lincom mpg + rep78
    local tstat=r(estimate)/r(se)
    local pval = tprob(r(df), abs(`tstat'))
    outreg2 using "myfile", adds(joint, r(estimate), t-stat, `tstat', p-val,`pval') replace see

 
 
 
 
 
 
 
 
 
 
 
 
 /*
 
 *** explore output format , labels, value labels etc
 
 
// extract value labels 
 
 decode trans_pattern, generate(trans_label)
replace trans_label = string(trans_pattern) + " - " + label
codebook trans_label

 
 
 outreg using  snr_trans.log, replace 
 type snr_trans.log
 
 
 
 
Online:  est, postfile, outfile, outsheet, save, modltbl, desrep



  
. estpost ci trans_pattern    ,   level(95) ///
saving est.xlsx, replace 
  
  
  
. esttab, cells("b  lb ub") label scalars(level)


esttab sim_lev_0 sim_lev_1 sim_lev_2 using "$tabs/table_lev.tex", replace ///
cells(b (star fmt(3)) se(par fmt(3) abs)) starlevels(* 0.1 ** 0.05 *** 0.01) ///
stats(N r2, fmt(%9.0f %9.2f) labels("N" "\$R^2$")) ///
nomtitles eqlabels(none) collabels(,none) label ///
addnote("`footnotes'") substitute(_ \_ LatexMath \$) ///
varlabels(`e(labels)')


 */
 
 
  ***************
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 