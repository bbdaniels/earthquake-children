use "${directory}/data/analysis_children.dta", clear

gen cohort = indiv_age == 3

lab var indiv_father_age "Father's Age"
lab var indiv_father_edu "Father Completed Primary School"

foreach var in indiv_father_age indiv_father_edu indiv_father_height m_age m_indiv_edu_binary m_indiv_health_height ///
  hh_perm_house_pre hh_stats_electricity_pre hh_water_inhouse_pre hh_stats_privateschool_pre hh_stats_govtschool_pre hh_assets_pca_pre {

  local lab : var label `var'
  
  preserve
  keep if hh_near_quake == 1
  collapse (mean) mean = `var' (sem) sem = `var' (count) n = `var' , by(indiv_age)
    gen ul = mean + (1.96 * sem)
    gen ll = mean - (1.96 * sem)
    
    gen check = inlist(indiv_age,3,4,5,6)
    
    tw ///
      (lfitci mean indiv_age [aweight = n] if !check , lw(none) fc(gs12)) ///
      (lfit mean indiv_age [aweight = n] if !check , lp(dash) lw(thin) lc(black)) ///
      (rspike ll ul indiv_age if check  , lc(red)) ///
      (rspike ll ul indiv_age if !check , lc(black)) ///
      (scatter mean indiv_age , mc (black)) ///
    , title("`lab'") xtit("Survey Age Cohort") xlab(0(1)18) ///
      saving("${directory}/appendix/`var'.gph", replace) nodraw
  restore
  
  local graphs `"`graphs' "${directory}/appendix/`var'.gph""'

  /*
  qui reg `var' indiv_age#i.cohort if hh_near_quake
  qui margins indiv_age#cohort
  marginsplot , title("`lab'") saving("${directory}/appendix/`var'.gph", replace) nodraw ///
    plot1opts(mc(black) lc(gray)) ci1opts(lc(gray) msize(0)) ///
    plot2opts(mc(black) lc(red)) ci2opts(lc(red)) ///
    xtit("Survey Age Cohort") xline(3, lc(gray) lw(thin)) ytit(" ")
  */
}

graph combine `graphs' , c(3) ysize(6) altshrink
graph export "${directory}/appendix/FAX_cohort.png" , width(4000) replace

-

// Version 1. Aid as control.
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  replace hh_aid_total = hh_aid_total/100000
    lab var hh_aid_total "Total Cash Aid Reported (PKR 100,000s)"

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "indiv_father_age indiv_father_edu indiv_father_height m_age m_indiv_edu_binary m_indiv_health_height hh_perm_house_pre hh_stats_electricity_pre hh_water_inhouse_pre hh_stats_privateschool_pre hh_stats_govtschool_pre hh_assets_pca_pre i.indiv_male i.indiv_age"

  xi: reg indiv_health_zanthro_weight hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg1
    su `e(depvar)' if e(sample) == 1
    estadd scalar mean = `r(mean)'

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_school_enrolled_post hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg3
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_edu hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_tested == 1 & indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg4
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg5
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    disr `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg6
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_male*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg7
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    i.indiv_age*hh_faultdist `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg8
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xml_tab reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 ///
  , save("${directory}/appendix/TA4X_birth.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)
