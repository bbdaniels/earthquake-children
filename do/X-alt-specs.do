// Appendix tables. Alternative specifications of Table 3

// Version 1. Aid as control.
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "hh_aid_total i.indiv_male i.indiv_age"

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
  , save("${directory}/appendix/TX_alt_aid.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 hh_aid_total _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)


// Version 2. Control sensitivity
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  // Reference
  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_male i.indiv_age"

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg1a
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg1b
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

  // No controls
  local fault_controls ""
  local other_controls ""

  xi: reg indiv_health_zanthro_height hh_faultdist ///
    i.agecat*hh_faultdist `fault_controls' `other_controls' ///
  , cl(village_code)

    estimates store reg2a
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  xi: reg indiv_theta_mean hh_faultdist ///
    `fault_controls' `other_controls' ///
  if indiv_age >= 9 ///
  , cl(village_code)

    estimates store reg2b
    su `e(depvar)' if e(sample)
    estadd scalar mean = `r(mean)'

  // District
  local fault_controls "hh_district_1 hh_district_2 hh_district_3"
  local other_controls ""

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg3a
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg3b
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

   // Geo
   local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
   local other_controls ""

     xi: reg indiv_health_zanthro_height hh_faultdist ///
       i.agecat*hh_faultdist `fault_controls' `other_controls' ///
     , cl(village_code)

       estimates store reg4a
       su `e(depvar)' if e(sample)
       estadd scalar mean = `r(mean)'

     xi: reg indiv_theta_mean hh_faultdist ///
       `fault_controls' `other_controls' ///
     if indiv_age >= 9 ///
     , cl(village_code)

       estimates store reg4b
       su `e(depvar)' if e(sample)
       estadd scalar mean = `r(mean)'

  // Age
  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_age"

    xi: reg indiv_health_zanthro_height hh_faultdist ///
      i.agecat*hh_faultdist `fault_controls' `other_controls' ///
    , cl(village_code)

      estimates store reg5a
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'

    xi: reg indiv_theta_mean hh_faultdist ///
      `fault_controls' `other_controls' ///
    if indiv_age >= 9 ///
    , cl(village_code)

      estimates store reg5b
      su `e(depvar)' if e(sample)
      estadd scalar mean = `r(mean)'


  xml_tab reg1a reg1b reg2a reg2b reg3a reg3b reg4a reg4b reg5a reg5b ///
  , save("${directory}/appendix/TX_alt_con.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist _IageXhh_fa_1 _IageXhh_fa_2)

// Version 3. Intensity replacing distance.
use "${directory}/data/analysis_children.dta", clear
  keep if m_missing == 0 & indiv_age < 16

  replace hh_faultdist = hh_intensity

  char indiv_age[omit] 9

  local fault_controls "hh_epidist hh_slope hh_fault_minimum hh_district_1 hh_district_2 hh_district_3"
  local other_controls "i.indiv_male i.indiv_age"

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
  , save("${directory}/appendix/TX_alt_mercalli.xls") ///
    replace below c("Constant") stats(mean r2 N) ///
    lines(COL_NAMES 3 LAST_ROW 3 _cons 2) format((SCLB0) (SCCB0 NCRR3 NCRI3)) drop(o.*) ///
    keep(hh_faultdist disr _IageXhh_fa_1 _IageXhh_fa_2 _Iindiv_mal_1 _IindXhh_fa_1 _IindX*)

// End
