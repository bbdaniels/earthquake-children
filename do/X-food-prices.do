* Food prices data

use "${directory}/data/analysis_all.dta" , clear
  keep village_code vil_uc_dfl_mean
  duplicates drop
  count
  tempfile dist
    save `dist'

use "${directory}/data/prices.dta", clear
merge m:1 censusid using "${directory}/data/analysis_hh.dta" , keepusing(village_code) keep(3) nogen
merge m:1 village_code using `dist' , keepusing(vil_uc_dfl_mean) keep(3) nogen
keep censusid food_item mSec30_q2_units mSec30_q2_code mSec30_q3 village_code vil_uc_dfl_mean
bys food_item : egen mcom = mode(mSec30_q2_code)
  keep if mSec30_q2_code == mcom // Only use comparable prices (drop 3% of data)
  drop mcom mSec30_q2_code
  keep if !missing(mSec30_q2_units)

  encode food_item , gen(code)
    drop food_item

  collapse (mean) price = mSec30_q3  ///
    (firstnm) distance = vil_uc_dfl_mean ///
    [fweight = mSec30_q2_units] ///
    , by(code village_code)

  qui levelsof code , local(codes)
    foreach i in `codes' {
      local l`i' : label (code) `i'
    }

  reshape wide price , i(village_code) j(code)

  foreach i in `codes' {
    lab var price`i' "`l`i''"
  }

  lab var distance "Distance (km)"

  forest reg (price*) , t(distance) b bh sort(global) graphopts(ysize(7)) d

  graph export "${directory}/appendix/FAX_prices.png", replace width(4000)

* End
