** Master do-file for earthquake analysis

* Set directory: the location of the downloaded repository

global directory "/Users/bbdaniels/GitHub/earthquake-children"

/* For data preparation: experimental version of |iecodebook|

  global data "/Users/bbdaniels/Box/Earthquake/Constructed"
	qui do "${directory}/ado/iecodebook.ado"

  foreach dta in analysis_hh analysis_children analysis_all {

    iecodebook export ///
      "${data}/`dta'.dta" ///
    using "${directory}/data/`dta'.xlsx" ///
    , replace reset copy hash ///
      trim("${directory}/do/figures.do" ///
        "${directory}/do/tables.do" ///
        "${directory}/do/appendix-figures.do" ///
        "${directory}/do/appendix-tables.do")

  }
*/

* Graph scheme: https://graykimbrough.github.io/uncluttered-stata-graphs/

  cd "${directory}/ado/"
  set scheme uncluttered

* Load adofiles only from here

  sysdir set PLUS "${directory}/ado/"

  ssc install xml_tab  , replace
  net install grc1leg  , from(http://www.stata.com/users/vwiggins) replace
  net install st0085_2 , from(http://www.stata-journal.com/software/sj14-2) replace
  net install st0030_2 , from(http://www.stata-journal.com/software/sj5-4) replace
  net install st0364   , from(http://www.stata-journal.com/software/sj14-4) replace
  net install gr32_1   , from(http://www.stata-journal.com/software/sj4-3) replace
  run "${directory}/ado/betterbar.ado"
  run "${directory}/ado/xilab.ado"
  run "${directory}/ado/xiplus.ado"
  run "${directory}/ado/reftab.ado"

* Global options

	global graph_opts ///
		title(, justification(left) color(black) span pos(11)) ///
		graphregion(color(white) lc(white) lw(med) la(center)) /// <- Delete la(center) for version < 15
		ylab(,angle(0) nogrid) xtit(,placement(left) justification(left)) ///
		yscale(noline) xscale(noline) legend(region(lc(none) fc(none)))

	global comb_opts graphregion(color(white) lc(white) lw(med) la(center))
	global hist_opts ylab(, angle(0) axis(2)) yscale(noline alt axis(2)) ytit(, axis(2)) ytit(, axis(1)) yscale(off axis(2)) yscale(alt)
	global xpct `" 0 "0%" .25 "25%" .5 "50%" .75 "75%" 1 "100%" "'
	global numbering `""(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)" "(9)" "(10)""'

* Run all program files

  do "${directory}/do/figures.do"
  do "${directory}/do/tables.do"
  do "${directory}/do/appendix.do"

* End of master do-file
