** Master do-file for earthquake analysis

* Set directory: the location of the /Earthquake/2015/ shared folder in Dropbox.

global directory "/Users/bbdaniels/Box/Earthquake"
global appendix "$directory/outputs/shock/appendix/"

* Adofiles: can be loaded this way (must be done each time Stata is reloaded) or installed to ado directory (must be done whenever files are updated).

	qui do "$directory/dofiles/adofiles/labelcollapse/labelcollapse.ado"
	qui do "$directory/dofiles/adofiles/anycategory/anycat.ado"
	qui do "$directory/dofiles/adofiles/xilabels/xilab.ado"
	qui do "$directory/dofiles/adofiles/xilabels/xiplus.ado"
	qui do "$directory/dofiles/adofiles/xilabels/xiplus_new.ado"
	qui do "$directory/dofiles/adofiles/referencecomparisons/reftab.ado"
	qui do "$directory/dofiles/adofiles/referencecomparisons/normdiff.ado"
	qui do "$directory/dofiles/adofiles/regressioncells/regcell.ado"
	qui do "$directory/dofiles/adofiles/tabgen/tabgen.ado"
	qui do "$directory/dofiles/adofiles/flowchart/flowchart.ado"
	// qui do "$directory/dofiles/adofiles/summarystatistics/sumstats.ado"
	qui do "$directory/dofiles/adofiles/freereshape/freeshape.ado"
	qui do "$directory/dofiles/adofiles/bootstrappoly/bstrappoly.ado"

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

* OK!
