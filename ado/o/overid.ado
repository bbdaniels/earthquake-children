*! overid V1.6.5 C F Baum, Vince Wiggins, Steve Stillman, Mark Schaffer
*  overid V1.1   C F Baum 9B19
* Ref: Davidson and MacKinnon, Estimation and Inference in Econometrics, p.236
*      Wooldridge, Econometric Analysis of Cross-Section and Panel Data, p.123
* V1.3: add handling of weights, guard against use with robust
* V1.35: correct handling of inst list 
* V1.4.0: adds support for xtivreg, fe
* V1.4.1: standard error msg re xtivreg
* V1.4.2: move to ivreg 5.00.09 for collinear instruments correction 
* V1.4.3: fixed bug with lagged covariates in xtivreg
* V1.4.4: suppress constant in aux reg if no constant in orig eqn, per ivreg2
* V1.5.0: comment xtivreg code, enable use after ivreg2
* V1.6.0: added Sargan2, Basmann, and pseudo-F versions of Sargan and Basmann
* V1.6.1: added GMM-style robust overid statistic including cluster
*         and handling of all weight types (except iweights not allowed with robust)
* V1.6.2: fixed nocons bug
* V1.6.3: removed incorrect robust overid stat and robust/cluster options,
*         including pweights
* V1.6.4: prevent execution if N < # of instruments
* V1.6.5: remove test for 5.00.09 since Stata 9 does not define e(version)

program define overid, rclass
	version 7.0
	local version 1.6.5
	syntax [, chi2 dfr f all]

	if "`e(cmd)'" ~= "ivreg" & "`e(cmd)'" ~= "ivreg2" {
		di in r "overid works only after ivreg, ivreg2; use overidxt after xtivreg"
		error 301
	}
	
*	if "`e(cmd)'" == "ivreg" & "`e(version)'" < "05.00.09" {
*		di in red "overid requires version 5.0.9 or later of ivreg"
*		di in red "type -update query- and follow the instructions" /*
*			*/ " to update your Stata"
*		exit 198
*	}

	if "`e(vcetype)'" == "Robust" {
		di in red "Test not valid with robust covariance matrix: use ivreg2"
		exit 198
	}

* pweight is equivalent to aweight+robust and hence not allowed
	local weight ""
	if "`e(wexp)'" != "" {
		if "`e(wtype)'"=="pweight" {
		di in red "test not valid with pweights"
		exit 198
		}
		else {
			local weight "[`e(wtype)'`e(wexp)']"
		}
        }

* Default is Sargan and Basmann chi2
	if "`chi2'`dfr'`f'`all'" == "" {
		local chi2 "chi2"
	}

	tempname res iota Nprec K L inst touse sargan regest rssiv

    /* determine whether _cons in original list (includes ,noc hanscons) */
	tempname b 
	mat `b' = e(b) 
	local x : colnames `b'
	local x : subinstr local x "_cons" "_cons" , word count(local hc)
	if `hc' == 0 { local noc "noc" }	

   	                 	/* instrument list */
	local inst `e(insts)'
	local rssiv = e(rss)
	gen byte `touse' = e(sample)

				/* fetch residuals */
	qui predict double `res' if `touse', res

* Nprec is ob count from mat accum.  Use this rather than e(N) in calculations
* because e(N) is rounded if iweights are used and because summarize
* won't work with iweights.
	qui gen `iota'=1
	qui matrix accum `b' = `iota' `weight' if `touse'
	scalar `Nprec'=`b'[1,1]

* L=number of non-collinear regressors
* Count includes constant if it exists
	mat `b'=diag(e(b))
   	local L=colsof(`b')-diag0cnt(`b')

* Regress IV residuals on instrument list
	capture {
		estimates hold `regest'
		regress `res' `inst' `weight' if `touse', `noc'
		scalar `sargan' = `Nprec'*(1.0-e(rss)/`rssiv')
* K=number of non-collinear instruments
* Count includes constant if it exists
		mat `b'=diag(e(b))
   		local K=colsof(`b')-diag0cnt(`b')
		local enn = `e(N)'
		estimates unhold `regest'
	}
	
* check that number of observations exceeds number of instruments	
		if `enn' <= `K' {
			di as err "Error: cannot calculate test when number of instruments"
			di as err "       equals or exceeds number of observations"
			error 2001
		}

* Calculate degree of overid
	return scalar df = `K'-`L'
	if return(df) == 0 {
		di in red _n "There are no overidentifying restrictions."
		exit 
	}
	return scalar dfu=`Nprec'-`K'
	return scalar dfr=`Nprec'-`L'
	return scalar N = e(N)

di in gr _n "Tests of overidentifying restrictions:"

if "`chi2'`all'" != "" {
		return scalar sargan = `sargan'
		return scalar sarganp= chiprob(return(df),return(sargan))
		di in gr _c "Sargan N*R-sq test " /*
			*/ in ye _col(25) %7.3f return(sargan) in gr            /* 
			*/ in gr "  Chi-sq(" %1.0f in ye return(df)    /*
			*/ in gr ")" _col(47) "P-value = " in ye %5.4f return(sarganp)
		di
		}

if "`dfr'`all'" != "" {
		return scalar sargan2 = `sargan'*return(dfr)/`Nprec'
		return scalar sargan2p= chiprob(return(df),return(sargan2))
		di in gr _c "Sargan (N-L)*R-sq test " /*
			*/ in ye _col(25) %7.3f return(sargan2) in gr            /* 
			*/ in gr "  Chi-sq(" %1.0f in ye return(df)    /*
			*/ in gr ")" _col(47) "P-value = " in ye %5.4f return(sargan2p)
		di
		}


if "`chi2'`dfr'`all'" != "" {
		return scalar basmann = /*
			*/ `sargan'*return(dfu)/(`Nprec'-`sargan')
		return scalar basmannp= chiprob(return(df),return(basmann))
		di in gr _c "Basmann test " /*
			*/ in ye _col(25) %7.3f return(basmann) in gr            /* 
			*/ in gr "  Chi-sq(" %1.0f in ye return(df)    /*
			*/ in gr ")" _col(47) "P-value = " in ye %5.4f return(basmannp)
		di
		}

if "`f'`all'" != "" {
		return scalar sarganf = /*
			*/ `sargan'/`Nprec'*return(dfr)/return(df)
		return scalar sarganfp= Ftail(return(df),return(dfr),return(sarganf))
		di in gr _c "Sargan pseudo-F test " /*
			*/ in ye _col(25) %7.3f return(sarganf) in gr            /* 
			*/ in gr "  F(" %1.0f in ye return(df)    /*
			*/ in gr "," %1.0f in ye return(dfr)         /*
			*/ in gr ")" _col(47) "P-value = " in ye %5.4f return(sarganfp)
		di
		return scalar basmannf = /*
			*/ `sargan'*return(dfu)/(`Nprec'-`sargan')/return(df)
		return scalar basmannfp= Ftail(return(df),return(dfu),return(basmannf))
		di in gr _c "Basmann pseudo-F test " /*
			*/ in ye _col(25) %7.3f return(basmannf) in gr            /* 
			*/ in gr "  F(" %1.0f in ye return(df)    /*
			*/ in gr "," %1.0f in ye return(dfu)         /*
			*/ in gr ")" _col(47) "P-value = " in ye %5.4f return(basmannfp)
		di
		}

end
exit
