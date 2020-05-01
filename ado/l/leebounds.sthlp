{smcl}
{* *! version 1.0  3mar2011}{...}
{cmd:help leebounds}{right: ({browse "http://www.stata-journal.com/article.html?article=st0364":SJ14-4: st0364})}
{hline}

{title:Title}

{p2colset 5 20 22 2}{...}
{phang}
{bf:leebounds} {hline 2} Lee (2009) treatment-effect bounds{p_end}
{p2colreset}{...}


{title:Syntax}

{p 8 17 2}
{cmd:leebounds}
{depvar} {it:treatvar}  {ifin} {weight} [{cmd:,} {it:{help leebounds##options:options}}]

{phang}
{it:depvar} specifies the outcome variable.

{phang}
{it:treatvar} specifies a binary variable, indicating receipt of treatment.
Estimating the effect of {it:treatvar} on {it:depvar} is subject of the
empirical analysis.  The (alphanumerically) larger value of {it:treatvar} is
assumed to indicate treatment.

{synoptset 26 tabbed}{...}
{synopthdr :options}
{synoptline}
{p2coldent:{cmdab:sel:ect(}{it:varname}{cmd:)}}selection indicator {p_end}
{p2coldent:{cmdab:tig:ht(}{it:varlist)}}covariates for tightened bounds {p_end}
{p2coldent:{opt cie:ffect}}compute confidence interval for treatment effect {p_end}
{p2coldent:{cmd:vce(}{cmdab:ana:lytic}|{help bootstrap:{ul on}{cmd:boot}{ul off}{cmd:strap}}{cmd:)}}compute analytic or bootstrapped standard errors; default is {opt vce}{cmd:(analytic)}{p_end}
{p2coldent:{opt lev:el(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{opt pweight}s, {opt fweight}s, and {opt iweight}s are allowed; see {help weight}.  Observations with negative weight are skipped for any weight type.{p_end}
{p 4 6 2}{cmd:bootstrap} is allowed; see {help prefix}.{p_end}


{title:Description}

{pstd}
{cmd:leebounds} computes treatment-effect bounds for samples with
nonrandom sample selection or attrition, as proposed by Lee (2009).  The
lower and upper bound correspond to extreme assumptions
about the missing information that are consistent with the observed
data.  As opposed to parametric approaches to correcting for
sample-selection bias, such as the classical Heckman (1979) estimator,
Lee (2009) bounds rest on very few assumptions, that is, random
assignment of treatment and monotonicity.  Monotonicity means that the
treatment status affects selection in just one direction.  That is,
receiving a treatment makes selection either more or less likely for any
observation.  In technical terms, the approach rests on a trimming
procedure. Either from below or from above, the group (treatment,
control) that suffers less from sample attrition is trimmed at the
quantile of the outcome variable that corresponds to the share of excess
observations in this group.  Calculating group differentials in mean
outcome yields the lower and the upper bound, respectively, for the
treatment effect depending on whether trimming is from below or above.


{marker options}{...}
{title:Options}

{phang}
{opt select(varname)} specifies a binary selection indicator.
{it:varname} can either be numeric or a string variable.  The
(alphanumerically) larger value of {it:varname} is assumed to indicate
selection.  If no selection indicator {it:varname} is specified, any
observation with nonmissing information on {it:depvar} is assumed to be
selected, and all observations with missing information on {it:depvar}
are assumed to be not selected.

{phang}
{opt tight(varlist)} specifies a list of covariates for computing
tightened bounds.  With {opt tight()} specified, the sample is split into
cells defined by the covariates in {it:varlist}.  
Continuous variables in {it:varlist} will cause the estimation to fail.

{phang}
{opt cieffect} requests calculation of a confidence interval for the
treatment effect.  This interval captures both uncertainty about the selection
bias and uncertainty about the sampling error.

{phang}
{cmd:vce(analytic}|{cmd:bootstrap)} specifies whether analytic or bootstrapped
standard errors are calculated for estimated bounds.  {cmd:analytic} is
the default.  {cmd:bootstrap} allows for the suboptions {opt reps(#)} and
{opt nodots}; see {help bootstrap:bootstrap}.  For {cmd: vce(analytic)},
the covariance for the estimated lower and upper bound is not computed.
If this covariance is relevant, one should choose {cmd:vce(bootstrap)}.  Instead of specifying {cmd: vce(bootstrap)}, one can
use the {help prefix:prefix} command {cmd:bootstrap},
which allows for numerous additional options.  Yet {cmd:leebounds}'s
internal bootstrapping routine is much faster than the prefix command,
allows for sampling weights by performing a weighted bootstrap, and
makes the option {opt cieffect} use bootstrapped standard errors.

{phang}
{cmd:level(}{it:#}{cmd:)} sets confidence level. See {helpb estimation options##level():[R] estimation options}. One can change the reported confidence level by retyping
{cmd:leebounds} without arguments and specifying only the option {cmd:level(}{it:#}{cmd:)}.  This affects the confidence interval for the
bounds, but it does not affect the confidence interval requested with {opt cieffect}.


{title:Examples}

{pstd}Basic syntax{p_end}
{phang2}{cmd:. leebounds wage training}{p_end}

{pstd}Tightened Lee bounds with weighted bootstrap and treatment-effect confidence interval{p_end}
{phang2}{cmd:. leebounds wage training [pw=1/prob], select(wageinfo) tight(female immigrant) cieffect vce(bootstrap, reps(250) nodots)}{p_end}


{title:Stored results}

{pstd}
{cmd:leebounds} stores the following in {cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(Nsel)}}number of selected observations{p_end}
{synopt:{cmd:e(cilower)}}lower bound of treatment-effect confidence interval
(if option {opt cieffect} was specified){p_end}
{synopt:{cmd:e(ciupper)}}upper bound of treatment-effect confidence interval
(if option {opt cieffect} was specified){p_end}
{synopt:{cmd:e(trim)}}(overall) trimming proportion{p_end}
{synopt:{cmd:e(level)}}confidence level{p_end}
{synopt:{cmd:e(cells)}}number of cells (if option {opt tight()} was specified){p_end}
{synopt:{cmd:e(N_reps)}}number of bootstrap repetitions (if option {cmd:vce(bootstrap)} was specified){p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:leebounds}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(title)}}title in estimation output{p_end}
{synopt:{cmd:e(depvar)}}name of dependent variable{p_end}
{synopt:{cmd:e(treatment)}}binary treatment indicator{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(select)}}{it:varname} (if option {opt select()} was specified){p_end}
{synopt:{cmd:e(cellsel)}}cell-specific selection pattern, {opt homo}
or {opt hetero} (if option {cmd:tight()} was specified){p_end}
{synopt:{cmd:e(covariates)}}{it:varlist} (if option {opt tight()} was
specified){p_end}
{synopt:{cmd:e(trimmed)}}{opt treatment} or {opt control}{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(properties)}}{opt b V}{p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}vector of estimated treatment-effect bounds{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of the estimators (covariance
set to zero for {cmd:vce(analytic)}){p_end}

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}
{p2colreset}{...}


{title:References}

{phang}
Heckman, J. J. 1979. Sample selection bias as a specification error. {it: Econometrica} 47: 153-161.

{phang}
Imbens, G. W., and C. F. Manski. 2004. Confidence intervals for partially identified parameters. {it: Econometrica} 72: 1845-1857.

{phang}
Lee, D. S. 2009. Training, wages, and sample selection: Estimating sharp bounds on treatment effects. {it: Review of Economic Studies} 76: 1071-1102. 


{title:Acknowledgment}

{pstd}
This work has been supported in part by the Collaborative Research
Center "Statistical Modelling of Nonlinear Dynamic Processes" (SFB 823)
of the German Research Foundation (DFG).  {p_end}


{title:Author}

{pstd}Harald Tauchmann{p_end}
{pstd}University of Erlangen-Nuremberg{p_end}
{pstd}Nürnberg, Germany{p_end}
{pstd}harald.tauchmann@wiso.uni-erlangen.de{p_end}


{title:Also see}

{p 4 14 2}Article:  {it:Stata Journal}, volume 14, number 3: {browse "http://www.stata-journal.com/article.html?article=st0364":st0364}

{p 7 14 2}Help:  {manhelp heckman R}, 
 {helpb bpbounds}, {helpb bpboundsi}, {helpb mhbounds} (if installed){p_end}
