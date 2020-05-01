cap prog drop xilab
prog def xilab

syntax anything , [three] [perm]

if "`three'" != "" local three 3

qui xi`three' `anything'

foreach var of varlist `_dta[__xi__Vars__To__Drop__]' {
	
	local theLogic : var label `var'
	
	local theLogic = subinstr("`theLogic'","&","",.)
	local theLogic = subinstr("`theLogic'","*"," ",.)
	
	local n_logic : word count `theLogic'
	
		local theNewLabel ""
		local and ""
		
		forvalues i = 1/`n_logic' {
		
			local theNextLogic : word `i' of `theLogic'
		
			if (strpos("`theNextLogic'","==") > 0) | (strpos("`theNextLogic'","=") > 0) { // Check for categorical
		
				local theNextLogic = subinstr("`theNextLogic'","=="," ",.)
				local theNextLogic = subinstr("`theNextLogic'","="," ",.)
				local theNextLogic = subinstr("`theNextLogic'","*"," ",.)
				local theNextLogic = subinstr("`theNextLogic'","("," ",.)
				local theNextLogic = subinstr("`theNextLogic'",")"," ",.)
				local theVar : word 1 of `theNextLogic'
				local theVal : word 2 of `theNextLogic'
				
				local theLab : label (`theVar') `theVal'
				
				local theNewLabel `theNewLabel' `and' `theLab'
					local and "&"
				
				}
			
			else { // for continuous
			
					local and "*"
					
					local theNextLogic = subinstr("`theNextLogic'","=="," ",.)
					local theNextLogic = subinstr("`theNextLogic'","="," ",.)
					local theNextLogic = subinstr("`theNextLogic'","*"," ",.)
					local theNextLogic = subinstr("`theNextLogic'","("," ",.)
					local theNextLogic = subinstr("`theNextLogic'",")"," ",.)
					local theVar : word 1 of `theNextLogic'
					local theVal : word 2 of `theNextLogic'
					
					local theLab : var label `theVar'
					
					local theNewLabel `theNewLabel' `and' `theLab'
						
						
					}
			}
					
		label var `var' "`theNewLabel'"
		
		qui if "`perm'" != "" {
			local theNewName = subinstr("`var'","_I","",1)
			cap clonevar `theNewName' = `var'
				drop `var'
				
			local theVarlist "`theVarlist' `theNewName'"
			}
			
	}
	
	if "`perm'" != "" codebook `theVarlist', compact
	
end
