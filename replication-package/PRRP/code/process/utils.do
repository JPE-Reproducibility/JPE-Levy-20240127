capture program drop checkVariable

program checkVariable

	global waves = "1 2 3 4 5 6 7 8 9 10 11 12"

	foreach wave of global waves {
	    quiet count if wave_abbr==`wave'
		local countAll = r(N)
		quiet count if missing(`1') & wave_abbr==`wave'
		local countMissing = r(N)
		if `countMissing'==`countAll' {
			display ("WARNING all obs in wave `wave' missing for `1'")
		}
		else if (`countMissing'/`countAll' > 0.1) {
			display ("WARNING more than 10% missing in wave `wave' for `1' ")
		}		
	}
	//check distinct **try to switch
	//quiet distinct `1'
	//assert r(ndistinct)>1 
end 
 

 
 
/*quiet count if wave_abbr==12
local countAll = r(N)
display `countAll'
quiet count if missing(X_dem_Athiest) & wave_abbr==12
local countMissing = r(N)
display `countMissing'*/
		