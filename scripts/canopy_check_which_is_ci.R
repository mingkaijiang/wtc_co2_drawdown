#### check which CO2 value is the Ci 
canopy_check_which_is_ci <- function(myDF) {
    with(myDF, plot(CO2Local~vCo2))
}

### change in CO2Local is the photosynthesis
### vCo2 is Ci

#descriptions	
#values calculated using SAS prog 	
#chamber	chamber number
#canopy	canopy layers present during drawdown
#vCo2	CO2 concentration in chamber
#vT	Air temp (by vaisala not shielded)
#vtime2	date and time 
#Tair	Air temp from WTC system
#DPLicorCh	Dew point in chamber updated every 14 mins
#PAR	PAR as measured by WTC system
#slope2	dCO2/dt umol tree-1 s-1
#cmarea	leaf area in chamber m2
#nslope2	normalised flux umol m-2 leaf s-1
#k	leak coefficient
#time	time of day
#leak	leak =(vco2-380)*k
#corrflux	leak corrected flux umol tree-1 s-1
#ncorrflux	normalised corrected flux umol m-2 leaf s-1#