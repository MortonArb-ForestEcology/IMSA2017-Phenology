# Function to extract Daymet for point locations
# THREDDS example file path for scraping data via HTTP
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1980/daymet_v3_prcp_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_tmax_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_tmin_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_dayl_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_srad_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_swe_1981_na.nc4
# https://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1328/1981/daymet_v3_vp_1981_na.nc4

# Daymet variables available: 
# ---------------------------------------------------
# Code    | Units        | Description
# ------- | ------------ | --------------------------
# dayl    | seconds      | day length in seconds
# prcp    | mm/day       | daily total preciptiation
# srad    | W/m2         | daylight; mean incident shortwave radiation
# swe     | kg/m2(/day?) | snow water equivalent (if precip is rain; swe = prcp)
# tmax    | C            | daily maximum temperature
# tmin    | C            | daily minimum temperature
# vp      | Pa           | daily mean vapor pressure
# ---------------------------------------------------