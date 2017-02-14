setwd("~/Desktop/Research/IMSA2017-Phenology/")
library(ncdf4)
MA <- nc_open("data/Daymet_Quercus/MortonArb/Daymet.1980.nc")

dat.ma <- data.frame(doy=1:365,
                     site="MortonArb",
                     lat=ncvar_get(MA, "latitude"),
                     lon=ncvar_get(MA, "longitude"),
                     tmax=ncvar_get(MA, "maximum_air_temperature"),
                     tmin=ncvar_get(MA, "minimum_air_temperature"),
                     prcp=ncvar_get(MA, "precipitation_flux"),
                     srad=ncvar_get(MA, "surface_downwelling_shortwave_flux_in_air"),
                     swe =ncvar_get(MA, "liquid_water_content_of_surface_snow"),
                     dayl=ncvar_get(MA, "day_length"),
                     vp  =ncvar_get(MA, "water_vapor_partial_pressure_in_air"))
nc_close(MA)


q09 <- nc_open("data/Daymet_Quercus/Quercus009/Daymet.1980.nc")

dat.q09 <- data.frame(doy=1:365,
                     site="Quercus009",
                     lat=ncvar_get(q09, "latitude"),
                     lon=ncvar_get(q09, "longitude"),
                     tmax=ncvar_get(q09, "maximum_air_temperature"),
                     tmin=ncvar_get(q09, "minimum_air_temperature"),
                     prcp=ncvar_get(q09, "precipitation_flux"),
                     srad=ncvar_get(q09, "surface_downwelling_shortwave_flux_in_air"),
                     swe =ncvar_get(q09, "liquid_water_content_of_surface_snow"),
                     dayl=ncvar_get(q09, "day_length"),
                     vp  =ncvar_get(q09, "water_vapor_partial_pressure_in_air"))
nc_close(q09)


dat.plot <- rbind(dat.ma, dat.q09)
summary(dat.plot )

library(ggplot2)
ggplot(data=dat.plot) + geom_line(aes(x=doy, y=tmax, color=site))

ggplot(data=dat.plot) + facet_grid(site~.) + geom_bar(aes(x=doy, y=prcp), fill="blue", stat="identity")
ggplot(data=dat.plot) + facet_grid(site~.) + geom_bar(aes(x=doy, y=swe), fill="lightblue", stat="identity")
ggplot(data=dat.plot) + geom_line(aes(x=doy, y=srad, color=site))
ggplot(data=dat.plot) + geom_line(aes(x=doy, y=dayl, color=site))
ggplot(data=dat.plot) + geom_line(aes(x=doy, y=vp, color=site))
