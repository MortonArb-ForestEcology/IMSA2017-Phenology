##' Download and convery to CF Daymet single grid point from ORNL server using OPENDAP interface
##' @name download.Daymet
##' @title download.Daymet
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param site_id
##' @param lat
##' @param lon
##' @param ext 
##' @param vars
##' 
##' @author Christy Rollinson
##' 
##' Note: this is modeled after the Pecan standards, but adapted so you can chose 
##' which variables to extract as well as extract an entire range
##' Daymet variables available: 
##' # ---------------------------------------------------
##' # Code    | Units        | Description
##' # ------- | ------------ | --------------------------
##' # dayl    | seconds      | day length in seconds
##' # prcp    | mm/day       | daily total preciptiation
##' # srad    | W/m2         | daylight; mean incident shortwave radiation
##' # swe     | kg/m2(/day?) | snow water equivalent (if precip is rain; swe = prcp)
##' # tmax    | C            | daily maximum temperature
##' # tmin    | C            | daily minimum temperature
##' # vp      | Pa           | daily mean vapor pressure
# ---------------------------------------------------

download.Daymet <- function(outfolder, start_date, end_date, site_id=NULL, lat.in=NULL, lon.in=NULL, ext=NULL,
                            vars=c("dayl", "prcp", "srad", "swe", "tmax", "tmin", "vp"),
                            overwrite = FALSE, verbose = FALSE, ...) {
  library(lubridate); library(stringr); library(ncdf4)
  library(rgdal)
  
  if(is.null(lat.in) & is.null(lon.in) & (is.null(ext) | length(ext)<4)) stop("Provide points or extent to extract (xmin, xmax, ymin, ymax)")
  if(length(lat.in) != length(lon.in)) stop("length of lat.in does not match lenght of lon.in")
  if(!is.null(site_id) & length(site_id) != length(lat.in)) stop("site IDs do not match number of points!")
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year <- lubridate::year(end_date)
  
  if(start_year < 1980){ 
    warning("Daymet doesn't go earlier than 1980, resetting start year to 1980-01-01")
    start_date <- as.POSIXlt("1980-01-01", tz = "UTC")
    start_year <- lubridate::year(start_date)
  }

  #  outfolder <- paste0(outfolder, "_site_", paste0(site_id%/%1e+09, "-", site_id %% 1e+09))
  
  lat.in <- as.numeric(lat.in)
  lon.in <- as.numeric(lon.in)
  
  # Note: Daymet is in Lambert Conformal Conic, NOT lat/lon 
  # and because the data is so HUGE, we need to transform our files to that 
  # to make our lives a lot easier
  # If we're working with a point or series of points, make a point file
  if(is.null(ext)){ # we have points
    if(is.null(site_id)) site_id <- paste0("site", str_pad(1:length(lat.in), nchar(length(lat.in)), pad="0"))
    
    pts.ll <- data.frame(lon=lon.in, lat=lat.in, site=site_id)
    pts.ll <- SpatialPointsDataFrame(coords=pts.ll[,c("lon", "lat")], data=pts.ll, proj4string=CRS("+proj=longlat"))
    
    pts.lcc <- spTransform(pts.ll, CRS("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84"))
    # summary(pts.lcc)
    
    # If you ever need a demo of the difference in projections:
    # plot(pts.ll, pch=19, cex=0.5)
    # plot(pts.lcc, pch=19, cex=0.5)
    
    x.lcc <- coordinates(pts.lcc)[,1]
    y.lcc <- coordinates(pts.lcc)[,2]
  } else { # we're pulling an area
    if(is.null(site_id)) site_id <- "site1"
    # ext=c(lon.in[1], lon.in[2], lat.in[2], lat.in[1])
    poly.pts = matrix(c(ext[1], ext[3],
                        ext[1], ext[4],
                        ext[2], ext[4],
                        ext[2], ext[3],
                        ext[1], ext[3]),
                      ncol=2, byrow=T)
    
    poly1 <- Polygon(coords=poly.pts)
    poly.ll <- SpatialPolygons(list(Polygons(list(poly1), ID="area1")), proj4string=CRS("+proj=longlat"))
    poly.lcc <- spTransform(poly.ll, CRS("+init=epsg:26978"))
    
    # If you ever need a demo of the difference in projections:
    # plot(poly.ll)
    # plot(poly.lcc)
  }
  
  
   # https://thredds.daac.ornl.gov/thredds/catalog/ornldaac/1328/catalog.html
  dap_base <- "https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/"
  
  if(!dir.exists(outfolder)) dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  ylist <- seq(start_year, end_year, by = 1)
  rows <- length(ylist)
  # results <- data.frame(file = character(rows), 
  #                       host = character(rows), 
  #                       mimetype = character(rows), 
  #                       formatname = character(rows), 
  #                       startdate = character(rows), 
  #                       enddate = character(rows), 
  #                       dbfile.name = "Daymet", 
  #                       stringsAsFactors = FALSE)
  
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
  # Note: many of these don't actually have CF standards (like daylength and max/min temperature)
  var <- data.frame(DAP.name = c("tmax", "tmin", "prcp", "srad", "swe", "dayl", "vp"),
                    CF.name = c("maximum_air_temperature", "minimum_air_temperature", "precipitation_flux", "surface_downwelling_shortwave_flux_in_air", "liquid_water_content_of_surface_snow", "day_length", "water_vapor_partial_pressure_in_air"),
                    DAP.units=c("C", "C", "mm/day", "W/m2", "kg/m2", "s", "Pa"),
                    units = c("K", "K", "kg/m2/s", "W/m2", "kg/m2", "s", "Pa")
                    )

  var <- var[var$DAP.name %in% vars,]
  
  # Establish the lat & lon indices
  # https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/1980/daymet_v3_prcp_1980_na.nc4
  file.name <- paste0("daymet_v3_", var$DAP.name[1], "_", start_year, "_na.nc4")
  dap_file <- paste0(dap_base, start_year, "/", file.name)
  dap <- nc_open(dap_file)

  daymet.x <- ncvar_get(dap, "x") 
  daymet.y <- ncvar_get(dap, "y")
  
  # Getting the increment for the Daymet resolution rather than hard-code it in
  x.inc <- mean(abs(diff(daymet.x)))
  y.inc <- mean(abs(diff(daymet.y)))
  
  nc_close(dap)
  # Find our x & y indices
  if(is.null(ext)){ # if we're working with a series of points, need a loop
    x.ind <- y.ind <- vector(length=length(lon.in))

    for(i in 1:length(x.ind)){
      # if our point is outside of daymet, give its index nothing
      if(x.lcc[i] > max(daymet.x) + x.inc/2 | x.lcc[i] < min(daymet.x) - x.inc/2 |
         y.lcc[i] > max(daymet.y) + y.inc/2 | y.lcc[i] < min(daymet.y) - y.inc/2 ){
        warning(paste0("Site '", site_id[i], "' out of bounds & can not be extracted. \n Continuing extraction for other sites.")) 
        next
      }
      x.ind[i] <- which((daymet.x - x.inc/2) <= x.lcc[i] & (daymet.x + x.inc/2) >= x.lcc[i])
      y.ind[i] <- which((daymet.y - y.inc/2) <= y.lcc[i] & (daymet.y + y.inc/2) >= y.lcc[i])
    }
    
    x.ind[x.ind==0] <- NA
    y.ind[y.ind==0] <- NA
  } else { # we're working with an area, so we're just going to extract the bounding box
    x.ind <- which((daymet.x + x.inc/2) >= bbox(poly.lcc)[1,1] & (daymet.x - x.inc/2) <= bbox(poly.lcc)[1,2])
    y.ind <- which((daymet.y + y.inc/2) >= bbox(poly.lcc)[2,1] & (daymet.y - y.inc/2) <= bbox(poly.lcc)[2,2])
  }

  # Making a progress bar
  pb.index=1
  pb <- txtProgressBar(min=1, max=length(site_id)*rows*nrow(var), style=3)
  
  for (i in seq_len(rows)) {
    year <- ylist[i]
    # ntime <- ifelse(lubridate::leap_year(year), 366, 365)
    ntime=365 # Daymet discards Dec 31 from leap year so that all years have 365 days
    
    var.list <- list()
    dat.list <- list()
    
    ## get data off OpenDAP
    for (j in seq_len(nrow(var))) {
      setTxtProgressBar(pb, pb.index)
      # https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/1980/daymet_v3_prcp_1980_na.nc4
      file.name <- paste0("daymet_v3_", var$DAP.name[j], "_", year, "_na.nc4")
      dap_file <- paste0(dap_base, year, "/", file.name)
      # PEcAn.utils::logger.info(dap_file)
      dap <- nc_open(dap_file)
      # dat.list[[j]] 
      
      # Extraction & netcdf format will depend on whether we're working with points or an area
      # Note: this is a departure from the standard Pecan order of operations to accommodate efficient
      # multi-site extraction
      if(is.null(ext)){ # We're working with points
        
        for(k in 1:length(site_id)){
          if(is.na(x.ind[k])) next # In the case of a missing site, that will just be left empty

          ## Create dimensions
          lat <- ncdim_def(name = "latitude", units = "degree_north", vals = lat.in[k], create_dimvar = TRUE)
          lon <- ncdim_def(name = "longitude", units = "degree_east", vals = lon.in[k], create_dimvar = TRUE)
          time <- ncdim_def(name = "time", units = "days", vals = (1:ntime), create_dimvar = TRUE, unlim = TRUE)
          dim <- list(lat, lon, time)

          # Note the additional index for multiple sites
          dat.list[[paste(site_id[k])]][[j]] <- ncvar_get(dap, as.character(var$DAP.name[j]), start=c(x.ind[k],y.ind[k],1), c(1,1,ntime))
          
          var.list[[paste(site_id[k])]][[j]] <- ncvar_def(name = as.character(var$CF.name[j]), 
                                                   units = as.character(var$units[j]), 
                                                   dim = dim, 
                                                   missval = -999,
                                                   verbose = verbose)
          # Doing unit conversions
          if(var$units[j] %in% c("Kelvin", "K")) dat.list[[paste(site_id[k])]][[j]] <- dat.list[[paste(site_id[k])]][[j]]+273.15
          if(var$units[j] %in% c("mm/s", "kg/m2/s")) dat.list[[paste(site_id[k])]][[j]] <- dat.list[[paste(site_id[k])]][[j]]/(60*60*24)
          
          pb.index=pb.index+1 # advance our status bar   
          }
          
      } else {
        lat <- ncdim_def(name = "lcc y", units = "meters", vals = daymet.y[y.ind], create_dimvar = TRUE)
        lon <- ncdim_def(name = "lcc x", units = "meters", vals = daymet.x[x.ind], create_dimvar = TRUE)
        time <- ncdim_def(name = "time", units = "days", vals = (1:ntime), create_dimvar = TRUE, unlim = TRUE)
        dim <- list(lat, lon, time)
        
        # Note the additional index for multiple sites
        dat.list[[j]] <- ncvar_get(dap, as.character(var$DAP.name[j]), 
                                   start=c(min(x.ind),min(y.ind),1), 
                                   count=c(length(x.ind),length(y.ind),ntime)
                                   )
        var.list[[j]] <- ncvar_def(name = as.character(var$CF.name[j]), 
                                   units = as.character(var$units[j]), 
                                   dim = dim, 
                                   missval = -999,
                                   verbose = verbose
                                   )
        # Doing unit conversions
        if(var$units[j] %in% c("Kelvin", "K")) dat.list[[j]] <- dat.list[[j]]+273.15
        if(var$units[j] %in% c("mm/s", "kg/m2/s")) dat.list[[j]] <- dat.list[[j]]/(60*60*24)
        
        pb.index=pb.index+1 # advance our status bar   
      } # end points vs area
      
      ncdf4::nc_close(dap)
      
    } # end vars
      
    ## put data in new file
    for(k in 1:length(site_id)){
      out.site <- file.path(outfolder, site_id[k])
      if(!dir.exists(out.site)) dir.create(out.site, showWarnings = T, recursive = TRUE)
      
      loc.file <- file.path(out.site, paste("Daymet", year, "nc", sep = "."))
      
      loc <- ncdf4::nc_create(filename = loc.file, vars = var.list[[k]], verbose = verbose)
      for (j in seq_len(length(var.list[[k]]))) {
        ncdf4::ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[k]][[j]])
      }
      ncdf4::nc_close(loc)
    } # End file creation
    
      
      # results$file[i] <- loc.file
      # results$host[i] <- PEcAn.utils::fqdn()
      # results$startdate[i] <- paste0(year, "-01-01 00:00:00")
      # results$enddate[i] <- paste0(year, "-12-31 23:59:59")
      # results$mimetype[i] <- "application/x-netcdf"
      # results$formatname[i] <- "CF Meteorology"
  }
  
  # return(invisible(results))
} # download.Daymet





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