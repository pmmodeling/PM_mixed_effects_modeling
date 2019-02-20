## Install packages
## install.packages("gdalUtils")
## install.packages("raster")
## install.packages("rgdal")
## install.packages("rgeos")
## install.packages("plyr")
## install.packages("magrittr")
## install.packages("sp")
## install.packages("ncdf4")
## install.packages("reshape2")
## install.packages("foreach")
## install.packages("parallel")
## install.packages("doParallel")
## install.packages("mapview")
## install.packages("velox")
## install.packages("sf")

###########################################################
## Part a.1- working with hdf files on Windows ############
###########################################################

library(gdalUtils)
library(raster)
library(rgdal) 
library(rgeos)
library(plyr)
library(magrittr)
library(sp)

# If you do not have it installed, you will probably get an error message and won't be able to read hdf files
gdal_setInstallation()
x <- getOption("gdalUtils_gdalPath")

x[[1]]$path ## Look where GDAL is installed
df <- x[[1]]$drivers # Table of all installed drivers
df[118:119,] # the hdf drivers

# Set your working directory
setwd("G:/My Drive/Basel_workshop/files_for_code/")

# Get a list of sds names
sds <- get_subdatasets("MAIACAAOT.h03v03.20160120950.hdf")  # Returns subdataset names 

# R wrapper for gdal_translate - converts from hdf to tiff file
gdal_translate(sds[1], "test.tif")

# Load the Geotiff created into R
r = raster("test.tif")
plot(r)

# This function is useful if you are running the code on Windows

read_hdf = function(file, n) {
  sds = get_subdatasets(file)
  f = tempfile(fileext = ".tif") # the tiff file is saved as a temporary file
  gdal_translate(sds[n], f)
  raster(f) # the ouptut is a raster file
}

# Load data
pol=readOGR(dsn="G:/My Drive/Basel_workshop/files_for_code","Project_border_latlon",verbose = FALSE)

# Define input Directories
aod_dir = "G:/My Drive/Basel_workshop/files_for_code"
ref_grid = "G:/My Drive/Basel_workshop/files_for_code/MAIACLatlon.h03v03.hdf"

###################################################################
# STEP 1 - Read the grid locations hdf files

sds = get_subdatasets(ref_grid) # Get the sub-datasets of the refrence grid file

lon = read_hdf(ref_grid, which(grepl("latlon:lon", sds)))
lat = read_hdf(ref_grid, which(grepl("latlon:lat", sds)))

# Create 'row' and 'col' rasters
row = lon
row[] = rowFromCell(lon, 1:ncell(lon)) # Get the row and/or column number from a cell number of a Raster object
col = lon
col[] = colFromCell(lon, 1:ncell(lon))

plot(lon,main="lon")
plot(lat,main="lat")

# Combine to multi-band raster
grid = stack(row, col, lon, lat)
names(grid) = c("row", "col", "lon", "lat")

# Convert to data.frame
grid = as.data.frame(grid)

# Spatial subset according to our project area (Israel)
coordinates(grid) = ~ lon + lat
proj4string(grid) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid = grid[pol, ]
grid = as.data.frame(grid)

#####################

for(year in 2014:2015) {
  # Read HDF files list from AOD directory
  setwd(file.path(aod_dir, year))

  files = list.files(pattern = "MAIACAAOT.h03v03.*\\.hdf$", recursive = TRUE) # note that MAIACTAOT is for TERRA data and MAIACAAOT is for AQUA data

  result = list()

  for(f in files) {

    # Read data
    sds = get_subdatasets(f)
    
    # Choose which subdatasets you want to retrieve from the hdf file
    Optical_Depth_047 = read_hdf(f, grep("grid1km:Optical_Depth_047", sds))
    Optical_Depth_055 = read_hdf(f, grep("grid1km:Optical_Depth_055", sds))
    AOT_Uncertainty = read_hdf(f, grep("grid1km:AOT_Uncertainty", sds))
    AOT_QA = read_hdf(f, grep("grid1km:AOT_QA", sds))
    RelAZ=read_hdf(f, grep("grid5km:RelAZ", sds))
    RelAZ=disaggregate(RelAZ, fact = 5)
    
    # Create rasters of rows and columns 
    row = Optical_Depth_047
    row[] = rowFromCell(Optical_Depth_047, 1:ncell(Optical_Depth_047))
    col = Optical_Depth_047
    col[] = colFromCell(Optical_Depth_047, 1:ncell(Optical_Depth_047))
    
    # Stack all the raster together
    r = stack(row, col, Optical_Depth_047, Optical_Depth_055, AOT_Uncertainty, AOT_QA, RelAZ)
    names(r) = c("row", "col", "Optical_Depth_047","Optical_Depth_055", "AOT_Uncertainty", "AOT_QA","RelAZ")
    r = as.data.frame(r)

    # Join with 'grid' to add the location on each grid cell
    r = plyr::join(r, grid, c("row", "col"))
    r = r[!is.na(r$lon) & !is.na(r$lat), ]

    # Add filename
    r$date =
      f %>%
      strsplit("\\.") %>%
      sapply("[", 3) %>%
      substr(1, 7) %>%
      as.Date(format = "%Y%j")

    # Combine results
    result[[f]] = r

  }

  result = do.call(plyr::rbind.fill, result)
  setwd(file.path(aod_dir))
  saveRDS(result, sprintf("MAIACTAOT_Isr_%s.rds", year))
}

###########################################################
## Part a.2- working with hdf files on Linux ##############
###########################################################

## library(gdalUtils)
## # install GDAL software in your computer
## # Get a list of sds names
## sds <- get_subdatasets('full/path/filename.hdf')
## # Any sds can then be read directly using the raster & readGDAL function
## r <- raster(readGDAL(sds[1]))

## library(plyr)
## library(magrittr)
## 
## # final = list()
## 
## for(year in 2014:2015) {
##   # Read HDF files list from AOD directory
##   setwd(file.path(aod_dir, year))
## 
##   files = list.files(pattern = "MAIACAAOT.h03v03.*\\.hdf$", recursive = TRUE) # note that MAIACTAOT is for TERRA data and MAIACAAOT is for AQUA data
## 
##   result = list()
## 
##   for(f in files) {
## 
##     # Read data
##     sds = get_subdatasets(f)
##     # Choose which subdatasets you want to retrieve from the hdf file
##     Optical_Depth_047 = sds[grepl("grid1km:Optical_Depth_047", sds)] %>% readGDAL %>% raster
##     Optical_Depth_055 = sds[grepl("grid1km:Optical_Depth_055", sds)] %>% readGDAL %>% raster
##     AOT_Uncertainty = sds[grepl("grid1km:AOT_Uncertainty", sds)] %>% readGDAL %>% raster
##     AOT_QA = sds[grepl("grid1km:AOT_QA", sds)] %>% readGDAL %>% raster
## 
##     row = Optical_Depth_047
##     row[] = rowFromCell(Optical_Depth_047, 1:ncell(Optical_Depth_047))
##     col = Optical_Depth_047
##     col[] = colFromCell(Optical_Depth_047, 1:ncell(Optical_Depth_047))
##     r = stack(row, col, Optical_Depth_047, Optical_Depth_055, AOT_Uncertainty, AOT_QA, RelAZ)
##     names(r) = c("row", "col", "Optical_Depth_047","Optical_Depth_055", "AOT_Uncertainty", "AOT_QA","RelAZ")
##     r = as.data.frame(r)
## 
##     # Join with 'grid' to add the location on each grid cell
##     r = join(r, grid, c("row", "col"))
##     r = r[!is.na(r$lon) & !is.na(r$lat), ]
## 
##     # Add filename
##     r$date =
##       f %>%
##       strsplit("\\.") %>%
##       sapply("[", 3) %>%
##       substr(1, 7) %>%
##       as.Date(format = "%Y%j")
## 
##     # Combine results
##     result[[f]] = r
## 
##   }
## 
##   result = do.call(rbind.fill, result)
##   setwd(file.path(aod_dir))
##   saveRDS(result, sprintf("MAIACTAOT_Isr_%s.rds", year))
## }

###########################################################
### Working with netCDF files in R ########################
###########################################################

# (1) First we will install and load all the needed packages 
# Load the required packages\libraries
library(ncdf4)
library(magrittr)
library(raster)
library(reshape2)

# Define your working directory and load the ncdf file 
setwd("G:/My Drive/Basel_workshop/files_for_code")
filename = "_grib2netcdf-atls15-95e2cf679cd58ee9b4db4dd119a05a8d-6JOEWD.nc"

# Open the NetCDF data set, and print some basic information.
nc = nc_open(filename)
class(nc)
# print(nc)

# Get the the variable (pbl) and its attributes, and verify the size of the array.
pbl_array <- ncvar_get(nc,"blh")
dim(pbl_array)

pbl_slice <- pbl_array[,,1]
dim(pbl_slice)

dlname <- ncatt_get(nc,"blh","long_name")
dlname
dunits <- ncatt_get(nc,"blh","units")
dunits

# In a netCDF file, values of a variable that are either missing or simply not available 
# (i.e. ocean grid points in a terrestrial data set) are flagged using specific "fill values" 
# (_FillValue) or missing values (missing_value), the values of which are set as attributes of a variable. 
# In R, such unavailable data are indicated using the "NA" value. 

fillvalue <- ncatt_get(nc,"blh","_FillValue")
fillvalue

# get longitude and latitude
lon <- ncvar_get(nc,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc,"latitude")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# Read time data
# Get the time variable and its attributes using the ncvar_get() and ncatt_get() functions, and list them, and also get the number of time steps using the dim() function.
time = ncvar_get(nc, "time")
nt <- dim(time)
nt

# Understand the time units
# time is usually stored as the CF (Climate Forecast) "time since" format that is not usually human-readable. 
# Print the time units string. Note the structure of the time units attribute. The object tunits has two components  hasatt (a logical variable), and tunits$value, the actual "time since" string.
tunits <- ncatt_get(nc,"time","units")
tunits

# Convert the time array into POSIXct class
origin = as.POSIXct("1900-01-01 00:00:0.0", tz = "GMT")
diffs = as.difftime(time, format = "%H", units = "hours")
time = origin + diffs
head(time)

# Read values data & Create a rasterBrick object
r = brick(filename, varname = "blh") 
dim(r)

# Just for the example we will run the following lines on a small portion of the data
r <- r[[1:2]]
dim(r)

# To 'data.frame'
dat = as.data.frame(r, xy = TRUE) # xy=TRUE means that the spatial coordinates data will be kept
# dat[1:10,1:4]
names(dat) = c("lon", "lat", as.character(time[1:2], usetz = TRUE))
# dat[1:10,1:4]

# Reshape the data frame into a more suitable structure
dat = melt(
  dat, 
  id.vars = c("lon", "lat"), 
  variable.name = "time", 
  value.name = "hpbl"
)

head(dat)

# Round & Remove rows with no 'hpbl' value
dat$hpbl = round(dat$hpbl)
dat = dat[!is.na(dat$hpbl), ] 

# Filter time frame
dat$time = as.POSIXct(dat$time, tz = "GMT")
dat = dat[dat$time >= as.POSIXct("2003-01-01 00:00:00 GMT"), ]

# Write CSV
write.csv(dat, "ecmwf_hpbl_israel_2003_2016_new.csv", row.names = FALSE)

############################################################################
###################### Part b.1- adding NDVI data ##########################
############################################################################

# Load libraries
library(magrittr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(parallel)
library(plyr)
library(sf)
library(gdalUtils)

setwd("G:/My Drive/Basel_workshop/files_for_code/MODIS_data")

# Prepare files list
files = list.files(
  pattern = "\\.hdf$", 
  recursive = TRUE, 
  full.names = TRUE
)
files

# Prepare years vector
files_split = strsplit(files, "\\.") # Split by '.'
dates = sapply(files_split, "[", 5) # Select date component from file name
dates = as.Date(dates, "A%Y%j")
years = format(dates, "%Y") %>% as.numeric # Characters 2-5 = Year
years

# This function is useful if you are running the code on Windows
read_hdf = function(file, n) {
  sds = get_subdatasets(file)
  f = tempfile(fileext = ".tif")
  gdal_translate(sds[n], f)
  raster(f)
}

###############################################################################################
# STEP 1 - covert from hdf files to raster files
###############################################################################################
## Note that in January months we get only 4 tiles (instead of five) in this area 
## This should be considered and the missing tile should be completed, by finding the extent of the 5 tiles and complete the missing tile using the extend function 

# The next stage takes time, therefore we will run only one year and one tile 
y <- 2000
d <- as.character(unique(dates[years == y]))
current_files <-  files[dates == d[1]]

k <- current_files[1]
sds = get_subdatasets(k) 

r = list()
r[[k]] = read_hdf(k, grep("1 km monthly NDVI", sds))

## # You can try later the following code that loops over multipile years
## # Creates a Mosaic out of the different tiles for each month
## # And saves the yearly raster as multi-band raster
 
for(y in 2000:2001) { # Loop 1: for each year

  # Empty raster stack
  result = stack()

  for(d in as.character(unique(dates[years == y]))) { # Loop 2: each day

    # Find the list of tiles for this date
    current_files = files[dates == d] # 'files' subset for given day

    # Empty list for 4 rasters
    r = list()

    # Reading tiles
    for(k in current_files) { # Loop 4: each tile

      sds = get_subdatasets(k) # Read current file
      r[[k]] = read_hdf(k, grep("1 km monthly NDVI", sds))
      # r[[k]] = sds[grepl("NDVI", sds)] %>% readGDAL %>% raster # Convert to raster- use this line instead if you work on Linux

    }

    # plot(r[[1]])

    # Save 1st tile to temporary object
    tmp = r[[1]]

    # If there is more than 1 tile...
    if(length(current_files) > 1) {

      # Loop through remaining 2, 3..., n tiles and mosaic
      for(j in 2:length(current_files)) {

        tmp = mosaic(tmp, r[[j]], fun = "mean") # Mosaic

      }

    }

    r = tmp
    names(r) = d # Set layer name
    plot(r)
    result = stack(result, r)     # Adding another month as raster layer
  }

  # Save whole year raster
  saveRDS(result, paste0("G:/My Drive/Basel_workshop/files_for_code/MODIS_data/MOD13A3_", y, ".rds"))

}

###############################################################################################
# STEP 2 - extract NDVI values from raster to the ECHO point grid
###############################################################################################
library(dplyr)
library(reshape2)

# Grid
setwd("G:/My Drive/Basel_workshop/files_for_code/grid_points_echo/")
grid = readOGR(".", "grid_boston", stringsAsFactors = FALSE, verbose = FALSE)

for(y in 2000:2001) {

  # Read raster
  setwd("G:/My Drive/Basel_workshop/files_for_code/MODIS_data")
  r = readRDS(paste0("MOD13A3_", y, ".rds"))
  r = r * 0.0001 * 0.0001 # scale factor

  # Extract to grid
  tmp = extract(r, grid)
  tmp = as.data.frame(tmp)
  tmp$aodid = grid$aodid
  tmp = melt(tmp, id.vars = "aodid", variable.name = "date", value.name = "MO13A3_NDVI")
  tmp$date = tmp$date %>% as.character %>% as.Date(format = "X%Y.%m.%d")

  # Results table
  dat = expand.grid(
    aodid = grid$aodid,
    date = seq(
      as.Date(paste0(y, "-03-01")),
      as.Date(paste0(y, "-04-30")),
      by = 1
    ),
    stringsAsFactors = FALSE
  )

  # Join
  dat$month = format(dat$date, "%m")
  tmp$month = format(tmp$date, "%m")
  dat = dplyr::left_join(dat, tmp[, c("aodid", "month", "MO13A3_NDVI")], c("aodid", "month"))
  dat$month = NULL

  # Write
  setwd("G:/My Drive/Basel_workshop/files_for_code/MODIS_data/")
  saveRDS(dat, paste0("./results/grid_MO13A3_NDVI_", y, ".rds"))

}


############################################################################
############# Part b.2- Adding Major road density (length / km^2) ##########
############################################################################

library(magrittr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(parallel)
library(plyr)
library(sf)

# Load roads data
setwd("G:/My Drive/Basel_workshop/files_for_code")
roads_major = st_read("./roads_data", "NEMIA_roadsmajor_Boston_area")
# Transform the coordinate system of the roads layer
roads_major = st_transform(roads_major, "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

# Load polygon grid of boston area
grid_1km = st_read("./grid_square_1km", "grid_square_1km_boston", stringsAsFactors = FALSE)
grid_1km_us = st_transform(grid_1km, "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")

########################################################

# Set parallel 
library(foreach)
library(doParallel) 
cl = makeCluster(detectCores(4))
registerDoParallel(cl)

# Calculate Road density & distance to nearest road
# Note that the output of this process is a list (the result of each iteration is stored as a list component)

roads_lengths = foreach(
    i = 1:nrow(grid_1km_us),
    .packages = c("sf", "magrittr")
  ) %dopar% {
  majorroads_length_km_km2 = st_intersection(roads_major, grid_1km_us[i, ])
  majorroads_length_km_km2 = if(nrow(majorroads_length_km_km2) > 0) {
    majorroads_length_km_km2 %>% 
      st_length %>%  # Length of intersecting roads
      as.numeric %>% 
      divide_by(1000)
    } else 0 # No intersection = 0
  
  # Calculate distance to major roads
  nearest_majorroad_dist_km = 
    st_distance(roads_major, grid_1km_us[i, ]) %>% # Compute Euclidian or great circle distance between pairs of geometries
    as.numeric %>% 
    divide_by(1000)
  
  c(
    majorroads_length_km_km2, 
    nearest_majorroad_dist_km
    )
  
}

stopCluster(cl)

# Add the columns back to the square grid 
grid_1km$majorroads_length_km_km2 = sapply(roads_lengths, "[", 1)
grid_1km$nearest_road_dist_km = sapply(roads_lengths, "[", 2)

dat = grid_1km
st_geometry(dat) = NULL # sf object to data frame
class(dat)

# Save result
saveRDS(dat, "grid_1km_roads.rds")

############################################################################
############# Part b.3- Adding Population data #############################
############################################################################

# Load libraries
library(magrittr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(plyr)
library(velox)

setwd("G:/My Drive/Basel_workshop/files_for_code") # Set working directory
# Load AOI and grid data
aoi = readOGR(dsn=".", "boston_area", verbose = FALSE)
grid_1km = readOGR("./grid_square_1km","grid_square_1km_boston", verbose = FALSE) # square grid

# Add Population data
r = raster("./pop_dens/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals_2010.tif")

# Reproject 'grid' and 'AOI' to CRS of raster
grid_1km_proj = spTransform(grid_1km, proj4string(r))
aoi = spTransform(aoi, proj4string(r))

# Crop raster
r = crop(r, aoi, progress = "text")
plot(r)

# Extract
# Velox is an R package for performing fast extraction and manipulation operations on geographic raster data.  
# velox is intended to be used together with the raster package, to which it provides a straightforward interface.
vx = velox(r) # 'RasterLayer' to 'velox' object
f = function(x) mean(x, na.rm = TRUE)
x = vx$extract(sp = grid_1km_proj, fun = f) # 'Fast' extraction method
grid_1km$pop_dens_2015 = x[, 1] # Add new column in 'grid_1km' layer

# Save the result and clean memory
grid_1km_t = as.data.frame(grid_1km)
saveRDS(grid_1km_t, "G:/My Drive/Basel_workshop/files_for_code/pop_dens/grid_1km_pop_dens.rds")

############################################################################
############# Part b.4- Adding Land use data ###############################
############################################################################

# Load libraries
library(magrittr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)
library(plyr)
library(velox)

setwd("G:/My Drive/Basel_workshop/files_for_code") # Set working directory

# Load AOI and grid data
aoi = readOGR(dsn=".", "boston_area", verbose = FALSE)
grid_1km = readOGR("./grid_square_1km","grid_square_1km_boston", verbose = FALSE) # square grid

# Read Land Cover raster
lu = raster("./land_use/nlcd_2011_boston_area.img")

# Reproject 'grid' and 'AOI' to CRS of raster
grid_1km_proj = spTransform(grid_1km, proj4string(lu))
aoi = spTransform(aoi, proj4string(lu))

# Crop raster
lu = crop(lu, aoi, progress = "text")

# Extract land cover to 'grid'
r = lu %in% c(41, 42, 43) # Reclassify
plot(r)
vx = velox(r) # 'RasterLayer' to 'velox' object
f = function(x) mean(x, na.rm = TRUE)
x = vx$extract(sp = grid_1km_proj, fun = f) # 'Fast' extraction method
grid_1km$forestProp_1km = x[, 1] # Add new column in 'grid_1km' layer

# Save the result and clean memory
grid_1km_t=as.data.frame(grid_1km)
saveRDS(grid_1km_t, "G:/My Drive/Basel_workshop/files_for_code/land_use/grid_1km_forestProp_1km.rds")

