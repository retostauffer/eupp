# ----------------------------------------------------------------
# Testing eupp_*_gridded()
# ----------------------------------------------------------------

require("tinytest")

# Setting up cache-dir
cache_dir <- tempdir()

# Basic check for functions
expect_true(is.function(eupp_get_inventory), info = "Cannot find function eupp_config")
expect_true(is.function(eupp_download_gridded), info = "Cannot find function eupp_config")

# ----------------------------------------------------------------
# Fetching GRIB index or inventory first; will be cached
# Setting up an eupp_config object first; member = 0 (control run only)
# and fetch inventory (tested extensively in dedicated test files)
# ----------------------------------------------------------------
expect_silent(config <- eupp_config("forecast", "surf", "ens", "2018-01-01",
                                    parameter = "2t", steps = 0:12, members = 0, cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(config))



# ----------------------------------------------------------------
# Downloading data as GRIB version 1
# Note that output_format should be guessed by file name
# ----------------------------------------------------------------

# Testing wrong usage
expect_error(eupp_download_gridded())
expect_error(eupp_download_gridded(config))
expect_error(eupp_download_gridded(config, output_file = 1),
             info = "'output_file' must be character")
expect_error(eupp_download_gridded(config, output_file = c("foo", "bar")),
             info = "'output_file' must be character of length 1")
expect_error(eupp_download_gridded(1, output_file = "_foo.grb"),
             info = "'x' must be eupp_config object")
expect_error(eupp_download_gridded(config, output_file = "_foo.grb", output_format = "foo"),
             info = "'ouptut_format' specified wrongly")
expect_error(eupp_download_gridded(config, output_file = "_foo.grb", output_format = c("grb", "nc")),
             info = "'ouptut_format' specified wrongly")

expect_error(eupp_download_gridded(config, "_foo.grb", verbose = c(TRUE, TRUE)),
             info = "'verbose' must be single logical TRUE or FALSE")
expect_error(eupp_download_gridded(config, "_foo.grb", verbose = 1),
             info = "'verbose' must be single logical TRUE or FALSE")
expect_error(eupp_download_gridded(config, "_foo.grb", verbose = NA),
             info = "'verbose' must be single logical TRUE or FALSE")

expect_error(eupp_download_gridded(config, "_foo.grb", overwrite = c(TRUE, TRUE)),
             info = "'overwrite' must be single logical TRUE or FALSE")
expect_error(eupp_download_gridded(config, "_foo.grb", overwrite = 1),
             info = "'overwrite' must be single logical TRUE or FALSE")
expect_error(eupp_download_gridded(config, "_foo.grb", overwrite = NA),
             info = "'overwrite' must be single logical TRUE or FALSE")

# Output file is actually a directory
expect_silent(tmpdir <- tempdir())
expect_error(eupp_download_gridded(config, tempdir, overwrite = TRUE))
expect_error(eupp_download_gridded(config, "/directory/does/not/exist/test.grb", overwrite = TRUE))

# Proper usage
grib_file <- file.path(tempdir(), "_test.grb")
if (file.exists(grib_file)) unlink(grib_file)
expect_silent(grb <- eupp_download_gridded(config, grib_file))
# -> overwrite = FALSE and we should see an error
expect_error(grb <- eupp_download_gridded(config, grib_file))
expect_true(file.exists(grib_file),
            info = "Output file not existing")

# If it is proper grib file we should be able to grib_ls it.
expect_silent(gout <- system(paste("grib_ls", grib_file), intern = TRUE),
              info = "Problem reading grib file with grib_ls")
expect_inherits(gout, "character")
expect_silent(tmp <- regmatches(gout, regexpr("[0-9]+\\sof\\s[0-9]+\\smessages", gout)),
              info = "Could not find string 'X of X messages' in grib_ls output")
expect_silent(tmp <- as.integer(regmatches(tmp, regexpr("^[0-9]+", tmp))),
              info = "Issues extracting the number of messages from grib_ls output")
expect_identical(nrow(inv), tmp,
                 info = "Found different number of messages via grib_ls compared to inventory")



# ----------------------------------------------------------------
# Downloading as NetCDF
# Note that output_format should be guessed by file name
# ----------------------------------------------------------------
suppressPackageStartupMessages(library("ncdf4"))
nc_file   <- file.path(tempdir(), "_test.nc")
if (file.exists(nc_file)) unlink(nc_file)
expect_silent(nc  <- eupp_download_gridded(config, nc_file, "nc"))
# -> overwrite = FALSE and we should see an error
expect_error(nc  <- eupp_download_gridded(config, nc_file, "nc"))
expect_true(file.exists(nc_file),
            info = "Output file not existing")

# Open netcdf file and check content
expect_silent(nc <- nc_open(nc_file),
                  info = "Problems loading NetCDF file")
expect_inherits(nc, "ncdf4",
                  info = "Object not of lcass 'ncdf4' (wrong return by nc_open())")
expect_identical(names(nc$var), "t2m",
                  info = "NetCDF file contains wrong variable")
expect_silent(nc_time <- as.POSIXct(ncvar_get(nc, "time") * 3600, origin = "1900-01-01", tz = "UTC"),
                  info = "Problems getting time dimension from NetCDF")
expect_equivalent(nc_time, config$date + config$steps * 3600,
                  info = "NetCDF time dimension not matching our config")


# ----------------------------------------------------------------
# Testing eupp_get_gridded which internally calls eupp_download_gridded
# but should return a stars object directly. Thus, no need for
# intensive testing.
# ----------------------------------------------------------------
expect_error(eupp_get_gridded("foo"),
             info = "Main argument 'x' must be of class 'eupp_config'")
expect_error(eupp_get_gridded(config, verbose = c(TRUE, FALSE)),
             info = "Argument for 'verbose' not single TRUE or FALSE")
expect_error(eupp_get_gridded(config, output_format = "nc"),
             info = "Wrong/unexpected input argument")


expect_silent(st <- eupp_get_gridded(config),
              info = "Expected to be silent")
expect_inherits(st, "stars",
                info = "Return of wrong class; should be a stars object")
expect_identical(dim(st), c(x = 93L, y = 125L, time = 13L),
                 info = "Dimension of stars object is wrong")



# ----------------------------------------------------------------
# Cleaning up
# ----------------------------------------------------------------
if (file.exists(grib_file)) unlink(grib_file)
if (file.exists(nc_file)) unlink(nc_file)
