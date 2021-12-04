

# Licenses

* Software license see <LICENSE>.
* For data license see <DATA_LICENSE> included in package.

# Requirements

## System requirements

* `curl` support for downloading the data via https
* Allows to retrieve the data as NetCDF (classic 64bit format). This conversion
    is done locally and requires the ecCodes tools to be installed, namely
    `grib_to_netcdf`. On most Linux systems install `libeccodes-tools` via
    package manager.

## R packages

* `dplyr::bind_rows` is used in one instance.
* `httr` for downloading data (curl based)
* ...



