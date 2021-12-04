

# Licenses

* Software license see <https://github.com/retostauffer/eupp/blob/main/LICENSE>
* For data license see <https://github.com/retostauffer/eupp/blob/main/DATA_LICENSE>

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


# FAQ

### Can we get the data as NetCDF

Yes and no; the data is currently only provided as GRIB version 1. This file format
can be converted to NetCDF; the package provides this functionality.

However, the conversion is done locally and requires the [ecCodes
tools](https://confluence.ecmwf.int/display/ECC/ecCodes+Home) and NetCDF libraries
to be installed.  Many modern UX distributions allow to install via the built
in package manager (`libeccodes-tools`, `libeccodes-bin` or similar; `libnetcdf-dev`, ...).

The R package will then call `grib_to_netcdf` on your local machine.

### Can we download spatial subsets?

This is not possible at the moment. The data sets are provided in the GRIB version 1 format
where subsetting (neither on request nor on the local machine) is possible. When downloading
as GRIB/NetCDF the CDO (climate data operation) toolbox may be one path to go.

