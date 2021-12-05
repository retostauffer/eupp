

# Licenses

* Software license see <https://github.com/retostauffer/eupp/blob/main/LICENSE>
* For data license see <https://github.com/retostauffer/eupp/blob/main/DATA_LICENSE>

# Requirements

## System requirements

* `curl` support for downloading the data via https
* `gdal` tools/library required by the _R_ package 'stars'
* Allows to retrieve the data as NetCDF (classic 64bit format). This conversion
    is done locally and requires the ecCodes tools to be installed, namely
    `grib_to_netcdf`. On most Linux systems install `libeccodes-tools` via
    package manager.

## R packages

* `dplyr::bind_rows` is used in one instance.
* `stars` for processing spatial data
* `httr` for downloading data (curl based)
* ...

* **Suggested**: `sf` ...

@TODO

# Available data

* Spatial extent: longitude `6E/17W`, latitude `36N/67N`.
* Forecast steps (in hours):
    * Hourly forecasts from `+0h` to `+90h`
    * Three-hourly intervals from `+90h` to `+144h` (6 days ahead)
    * Six-hourly intervals from `+144h` to `+240h` (10 days ahead)

## Surface forecast variables

| Parameter name                        | ECMWF key | Remarks |
|:--------------------------------------|:----------|:--------|
| 2 metre temperature                   | `2t`      |         |
| 10 metre U wind component             | `10u`     |         |
| 10 metre V wind component             | `10v`     |         |
| Total cloud cover                     | `tcc`     |         |
| 100 metre U wind component anomaly    | `100ua`   |         |
| 100 metre V wind component anomaly    | `100va`   |         |
| Convective available potential energy | `cape`    |         |
| Soil temperature level 1              | `stl1`    |         |
| Surface sensible heat flux            | `sshf`    |         |
| Surface latent heat flux              | `slhf`    |         |
| Total column water                    | `tcw`     |         |
| Total column water vapour             | `tcwv`    |         |
| Volumetric soil water layer 1         | `swvl1`   |         |
| Surface net solar radiation           | `ssr`     |         |
| Surface net thermal radiation         | `str`     |         |
| Snow depth                            | `sd`      |         |
| Convective precipitation              | `cp`      |         |
| Convective inhibition                 | `cin`     |         |
| Surface solar radiation downwards     | `ssrd`    |         |
| Surface thermal radiation downwards   | `strd`    |         |
| Visibility                            | `vis      | Observations not available |


## Pressure level forecast variables

| Parameter name      | Level  | ECMWF key | Remarks |
|:--------------------|:-------|:----------|:--------|
| Temperature         | 850    | `t`       |         |
| U component of wind | 700    | `u`       |         |
| V component of wind | 700    | `v`       |         |
| Geopotential        | 500    | `z`       |         |
| Specific humidity   | 700    | `q`       |         |
| Relative humidity   | 850    | `r`       |         |

## Extreme forecast index variables

| Parameter name                | ECMWF key | Remarks |
|:------------------------------|:----------|:--------|
| 2 metre temperature efi       | `2ti`     |         |
| 10 metre wind speed efi       | `10ws`    |         |
| 10 metre wind gust efi        | `10fgi`   |         |
| cape efi                      | `capei`   |         |
| cape shear efi                | `capesi`  |         |
| Maximum temperature at 2m efi | `mx2ti`   |         |
| Minimum temperature at 2m efi | `mn2ti`   |         |
| Snowfall efi                  | `sfi`     |         |
| Total precipitation efi       | `tpi`     |         |

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

