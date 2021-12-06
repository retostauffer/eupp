
# Documentation

Full documentation can be found on <https://retostauffer.github.io/eupp/>.

# Licenses

* Software license see <https://github.com/retostauffer/eupp/blob/main/LICENSE>
* For data license see <https://github.com/retostauffer/eupp/blob/main/DATA_LICENSE>

# Requirements

## System requirements

| Software/libs | Remarks |
|:--------------|:--------|
| `curl`        | Required for downloading data |
| `gdal`        | Geospatial Data Abstraction Library (req. by R package `stars`) |
| ecCodes       | ECMWF ecCodes toolbox; `grib_to_netcdf` is called to convert GRIB to NetCDF if requested |

On most up-to-date Linux systems the ecCodes tools can be installed via package
manager (e.g., `apt get install libeccodes-tools -y`).


## R packages

Shows required packages. Defined as dependencies; should be installed automatically
when using `github_install()` (see below). The package has been developed/tested using
R version `4.1.2` using the following packages/package versions:

| Package name | Tested on | Remarks |
|:-------------|:---------:|:--------|
| `httr`       | 1.4.2     | Downloading data via https |
| `dplyr`      | 1.0.7     | `dplyr::bind_rows` used in one instance |
| `stars`      | 0.5.5     | Handling/reading spatio-temporal data |

Suggested packages ...

| Package name | Tested on | Remarks |
|:-------------|:---------:|:--------|
| `sf`         | 1.0.4     | Plotting, processing data, spatial subsetting |


## Installation

The package is currently only available via `[github](https://github.com/retostauffer/eupp)`.
The simplest way to install:

```
library("remotes")
github_install("retostauffer/eupp")
```

This will install the current version from the `main` branch. Once stable, a stable
release will be provided.


# Available data

* Spatial extent: longitude `6E/17W`, latitude `36N/67N`.
* Spatial resolution: `0.25` times `0.25` degrees (`rectangular_ll` grid)

## Surface forecast variables

* Forecast steps (in hours):
    * Hourly forecasts from `+0h` to `+90h`
    * Three-hourly intervals from `+90h` to `+144h` (6 days ahead)
    * Six-hourly intervals from `+144h` to `+240h` (10 days ahead)

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

* Forecast steps (in hours):
    * Hourly forecasts from `+0h` to `+90h`
    * Three-hourly intervals from `+90h` to `+144h` (6 days ahead)
    * Six-hourly intervals from `+144h` to `+240h` (10 days ahead)

| Parameter name      | Level  | ECMWF key | Remarks |
|:--------------------|:-------|:----------|:--------|
| Temperature         | 850    | `t`       |         |
| U component of wind | 700    | `u`       |         |
| V component of wind | 700    | `v`       |         |
| Geopotential        | 500    | `z`       |         |
| Specific humidity   | 700    | `q`       |         |
| Relative humidity   | 850    | `r`       |         |

## Extreme forecast index variables

* Forecast ranges (in hours): `0-24`, `24-48`, `48-72`, `72-96`, `96-120`,
  `120-144` and `144-168`.

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

