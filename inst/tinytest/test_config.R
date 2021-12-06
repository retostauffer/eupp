# ----------------------------------------------------------------
# Testing eupp_config()
# ----------------------------------------------------------------

library('devtools')
library('tinytest')
load_all("../../")

expect_true(is.function(eupp_config),
            info = "Cannot find function eupp_config")

# ----------------------------------------------------------------
# Checking usage
# ----------------------------------------------------------------

# Minimal usage: should run silently
expect_silent(c1 <- eupp_config(type = "forecast", kind = "ctr", level = "surf", date = "2017-01-01"))
expect_silent(c2 <- eupp_config("forecast", "ctr", "surf", "2017-01-01"))
expect_true(identical(c1, c2))


# Missing one required argument: error expected
expect_error(eupp_config(kind = "ctr", level = "surf", date = "2017-01-01"),
             info = "Required 'type' not specified")
expect_error(eupp_config(type = "forecast", level = "surf", date = "2017-01-01"),
             info = "Required 'kind' not specified")
expect_error(eupp_config(type = "forecast", kind = "ctr", date = "2017-01-01"),
             info = "Required 'level' not specified")
expect_error(eupp_config(type = "forecast", kind = "ctr", level = "surf"),
             info = "Required 'date' not specified")

# Wrong input arguments 'type'
expect_error(eupp_config(type = "foo", kind = "ctr", level = "surf", date = "2017-01-01"),
             info = "Type does not exist")
expect_error(eupp_config(type = 3, kind = "ctr", level = "surf", date = "2017-01-01"),
             info = "Type not character")
expect_error(eupp_config(type = c("forecast", "reforecast"), kind = "ctr", level = "surf", date = "2017-01-01"),
             info = "Type not of length 1")

# Wrong input arguments 'kind'
expect_error(eupp_config(type = "forecast", kind = "foo", level = "surf", date = "2017-01-01"),
             info = "Kind does not exist")
expect_error(eupp_config(type = "forecast", kind = 1, level = "surf", date = "2017-01-01"),
             info = "Kind does not character")
expect_error(eupp_config(type = "forecast", kind = c("ctr", "ens"), level = "surf", date = "2017-01-01"),
             info = "Kind not of length 1")


# Wrong input arguments 'kind'
expect_error(eupp_config(type = "forecast", kind = "ctr", level = "foo", date = "2017-01-01"),
             info = "Level does not exist")
expect_error(eupp_config(type = "forecast", kind = "ctr", level = 1, date = "2017-01-01"),
             info = "Level not character")
expect_error(eupp_config(type = "forecast", kind = "ctr", level = c("surf", "pressure"), date = "2017-01-01"),
             info = "Level not of length 1")

# Wrong input arguments 'date'
expect_error(eupp_config(type = "forecast", kind = "ctr", level = "surf", date = 1),
             info = "Date not of type character, Date, or POSIXt")
expect_error(eupp_config(type = "forecast", kind = "ctr", level = "surf", date = "01.01.2017"),
             info = "Date not ISO format.")

# Wrong input for further parameters
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", parameter = 1),
             info = "'parameter' must be NULL or character")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", steps = "3"),
             info = "'steps' must be NULL or character")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", area = "DE"),
             info = "'area' must be NULL or of class bbox")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", cache = 3),
             info = "'cache' must be NULL or character length 1")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", cache = c("foo", "bar")),
             info = "'cache' must be NULL or character length 1")
rnd <- paste(sample(c(LETTERS, letters, 1:9), 30, replace = TRUE), collapse = "")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", cache = rnd),
             info = "'cache' must point to existing directory")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", version = "foo"),
             info = "'version' must be single integer")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", version = 0:1),
             info = "'version' must be single integer")
expect_error(eupp_config("forecast", "ctr", "surf", "2017-0-101", version = -1L),
             info = "'version' must be larger or equal to 0")





# ----------------------------------------------------------------
# Testing allowed parameters and defaults
# ----------------------------------------------------------------
for (n in c("reforecast", "forecast", "analysis"))
    expect_silent(eupp_config(n, "ctr", "surf", "2021-01-01"), info = "Issue with 'type' defaults")
for (n in c("ctr", "ens", "hr"))
    expect_silent(eupp_config("forecast", n, "surf", "2021-01-01"), info = "Issue with 'kind' defaults")
for (n in c("surf", "pressure", "efi"))
    expect_silent(eupp_config("forecast", "ctr", n, "2021-01-01"), info = "Issue with 'level' defaults")

# Testing partial matching
expect_silent(c3 <- eupp_config("ref", "c", "su", "2017-01-01"))
expect_identical(c3$type, "reforecast")
expect_identical(c3$kind, "ctr")
expect_identical(c3$level, "surf")
expect_identical(c3$date, as.POSIXct("2017-01-01 00:00:00", tz = "UTC"))
for (n in c("parameter", "steps", "area", "cache"))
    expect_true(is.null(c3[[n]]), info = sprintf("'%s' should be NULL by default", n))


# ----------------------------------------------------------------
# Testing response when defaults are overwritten and test
# correct order of all function arguments.
# ----------------------------------------------------------------
library("sf")
bbox   <- st_bbox(c(xmin = -3, xmax = 3, ymin = 40, ymax = 50))
tmpdir <- tempdir()
expect_silent(c4 <- eupp_config(type = "forecast", kind = "ctr", level = "surf", date = "2017-01-01",
                                parameter = c("t2m", "cp"), steps = 0:6, area = bbox, cache = tmpdir,
                                version = 666L),
              info = "Testing all args (overwrite defaults); named args")
expect_silent(c5 <- eupp_config("forecast", "ctr", "surf", "2017-01-01",
                                c("t2m", "cp"), 0:6, bbox, tmpdir,
                                666L),
              info = "Testing all args (overwrite defaults); unnamed args")
# Return must be the same ...
expect_identical(c4, c5)
expect_identical(c4$parameter, c("t2m", "cp"))
expect_identical(c4$steps,     0:6)
expect_identical(c4$area,      bbox)
expect_identical(c4$cache,     tmpdir)
expect_identical(c4$version,   666L)

# Checking return
expect_inherits(c4, "eupp_config", info = "Returned object wrong class")
expect_true(is.list(c4))

# Checking elements (and class) of objects in return
def <- c(type = "character",        type_abbr = "character",
         kind = "character",        level = "character",
         date = "POSIXct",          steps = "integer",
         parameter = "character",   version = "integer",
         cache = "character")
for (n in names(def)) {
    expect_true(n %in% names(c4), info = "Named element not found in return")
    expect_inherits(c4[[n]], def[[n]], info = "Returned element of unexpected class")
}


