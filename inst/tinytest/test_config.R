# ----------------------------------------------------------------
# Testing eupp_config()
# ----------------------------------------------------------------

require("tinytest")

expect_true(is.function(eupp_config), info = "Cannot find function eupp_config")

# ----------------------------------------------------------------
# Checking usage
# ----------------------------------------------------------------

# Minimal usage: should run silently
expect_silent(c1 <- eupp_config(product = "forecast", level = "surf", type = "ens", date = "2017-01-01"))
expect_silent(c2 <- eupp_config("forecast", "surf", "ens", "2017-01-01"))
expect_true(identical(c1, c2))


# Missing one required argument: error expected
expect_error(eupp_config(type = "ctr", level = "surf", date = "2017-01-01"),
             info = "Required 'product' not specified")
expect_error(eupp_config(product = "forecast", level = "surf", date = "2017-01-01"),
             info = "Required 'type' not specified")
expect_error(eupp_config(product = "forecast", type = "ctr", date = "2017-01-01"),
             info = "Required 'level' not specified")
expect_error(eupp_config(product = "forecast", level = "surf", type = "ctr"),
             info = "Required 'date' not specified")

# Wrong input arguments 'product'
expect_error(eupp_config(product = "foo", level = "surf", type = "ens", date = "2017-01-01"),
             info = "Product invalid; does not exist")
expect_error(eupp_config(product = 3, level = "surf", type = "ens", date = "2017-01-01"),
             info = "Product not character")
expect_error(eupp_config(product = c("forecast", "reforecast"), level = "surf", type = "ens", date = "2017-01-01"),
             info = "Product not of length 1")

# Wrong input arguments 'level'
expect_error(eupp_config(type = "forecast", level = "foo", type = "ens", date = "2017-01-01"),
             info = "Level does not exist")
expect_error(eupp_config(type = "forecast", level = 1, type = "ens", date = "2017-01-01"),
             info = "Level not character")
expect_error(eupp_config(type = "forecast", level = c("surf", "pressure"), type = "ens", date = "2017-01-01"),
             info = "Level not of length 1")

# Wrong input arguments 'type'
expect_error(eupp_config(product = "forecast", level = "surf", type = "foo", date = "2017-01-01"),
             info = "Type wrong; does not exist")
expect_error(eupp_config(product = "forecast", level = "surf", type = 1, date = "2017-01-01"),
             info = "Type does not character")
expect_error(eupp_config(product = "forecast", level = "surf", type = c("ctr", "ens"), date = "2017-01-01"),
             info = "Type not of length 1")

# Wrong input arguments 'date'
expect_error(eupp_config(product = "forecast", level = "surf", type = "ens", date = 1),
             info = "Date not of type character, Date, or POSIXt")
expect_error(eupp_config(product = "forecast", level = "surf", type = "ens", date = "01.01.2017"),
             info = "Date not ISO format.")

# Wrong input for further parameters
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", parameter = 1),
             info = "'parameter' must be NULL or character")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", steps = "3"),
             info = "'steps' must be NULL or character")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", area = "DE"),
             info = "'area' must be NULL or of class bbox")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", cache = 3),
             info = "'cache' must be NULL or character length 1")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", cache = c("foo", "bar")),
             info = "'cache' must be NULL or character length 1")
rnd <- paste(sample(c(LETTERS, letters, 1:9), 30, replace = TRUE), collapse = "")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", cache = rnd),
             info = "'cache' must point to existing directory")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", version = "foo"),
             info = "'version' must be single integer")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", version = 0:1),
             info = "'version' must be single integer")
expect_error(eupp_config("forecast", "surf", "ens", "2017-0-101", version = -1L),
             info = "'version' must be larger or equal to 0")



# ----------------------------------------------------------------
# Testing allowed parameters and defaults
# ----------------------------------------------------------------
expect_silent(c1   <- eupp_config("analysis", "surface", date = "2017-01-01", steps = 12))
expect_silent(c2   <- eupp_config("analysis", "pressure", date = "2017-01-01", steps = 12))

expect_silent(c11  <- eupp_config("forecast", "efi", date = "2017-01-01", steps = 24))
expect_silent(c12  <- eupp_config("forecast", "surface", "ens", date = "2017-01-01", steps = 12))
expect_silent(c13  <- eupp_config("forecast", "surface", "hr", date = "2017-01-01", steps = 12))
expect_silent(c14  <- eupp_config("forecast", "pressure", "ens", date = "2017-01-01", steps = 12))
expect_silent(c15  <- eupp_config("forecast", "pressure", "hr", date = "2017-01-01", steps = 12))

expect_silent(c21  <- eupp_config("reforecast", "surface", "ens", date = "2017-01-02", steps = 12))
expect_silent(c21a <- eupp_config("reforecast", "surface", "ens", date = "2017-01-02", steps = 12, member = 0))
expect_silent(c21b <- eupp_config("reforecast", "surface", "ens", date = "2017-01-02", steps = 12, member = 1))
expect_silent(c21c <- eupp_config("reforecast", "surface", "ens", date = "2017-01-02", steps = 12, member = 0:3))
expect_silent(c22  <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-02", steps = 12))
expect_silent(c22a <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-02", steps = 12, member = 0))
expect_silent(c22b <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-02", steps = 12, member = 1))
expect_silent(c22c <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-02",
                                  steps = 12:13, member = 0:3, parameter = c("t", "z")))

# Testing return c1
expect_inherits(c1, "eupp_config")
expect_true(is.list(c1))
expect_true(c1$product == "analysis" & c1$product_abbr == "ana" & c1$level == "surface")
expect_true(is.null(c1$type) & c1$date == as.POSIXct("2017-01-01 00:00:00", tz = "UTC"))
expect_true(c1$steps == 12 & is.null(c1$members) & is.null(c1$area) & is.null(c1$parameter))
expect_true(c1$version == 0 & is.null(c1$cache))

# Testing return c22c
expect_inherits(c22c, "eupp_config")
expect_true(is.list(c22c))
expect_true(c22c$product == "reforecast" & c22c$product_abbr == "rfcs" & c22c$level == "pressure")
expect_true(c22c$type == "ens" & c22c$date == as.POSIXct("2017-01-02 00:00:00", tz = "UTC"))
expect_true(identical(c22c$steps, 12:13) & identical(c22c$members, 0:3) & is.null(c22c$area))
expect_true(c22c$version == 0 & is.null(c22c$cache) & identical(c22c$parameter, c("t", "z")))

# ----------------------------------------------------------------
# Checking return of eupp_get_source_urls
# ----------------------------------------------------------------
# Should return one single url (there is only one single file)
for (obj in c("c1", "c2", "c11", "c13", "c15")) {
    expect_silent(urls <- eupp_get_source_urls(eval(parse(text = obj))))
    expect_inherits(urls, "character", info = "Return must be character")
    expect_identical(length(urls), 1L, info = "Number of expected URLs is one")
}
# Ensemble and members not defined -> we need to load data of the
# control run index file _and_ the ensemble files; thus two URLs.
# Same is true if members are specified and include 0 (ctr) as well as one > 0 (ens)
for (obj in c("c12", "c14", "c21", "c21c", "c22", "c22c")) {
    expect_silent(urls <- eupp_get_source_urls(eval(parse(text = obj))))
    expect_inherits(urls, "character", info = "Return must be character")
    expect_identical(length(urls), 2L, info = "Number of expected URLs is two (ctr + ens)")
}

# However, when the ensemble is used in combination with specifying
# the members and the user is interested in either only member 0 (ctr)
# or only members > 0 (ens) only one URL shall be returned.
for (obj in c("c21a", "c21b", "c22a", "c22b")) {
    expect_silent(urls <- eupp_get_source_urls(eval(parse(text = obj))))
    expect_inherits(urls, "character", info = "Return must be character")
    expect_identical(length(urls), 1L, info = "Number of expected URLs is one (ctr or ens)")
}

# ----------------------------------------------------------------
# Testing response when defaults are overwritten and test
# correct order of all function arguments.
# ----------------------------------------------------------------
library("sf")
bbox   <- st_bbox(c(xmin = -3, xmax = 3, ymin = 40, ymax = 50))
tmpdir <- tempdir()
expect_silent(c50 <- eupp_config(product = "forecast", level = "surf", type = "ens", date = "2017-10-01",
                                 parameter = c("t2m", "cp"), steps = 0:6,
                                 members = 1:2, area = bbox, cache = tmpdir,
                                 version = 666L),
              info = "Testing all args (overwrite defaults); named args")
expect_silent(c51 <- eupp_config("forecast", "surf", "ens", "2017-10-01",
                                 c("t2m", "cp"), 0:6, 1:2, bbox, tmpdir,
                                 666L),
              info = "Testing all args (overwrite defaults); unnamed args")

# Return must be the same ...
expect_identical(c50, c51)
expect_identical(c50$parameter, c("t2m", "cp"))
expect_identical(c50$steps,     0:6)
expect_identical(c50$area,      bbox)
expect_identical(c50$cache,     tmpdir)
expect_identical(c50$version,   666L)

# Checking return
expect_inherits(c50, "eupp_config", info = "Returned object wrong class")
expect_true(is.list(c50))

# Checking elements (and class) of objects in return
def <- c(product   = "character",     product_abbr = "character",
         level     = "character",     type         = "character",
         date      = "POSIXct",       steps        = "integer",
         members   = "integer",       area         = "bbox",
         parameter = "character",     version      = "integer",
         cache     = "character")
for (n in names(def)) {
    expect_true(n %in% names(c50), info = "Named element not found in return")
    expect_inherits(c50[[n]], def[[n]], info = "Returned element of unexpected class")
}


