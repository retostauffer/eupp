
# Setting up cache-dir
cache_dir <- tempdir()

# -------------------------------------------------------------------
# Testing inventory for an analysis case
# -------------------------------------------------------------------
expect_silent(conf <- eupp_config("analysis", "surface", date = "2017-01-01", steps = 12, cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(conf))
expect_identical(class(inv), c("eupp_inventory", "data.frame"),
                 info = "Return must be of class c(\"eupp_inventory\", \"data.frame\")")
expect_true(all(as.integer(format(inv$valid, "%H")) == 12),
                 info = "All fields must be valid at 12 o'clock (step was set to 12)")
expect_true(all(inv$levtype == "sfc"),
                 info = "Only surface variables")
rm(list = c("conf", "inv"))


# -------------------------------------------------------------------
# Testing a specific case for 'efi' (forecast)
# -------------------------------------------------------------------
expect_silent(conf <- eupp_config("forecast", "efi", date = "2017-10-01", parameter = "mx2ti", steps = 96, cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(conf))
expect_identical(class(inv), c("eupp_inventory", "data.frame"), info = "Return must be of class c(\"eupp_inventory\", \"data.frame\")")
expect_identical(nrow(inv),   1L, info = "Inventory must contain one row (one parameter, one step only)")
expect_identical(inv$init,    as.POSIXct("2017-10-01", tz = "UTC"), info = "Testing 'init' date")
expect_identical(inv$valid,   as.POSIXct("2017-10-01", tz = "UTC") + 96 * 3600, info = "Testing 'valid' date; init date + forecast step")
expect_identical(inv$type,    "efi", info = "Type must be 'efi'")
expect_identical(inv$levtype, "sfc", info = "Only surface variables")
rm(list = c("conf", "inv"))


# -------------------------------------------------------------------
# Testing a specific ensemble forecast request with only 'member = 0' (ctr run)
# -------------------------------------------------------------------
expect_silent(conf <- eupp_config("forecast", "surface", "ens", date = "2017-10-01",
                                  parameter = "2t", steps = c(24, 48), members = 0L, cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(conf))
expect_identical(class(inv), c("eupp_inventory", "data.frame"), info = "Return must be of class c(\"eupp_inventory\", \"data.frame\")")
expect_identical(nrow(inv),   2L, info = "Inventory must contain two rows (one parameter, two forecast steps)")
expect_identical(inv$step,    c(24L, 48L), info = "Must contain step 24 and 48")
expect_identical(inv$init,    rep(as.POSIXct("2017-10-01", tz = "UTC"), 2L), info = "Testing 'init' date")
expect_identical(inv$valid,   as.POSIXct("2017-10-01", tz = "UTC") + c(24, 48) * 3600, info = "Testing 'valid' date; init date + forecast step")
expect_identical(inv$type,    rep("cf", 2L), info = "Type must be 'cf' (control forecast)")
expect_identical(inv$levtype, rep("sfc", 2L), info = "Only surface variables")
rm(list = c("conf", "inv"))


# -------------------------------------------------------------------
# Testing a specific ensemble forecast request, one parameter, member 0 and 20, one forecast step
# -------------------------------------------------------------------
expect_silent(conf <- eupp_config("forecast", "surface", "ens", date = "2017-10-01",
                                  parameter = "2t", steps = 12, members = c(0, 20), cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(conf, verbose = TRUE))
expect_identical(class(inv), c("eupp_inventory", "data.frame"), info = "Return must be of class c(\"eupp_inventory\", \"data.frame\")")
expect_identical(nrow(inv),   2L, info = "Inventory must contain two rows (one parameter, one step, two members)")
expect_identical(inv$step,    rep(12L, 2L), info = "Forecast step 12")
expect_identical(inv$init,    rep(as.POSIXct("2017-10-01", tz = "UTC"), 2L), info = "Testing 'init' date")
expect_identical(inv$valid,   rep(as.POSIXct("2017-10-01", tz = "UTC") + 12 * 3600, 2L), info = "Testing 'valid' date; init date + forecast step")
expect_identical(inv$type,    c("cf", "pf"), info = "Type must be c(\"cf\", \"pf\") (one control forecast and one perturbed forecast)")
expect_identical(inv$levtype, rep("sfc", 2L), info = "Only surface variables")
rm(list = c("conf", "inv"))


# -------------------------------------------------------------------
# Getting one variable, one step, all members of one reforecast run
# -------------------------------------------------------------------
expect_silent(conf <- eupp_config("reforecast", "pressure", "ens", date = "2017-01-09",
                                  parameter = "t", steps = 12, cache = cache_dir))
expect_silent(inv <- eupp_get_inventory(conf, verbose = TRUE))
expect_identical(class(inv), c("eupp_inventory", "data.frame"), info = "Return must be of class c(\"eupp_inventory\", \"data.frame\")")
# Number of rows? One param (t on 850), one step, one init date, all members (10+1), 
# and reforecast over the past 20 years = 1 * 1 * 1 * 11 * 20 = 11 * 20 = 220
expect_identical(nrow(inv),   220L, info = "Inventory must contain 220 rows (reforecast)")
expect_identical(inv$step,    rep(12L, 220L), info = "Forecast step 12")
expect_identical(inv$init,    rep(as.POSIXct("2017-01-09", tz = "UTC"), 220L), info = "Testing 'init' date")
expect_true(all(format(inv$valid, "%m-%d") == "01-09"), info = "Valid month/day must match initialization month/day")
expect_identical(sum(inv$type == "cf"),  20L, info = "Checking number of 'cf' fields")
expect_identical(sum(inv$type == "pf"), 200L, info = "Checking number of 'pf' fields")
expect_identical(inv$levtype, rep("pl", 220L), info = "Only surface variables")
expect_identical(inv$levelist, rep("850", 220L), info = "Level 850 (t850) only")
rm(list = c("conf", "inv"))




