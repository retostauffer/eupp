# Running tests
if (requireNamespace("tinytest", quietly = TRUE)) {
    library("tinytest")
    tinytest::test_package("eupp")
}
