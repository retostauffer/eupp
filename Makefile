



document:
	Rscript -e 'devtools::document()'

install: document
	Rscript -e 'devtools::install(upgrade = FALSE)'

check: document
	Rscript -e 'devtools::check(vignettes = FALSE)'

.PHONY: docs
docs:
	make install
	Rscript -e 'pkgdown::build_site()'

test:
	Rscript -e 'devtools::load_all(); tinytest::test_all()'

testgridded:
	Rscript -e 'devtools::load_all(); tinytest::run_test_file("inst/tinytest/test_gridded.R")'

coverage:
	Rscript -e 'covr::report(covr::package_coverage(), file = "coverage.html")'


#clean:
#	-rm -rf summary
#	-rm test.html
#	-rm inst/demo/demo_*.html
#
