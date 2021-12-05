



document:
	Rscript -e 'devtools::document()'

install: document
	Rscript -e 'devtools::install(upgrade = FALSE)'

check: document
	Rscript -e 'devtools::check()'

.PHONY: docs
docs:
	make install
	Rscript -e 'pkgdown::build_site()'

#test:
#	Rscript -e 'devtools::load_all(); tinytest::test_all()'
#
#coverage:
#	Rscript -e 'covr::report(covr::package_coverage(), file = "coverage.html")'

#clean:
#	-rm -rf summary
#	-rm test.html
#	-rm inst/demo/demo_*.html
#
