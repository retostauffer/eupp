


document:
	Rscript -e 'devtools::document()'

install: document
	Rscript -e 'devtools::install()'

check: document
	Rscript -e 'devtools::check()'

#test:
#	Rscript -e 'devtools::load_all(); tinytest::test_all()'
#
#coverage:
#	Rscript -e 'covr::report(covr::package_coverage(), file = "coverage.html")'

#clean:
#	-rm -rf summary
#	-rm test.html
#	-rm inst/demo/demo_*.html
