



document:
	Rscript -e 'devtools::document()'

install: document
	Rscript -e 'devtools::install()'
	Rscript -e 'library("EUPPB"); download(dataset("reforecasts", "surface", "2021-02-02", "t2")'

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
