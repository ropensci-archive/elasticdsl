PACKAGE := $(shell grep '^Package:' DESCRIPTION | sed -E 's/^Package:[[:space:]]+//')
RSCRIPT = Rscript --no-init-file

all: move rmd2md

vignettes:
		cd inst/vign;\
		Rscript -e 'library(knitr); knit("elastic_dsl.Rmd")'

move:
		cp inst/vign/elastic_dsl.md vignettes

rmd2md:
		cd vignettes;\
		mv elastic_dsl.md elastic_dsl.Rmd

install: doc build
	R CMD INSTALL . && rm *.tar.gz

build:
	R CMD build .

doc:
	${RSCRIPT} -e "devtools::document()"

eg:
	${RSCRIPT} -e "devtools::run_examples(run = TRUE)"

check: build
	_R_CHECK_CRAN_INCOMING_=FALSE R CMD CHECK --as-cran --no-manual `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -f `ls -1tr ${PACKAGE}*gz | tail -n1`
	@rm -rf ${PACKAGE}.Rcheck

test:
	${RSCRIPT} -e "devtools::test()"

readme:
	${RSCRIPT} -e "knitr::knit('README.Rmd')"
