all: move rmd2md

vignettes:
		cd inst/vign;\
		Rscript -e 'library(knitr); knit("elastic_dsl.Rmd")'

move:
		cp inst/vign/elastic_dsl.md vignettes

rmd2md:
		cd vignettes;\
		mv elastic_dsl.md elastic_dsl.Rmd
