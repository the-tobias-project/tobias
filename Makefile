init:
	rm -rf renv && \
	rm renv.lock && \
	rm .Rprofile && \
	R -s -e "install.packages(c('devtools', 'roxygen2', 'lintr', 'formatR', 'attachment', 'typed', 'testthat'), repos = 'https://cloud.r-project.org/')"

document: 
	R -s -e "attachment::att_amend_desc()" && \
	R -s -e "devtools::document()"

check:
	R -s -e "devtools::check(error_on=eval($(ERROR_ON)))" 

test:
	R -s -e "devtools::test()

static: 
	R -s -e 'lintr::lint_package()'

format:
	R -s -e "formatR::tidy_dir('R', recursive = TRUE, keep.comment = TRUE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: activate init document format check static 


install: 
	R -s -e "devtools::install('.', dependencies = TRUE)"


.PHONY : activate install init document