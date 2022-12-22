
init:
	rm -rf renv; rm .renv.lock; R -s -e "install.packages(c('devtools', 'roxygen2', 'lintr', 'formatR', 'attachment', 'typed', 'testthat'), repos = 'https://cloud.r-project.org/')"

document: 
	R --vanilla -s -e "attachment::att_amend_desc()" && \
	R --vanilla -s -e "devtools::document()"

check:
	R --vanilla -s -e "devtools::check()" && \
	R --vanilla -s -e "devtools::test()"


static: 
	R --vanilla -s -e 'lintr::lint_package()'

format:
	R --vanilla -e "formatR::tidy_dir('R', recursive = TRUE, keep.comment = TRUE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: activate init document format check static 


install: 
	R -s -e "devtools::install('.', dependencies = TRUE)"


.PHONY : activate install init document