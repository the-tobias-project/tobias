
activate:
	direnv allow

init:
	cd ${REPO} && \
	R --vanilla -s -e " \
		install.packages(c('devtools', 'renv', 'roxygen2', 'lintr', 'formatR', 'attachment', 'typed', 'testthat'), repos = 'https://cloud.r-project.org/'); \
		renv::restore()"


document: 
	cd ${REPO} && \
	R --vanilla -s -e "attachment::att_amend_desc()" && \
	R --vanilla -s -e "devtools::document()"


check:
	cd ${REPO} && \
	R --vanilla -s -e "devtools::check()" && \
	R --vanilla -s -e "devtools::test()"


static: 
	cd ${REPO} && \
	R --vanilla -s -e 'lintr::lint_package()'


format:
	R --vanilla -e "formatR::tidy_dir('${REPO}/R', recursive = TRUE, keep.comment = TRUE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: activate init document format check static 


clean: 
	rm -rf ${REPO}*.tar.gz
	

install_package: init
	cd ${REPO} && \
	R --vanilla -s -e "devtools::build()" && \
	cd .. & R --vanilla -s -e "devtools::install('${REPO}', dependencies = TRUE)"


install: activate init configure install_package clean

.PHONY : activate install init document