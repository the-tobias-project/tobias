.PHONY : activate install init document

activate:
	direnv allow


init: activate
	cd ${REPO} && \
	R --vanilla --slave -e "renv::restore(); \
	install.packages(c('devtools', 'renv', 'roxygen2', 'lintr', 'formatR', 'attachment', 'typed'), repos = 'https://cloud.r-project.org/')"


clean: 
	rm -rf ${REPO}*.tar.gz


install: init clean
	cd ${REPO} && \
	R --vanilla --slave -e "devtools::build()" && \
	cd .. & R --vanilla -e "devtools::install('${REPO}', dependencies = TRUE)"


document: activate
	cd ${REPO} && \
	R --vanilla --slave -e "attachment::att_amend_desc()" && \
	R --vanilla --slave -e "devtools::document()"


check: activate
	cd ${REPO} && \
	R --vanilla --slave -e "devtools::check()" && \
	R --vanilla --slave -e "devtools::test()"


static: activate
	cd ${REPO} && \
	R --vanilla --slave -e 'lintr::lint_package()'


format: activate
	R --vanilla -e "formatR::tidy_dir('${REPO}/R', recursive = TRUE, keep.comment = FALSE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: document format check static 

