.PHONY : activate install init document

activate:
	direnv allow


init: activate
	cd ${REPO} && \
	R --vanilla --slave -e "renv::restore(); \
	install.packages(c('devtools', 'renv', 'roxygen2', 'lintr', 'formatR', 'attachment', 'typed'), repos = 'https://cloud.r-project.org/')"


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
	R --vanilla -e "formatR::tidy_dir('${REPO}/R', recursive = TRUE, keep.comment = TRUE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: init document format check static 


clean: 
	rm -rf ${REPO}*.tar.gz


configure:
	echo '{"host": "${databricks_host}", "token":"${databricks_token}", "cluster_id":"${databricks_cluster_id}", "org_id":"${databricks_org_id}", "port":"${databricks_port}"}' | jq . > ~/.databricks-connect 
	databricks-connect test
	

install: init clean configure
	cd ${REPO} && \
	R --vanilla --slave -e "devtools::build()" && \
	cd .. & R --vanilla -e "devtools::install('${REPO}', dependencies = TRUE)"

