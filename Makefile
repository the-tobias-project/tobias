
document: 
	R -s -e "attachment::att_amend_desc()" && \
	R -s -e "devtools::document()"


check:
	R -s -e "devtools::check(error_on=eval('$(ERROR_ON)'))" 


static: 
	R -s -e 'lintr::lint_package()'

format:
	R -s -e "formatR::tidy_dir('R', recursive = TRUE, keep.comment = TRUE, keep.blank.line = FALSE, reindent.spaces = 2)"


prepare: document format check static 


install: 
	R -s -e "devtools::install('.', dependencies = TRUE)"


.PHONY : document check static format prepare install 