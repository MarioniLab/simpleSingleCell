all: book

OSCA:
	git clone https://github.com/Bioconductor/OSCABase OSCA

book: OSCA
	for x in $(shell ls OSCA/*.Rmd); do \
		cat $$x | sed "s/rebook/simpleSingleCell/g" > blah; \
		mv blah $$x; \
	done
	cd OSCA && ${R_HOME}/bin/R -e "bookdown::render_book('index.Rmd')"
	rm -rf OSCA/_bookdown_files/

clean: 
	rm -rf *_cache *_files
