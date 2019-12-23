.PHONY: pubdocs

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)

# src/utils.lisp: src/make-utilities.lisp
# 	cd src && sbcl --noinform --load make-utilities.lisp  --eval '(quit)'

$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/docs.stevelosh.com pull -u
	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/cl-ggp
	hg -R ~/src/docs.stevelosh.com commit -Am 'cl-ggp: Update site.'
	hg -R ~/src/docs.stevelosh.com push

