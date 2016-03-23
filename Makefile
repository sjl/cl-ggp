.PHONY: pubdocs

sourcefiles = $(shell ffind --full-path --dir src --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidoc = docs/03-reference.markdown

# src/utils.lisp: src/make-utilities.lisp
# 	cd src && sbcl --noinform --load make-utilities.lisp  --eval '(quit)'

$(apidoc): $(sourcefiles) docs/api.lisp package.lisp
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs: docs/build/index.html

docs/build/index.html: $(docfiles) $(apidoc) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

pubdocs: docs
	hg -R ~/src/sjl.bitbucket.org pull -u
	rsync --delete -a ./docs/build/ ~/src/sjl.bitbucket.org/cl-ggp
	hg -R ~/src/sjl.bitbucket.org commit -Am 'cl-ggp: Update site.'
	hg -R ~/src/sjl.bitbucket.org push

