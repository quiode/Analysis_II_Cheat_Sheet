FILE=cheat_sheet
SHELL:=/usr/bin/env bash

compile: $(FILE).pdf

%.pdf: %.typ
	typst compile "$<" --input REV=$(git rev-parse --short HEAD)

edit: $(FILE).typ open
	typst watch $(FILE).typ --input REV=$(git rev-parse --short HEAD)

open: $(FILE).pdf
	xdg-open $(FILE).pdf &

clean:
	rm *.pdf

.PHONY: clean all edit open compile