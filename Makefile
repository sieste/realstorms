.SUFFIXES:
MAKEFLAGS += -r

PKGFILES := $(shell find R man -type f ! -name "*.swp") DESCRIPTION NAMESPACE
PACKAGE := $(shell awk -F": +" '/^Package/ { print $$2 }' DESCRIPTION)
VERSION := $(shell awk -F": +" '/^Version/ { print $$2 }' DESCRIPTION)
R_PKG_tgz := $(PACKAGE)_$(VERSION).tar.gz


.PHONY: all readme build install clean

all: build install
build: $(R_PKG_tgz)
install: libtmp/$(PACKAGE)

$(R_PKG_tgz): $(PKGFILES)
	R -e 'roxygen2::roxygenize(package.dir=".", clean=TRUE)';\
	R CMD build .

libtmp/$(PACKAGE): $(R_PKG_tgz)
	mkdir -p libtmp;\
	R CMD INSTALL $(R_PKG_tgz) -l libtmp

check: 
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz)

check-cran: 
	R_LIBS=/home/stefan/lib/R R CMD check $(R_PKG_tgz) --as-cran; \
	R_LIBS=/home/stefan/lib/R R-devel CMD check $(R_PKG_tgz) --as-cran

readme: README.md

README.md: README.Rmd
	Rscript -e 'knitr::knit("README.Rmd")'

clean:
	rm $(R_PKG_tgz);\
	rm -rf libtmp/$(PACKAGE)

