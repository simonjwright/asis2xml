# $Id: Makefile,v 1.2 2004/10/24 06:45:43 simon Exp $
#
# Makefile for ASIS2XML.

all::

dist::

clean::


#######
# Build

all:: .build asis2xml

asis2xml: force
	gnatmake -Pbuild

.build:
	mkdir .build

.PHONY: force


############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works
# in Linux.
# You can override the use of today's date by setting DATE on the make
# command line. This might be useful for a patch release.
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

HTMLDOCS = \
index.html

DOCS = \
COPYING \
INSTALL

SRC = \
asis2xml.adb \
xml_support.ad[bs] \
normalize-xml.py

BUILDING = \
Makefile \
build.gpr

DISTRIBUTION_FILES = \
asis2xml-$(DATE).tgz \
asis2xml-$(DATE).zip

asis2xml-$(DATE).tgz: asis2xml-$(DATE)
	-rm $@
	tar zcvf $@ $</

asis2xml-$(DATE).zip: asis2xml-$(DATE)
	-rm $@
	zip -r $@ $</*

asis2xml-$(DATE): $(DOCS) $(SRC) $(BUILDING)
	-rm -rf $@
	mkdir $@
	cp -p $(DOCS) $@
	cp -pR $(SRC) $@
	cp -p $(BUILDING) $@

dist:: $(DISTRIBUTION_FILES) $(HTMLDOCS)
	-@rm -rf dist
	mkdir -p dist/download
	cp -p $(HTMLDOCS) dist/
	cp $(DISTRIBUTION_FILES) dist/download/


#######
# Clean

clean::
	rm -rf .build
	rm -f asis2xml
