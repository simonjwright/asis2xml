# Makefile for ASIS2XML.

all::

dist::

clean::


#######
# Build
#
# Expects asis.gpr and xmlada.gpr to be on ADA_PROJECT_PATH.

all:: .build asis2xml

asis2xml: force
	gnatmake -Pasis2xml

.build:
	mkdir .build

.PHONY: force


############################
# Distribution construction

# Create the current date, in the form yyyymmddSSS (SSS is the value
# of SUBRELEASE, default svn).
# You can override the use of today's date and subrelease by setting
# DATE on the make command line. This might be useful for a patch
# release.
SUBRELEASE = svn
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS = \
COPYING \
INSTALL

SRC = \
asis2xml.adb \
xml_support.ad[bs]

BUILDING = \
Makefile \
asis2xml.gpr

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

dist:: $(DISTRIBUTION_FILES)
	touch dist


#######
# Clean

clean::
	rm -f asis2xml
	rm -rf .build
	rm -f dist
