# Makefile for ASIS2XML.

all::

install::

dist::

clean::


#######
# Build
#
# Expects asis.gpr and xmlada.gpr to be on ADA_PROJECT_PATH.

all:: asis2xml

asis2xml: force
	gprclean -p -Pasis2xml

.PHONY: force

install::
	gprinstall --mode=usage -P asis2xml


############################
# Distribution construction

# Create the current date, in the form yyyymmddSSS (SSS is the value
# of SUBRELEASE, default hg).
# You can override the use of today's date and subrelease by setting
# DATE on the make command line. This might be useful for a patch
# release.
SUBRELEASE = hg
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DOCS =						\
CHANGES						\
COPYING						\
INSTALL

SRC =						\
asis2xml.adb					\
xml_support.ad[bs]

BUILDING =					\
Makefile					\
asis2xml.gpr

DISTRIBUTION_FILES =				\
asis2xml-$(DATE).tgz				\
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
.PHONY: dist


############################
# Documentation upload to SF

SFUSER ?= simonjwright

upload-docs:: index.html sjw.css
	rsync \
	  --compress \
	  --copy-unsafe-links \
	  --cvs-exclude \
	  --delete \
	  --perms \
	  --recursive \
	  --rsh=ssh \
	  --times \
	  --update \
	  --verbose \
	  $^ \
	  $(SFUSER),asis2xml@web.sourceforge.net:htdocs/

#######
# Clean

clean::
	gprclean -P asis2xml
	rm -rf asis2xml-*

.PHONY: clean
