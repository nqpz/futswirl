.PHONY: all clean

PROGNAME=futswirl
LYS_MAKEFILE=lib/github.com/diku-dk/lys/common.mk

all:
	$(MAKE) settings.fut
	PROGNAME=$(PROGNAME) $(MAKE) --file=$(LYS_MAKEFILE)

clean:
	$(MAKE) settings.fut
	PROGNAME=$(PROGNAME) $(MAKE) --file=$(LYS_MAKEFILE) clean

settings.fut: settings_template.fut
	cp settings_template.fut settings.fut
