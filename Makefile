.PHONY: all clean

PROGNAME=futswirl
LYS_MAKEFILE=lib/github.com/diku-dk/lys/common.mk
BASE_LOCAL_DEPS=settings.fut lib

all:
	$(MAKE) $(BASE_LOCAL_DEPS)
	PROGNAME=$(PROGNAME) $(MAKE) --file=$(LYS_MAKEFILE)

run:
	$(MAKE) $(BASE_LOCAL_DEPS)
	PROGNAME=$(PROGNAME) $(MAKE) --file=$(LYS_MAKEFILE) run

clean:
	$(MAKE) $(BASE_LOCAL_DEPS)
	PROGNAME=$(PROGNAME) $(MAKE) --file=$(LYS_MAKEFILE) clean

lib: futhark.pkg
	futhark pkg sync

settings.fut: settings_template.fut
	cp settings_template.fut settings.fut
