CABAL_FILE := minilens.cabal
MODULE_TOP := Data/Lens

####################################################################################################

PROJECT_NAME    := $(shell grep ^name: -i $(CABAL_FILE) | sed -e 's,^name:[[:space:]]*,,i')
PROJECT_VERSION := $(shell grep ^version: -i $(CABAL_FILE) | sed -e 's,version:[[:space:]]*,,i')
PROJECT_DIR     := $(PROJECT_NAME)
RELEASE_NAME    := $(PROJECT_NAME)-$(PROJECT_VERSION)
ARCHIVE_NAME    := $(RELEASE_NAME).tar.gz

SOURCES := $(shell find src -type f -name '*.hs')

LIBRARY := dist/build/$(PROJECT_NAME)
export VIM_SESSION := .$(PROJECT_NAME).vim

.PHONEY: all install clean edit configure config archive

# This exists because it is easier to use than figure out how to make vim
# properly parse compile time error log messages from GHC.
FILTER := \
    sed -e 's,^src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+.*$$,& ,' \
        -e 's,[(]bound at \(src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\)[)],...bound at...\n\1: \n,' \
        -e 's,^[[:space:]]*at \(src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\),...at...\n\1: ,'

$(LIBRARY): $(CABAL_FILE) $(SOURCES)
	cabal build 2>&1 | $(FILTER)

install: $(LIBRARY)
	cabal install --force-reinstalls | $(FILTER)

clean:
	rm -Rf dist

config: configure

configure:
	cabal configure

edit:
	if [ -f '$(VIM_SESSION)' ]; \
	then vim -S '$(VIM_SESSION)'; \
	else vim '$(CABAL_FILE)' '$(SOURCES)' Makefile; \
	fi;

TAGS tags: $(SOURCES)
	hasktags '$(SOURCES)'

../$(ARCHIVE_NAME): $(LIBRARY)
	mkdir -p ../$(RELEASE_NAME); \
	if tar cf - $(CABAL_FILE) Setup.hs LICENSE $(SOURCES) | \
			tar xvf - -C ../'$(RELEASE_NAME)' | sed -e 's,^,COPY: ,'; \
	then \
		if tar czvf ../'$(ARCHIVE_NAME)' --format=ustar -C ../ '$(RELEASE_NAME)' | sed -e 's,^,ARCHIVE: ,'; \
		then rm -Rf ../'$(RELEASE_NAME)'; \
		fi; \
	fi;

archive: ../$(ARCHIVE_NAME)

