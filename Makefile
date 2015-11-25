PROJECT_NAME := minilens
MODULE_TOP := Data/Lens

####################################################################################################

SOURCES := $(shell find src -type f -name '*.hs')

LIBRARY := dist/build/$(PROJECT_NAME)
CABAL_FILE := ./$(PROJECT_NAME).cabal
export VIM_SESSION := ./.$(PROJECT_NAME).vim

.PHONEY: all install clean edit configure config

# This exists because it is easier to use than figure out how to make vim
# properly parse compile time error log messages from GHC.
FILTER := \
    sed -e 's,^src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+.*$$,& ,' \
        -e 's,[(]bound at \(src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\)[)],...bound at...\n\1: \n,' \
        -e 's,^[[:space:]]*at \(src/$(MODULE_TOP)/[^.]\+[.]hs:[0-9:]\+\),...at...\n\1: ,'

$(LIBRARY): $(CABAL_FILE) $(SOURCES)
	cabal build 2>&1 | $(FILTER)

install: $(LIBRARY)
	cabal install | $(FILTER)

clean:
	rm -Rf dist

config: configure

configure:
	cabal configure

edit:
	if [ -f $(VIM_SESSION) ]; \
	then vim -S $(VIM_SESSION); \
	else vim $(CABAL_FILE) $(SOURCES) Makefile; \
	fi;

TAGS tags: $(SOURCES)
	hasktags $(SOURCES)

