#Master Makefile for BINSYN

## General Makefile Setup ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  export FC = gfortran
endif
ifeq ($(FFLAGS),)
  export FFLAGS = -g
endif
ifeq ($(MAKEOPTS),)
  MAKEOPTS := --ignore-errors
endif

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o

# Find all Makefiles in this project directory
makeFiles = $(sort $(wildcard */Makefile))
#List of subsystems
subsystems = $(subst /Makefile,,$(makeFiles))

# Build Main
.PHONY: all
all: $(subsystems)

.PHONY: $(subsystems)
$(subsystems):
	$(MAKE) $(MAKEOPTS) -C $@

# Test Actions
workingDataDir = IA
testDataDir = SYSTEMS
testDataFiles = $(wildcard $(testDataDir)/$^/* )
# loads the test data into IA $(workingDataDir)
define loadTest
	rm $(workingDataDir)/*
	cp $(testDataDir)/$@/* $(workingDataDir)/.
endef
# Test Rules
TEST001:
# Testing rules.  Each intput file type

# Clean Actions
cleanSubs = $(subsystems)
.PHONY: cleanall cleanobj
cleanall:
	for dir in $(subsystems); \
	do \
	  $(MAKE) -C $$dir cleanall; \
	done

cleanobj:
	for dir in $(subsystems); \
	do \
	  $(MAKE) -C $$dir cleanobj; \
	done
