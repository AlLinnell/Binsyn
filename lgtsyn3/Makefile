#Makefile for lgtsyn3

## General Makefile Setup ##
include ../defaults.mk

# Important definitions
#mainFile := CALPT.F
prog := lgtsyn3
srcFiles := $(wildcard *.F)
objectFiles := $(srcFiles:.F=.o)
# Libararies
# libs are configured by parent Makefile, see $(libDirs)
libDirs := ../lib
modDirs := ../modules
moduleSources = $(wildcard $(addsuffix /*.FOR, $(modDirs) ) )
modules = $(wildcard $(addsuffix /*.mod,$(modDirs)))
moduleObjects = $(wildcard $(addsuffix /*.o,$(modDirs)))
moduleNames = $(foreach file,$(basename $(moduleSources)), $(notdir $(file)))
# Headers
includeDirs := ../include
ifeq ($(FPPFLAGS),)
  FPPFLAGS := $(addprefix -I, $(includeDirs)) $(addprefix -L, $(libDirs)) $(addprefix -J, $(modDirs))
endif
# definitions required for building include/header dependencies
#   a variable holding the grep command, can be used as a function
grepSrcFiles = egrep -l "^[^C]\W\W*USE\W\W*$(1)" $(srcFiles)
grepModFiles = egrep "^[^C]\W\W*MODULE\W\W" $(srcFiles)
#   buildModDepends.  Takes any header file as argument 1
#   and returns the makefile rule with all the objects that 
#   depend on that header file in one line.
define buildModDepends
 $(subst .F,.o,$(shell $(call grepSrcFiles,$(1)))) : $(filter %/$(1).o, $(moduleObjects))
endef


# Program/Local-specific settings
prog := lgtsyn3

# Build Main
$(prog): $(objectFiles)
	$(FC) $(FFLAGS) $(FPPFLAGS) -o $(prog) $(objectFiles)

$(objectFiles): %.o: %.F
	$(FC) $(FFLAGS) $(FPPFLAGS) -c $< -o $@
# module dependencies
# Calls buildModDepends for every module obj, and then evaluates the result as a makefile rule
ifdef moduleNames
  $(foreach mod,$(moduleNames),$(eval $(call buildModDepends,$(mod))))
endif


# Other Actions
rmObjects = $(wildcard *.o)
rmProg = $(wildcard $(prog))
.PHONY: cleanall cleanobj
cleanall: cleanobj
	@if [ ! -z "$(rmProg)" ]; then rm $(rmProg); else echo "No program to clean."; fi

cleanobj:
	@if [ ! -z "$(rmObjects)" ]; then rm $(rmObjects); else echo "No objects to clean."; fi

# Debuging
.PHONY: show
show:
	@echo FPPFLAGS=$(FPPFLAGS)
	@echo modules = $(modules)
	@echo module Src = $(moduleSources)
	@echo module names = $(moduleNames)
	@echo module objects = $(moduleObjects)
	@echo grepSrcFiles = $(grepSrcFiles)
	@echo "grepSrcFiles(COMA) = $(call grepSrcFiles,COMA)"
	@echo "run grp(COMA) = $(shell $(call grepSrcFiles,COMA))"
	@echo "run grp(COMA)+sub F=o = $(subst .F,.o,$(shell $(call grepSrcFiles,COMA))) : ../modules/COMA.o"
	@echo "filter COMA = $(filter %/COMA.o, COMA.o modules/ACOMA.o ../modules/COMA.o)"
	@echo "buildModDepends(COMA) = $(call buildModDepends,COMA)"
	@echo ACOMA depends = $(call buildModDepends,ACOMA)
	@echo COMD depends = $(call buildModDepends,COMD)
	@echo COMDA depends = $(call buildModDepends,COMDA)
	@echo COMM depends = $(call buildModDepends,COMM)
	@echo COMN depends = $(call buildModDepends,COMN)
	@echo COMR depends = $(call buildModDepends,COMR)
	@echo COMPG5 depends = $(call buildModDepends,COMPG5)
	@echo COMPH depends = $(call buildModDepends,COMPH)
