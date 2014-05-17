## Project defaults ##
DEFAULTS := BINSYN
export DEFAULTS

SHELL = /bin/sh
ifeq ($(FC),f77)
  FC := gfortran
  export FC
endif
## Compiler Flags
  OLDFF := $(FFLAGS)
  FFLAGS := -g -ffpe-trap=invalid,zero,overflow -fbacktrace -fdump-core -mtune=native
  ifeq ($(O),2)
    FFLAGS := $(FFLAGS) -O2
  endif
  ifdef MP
    FFLAGS := $(FFLAGS) -fopenmp
  endif
  FFLAGS := $(FFLAGS) $(OLDFF)
  FFLAGS := $(strip $(FFLAGS))
  export FFLAGS
  FFLAGSFILE := .lastCompileFlags
  export FFLAGSFILE

srcFiles := $(wildcard *.F)
includeFiles := $(wildcard *.FOR)
objectFiles := $(srcFiles:.F=.o)
# definitions required for building include/header dependencies
#   a variable holding the grep command, can be used as a function
grepSrcFiles = grep -l "^[^C].*INCLUDE.*$(1)" $(srcFiles)
#   headerDepends.  Takes any header file as argument 1
#   and returns the makefile rule with all the objects that 
#   depend on that header file in one line.
define headerDepends
 $(subst .F,.o,$(shell $(call grepSrcFiles,$(1)))) : $(1)
endef
# get last compiled flags, and update if necessary
#lastFFLAGS := $(shell if [ -r .lastCompileFlags ]; then cat .lastCompileFlags; else :; fi)
lastFFLAGS := $(shell if [ -r $(FFLAGSFILE) ]; then cat $(FFLAGSFILE) ; else :; fi)

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o

