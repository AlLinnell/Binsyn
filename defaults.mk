## Project defaults ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  FC := gfortran
  export FC
endif
ifeq ($(FFLAGS),)
#  FFLAGS := -g -Wall
#  FFLAGS := -g -ffpe-trap=invalid,zero,overflow -fbacktrace -fdump-core
  FFLAGS := -g -ffpe-trap=invalid,zero,overflow -fbacktrace -fdump-core -mtune=native
  ifeq ($(O),2)
    FFLAGS := $(FFLAGS) -O2
  endif
  ifdef MP
    FFLAGS := $(FFLAGS) -fopenmp
  endif
  export FFLAGS
endif
## For now, ignore make errors, so that the whole project can
## be compiled with a single make.
ifeq ($(MAKEOPTS),)
  MAKEOPTS := --ignore-errors
endif

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
lastFFLAGS := $(shell cat .lastCompileFlags)

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o

