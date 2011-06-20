## Project defaults ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  FC := gfortran
  export FC
endif
ifeq ($(FFLAGS),)
#  FFLAGS := -g -Wall
  FFLAGS := -g -ffpe-trap=invalid,zero,overflow -fbacktrace -fdump-core -mtune=native
  ifeq ($(O),2)
    FFLAGS := $(FFLAGS) -O2
  endif
  ifdef MP
    FFLAGS := $(FFLAGS) -fopenmp
  endif
  export FFLAGS
endif

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o
