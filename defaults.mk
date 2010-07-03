## Project defaults ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  export FC := gfortran
endif
ifeq ($(FFLAGS),)
#  FFLAGS := -g -Wall
  export FFLAGS := -g -ffpe-trap=invalid,zero,overflow -fbacktrace -fdump-core
endif

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o
