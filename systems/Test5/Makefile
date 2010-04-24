#Makefile for Test5

## General Makefile Setup ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  FC := gfortran
endif
ifeq ($(FFLAGS),)
  FFLAGS := -g -Wall
endif

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o

filesToMove = *.INF *.IUP
# Important definitions
# Build Main
all:
	cp $(filesToMove) ../../ia/.
