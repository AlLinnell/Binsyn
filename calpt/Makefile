#Makefile for calpt

## General Makefile Setup ##
SHELL = /bin/sh
ifeq ($(FC),f77)
  FC = gfortran
endif
ifeq ($(FFLAGS),)
  FFLAGS = -g -Wall
endif

# Clear and set list of suffixes for implicit rules (which we
# will probably not use).
.SUFFIXES: 
.SUFFIXES: .F .o

# Important (prerequisit) objects
mainFile = CALPT.F
srcFiles = CALPT.F CREAD.F CVJD.F DREAD.F DUMMY.F GTLBL.F IREAD.F LAGIN.F LNTER.F PMDSTOP.F RCH0.F ROOTFN.F RTFDA.F SETLAG.F SRTA.F
SRTs = COMA.FOR COMM.FOR COMR.FOR
objectFiles = CALPT.o CREAD.o CVJD.o DREAD.o DUMMY.o GTLBL.o IREAD.o LAGIN.o LNTER.o PMDSTOP.o RCH0.o ROOTFN.o RTFDA.o SETLAG.o SRTA.o

# Build Main
calpt:	$(objectFiles)
	$(FC) $(FFLAGS) -o calpt $^

# Build objects
$(objectFiles): %.o: %.F
	$(FC) $(FFLAGS) -c $< -o $@

# Add some dependancies to objects with includes in the source
CALPT.o: 	COMM.FOR
RCH0.o: 	COMA.FOR
SETLAG.o:  	COMA.FOR

# Other Actions
.PHONY: cleanall cleanobj
cleanall:
	-rm *.o calpt
cleanobj:
	-rm *.o
