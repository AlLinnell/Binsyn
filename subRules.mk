## Default Rules for sub-programs ##

## Include Global Defaults Makefile ##
include ../defaults.mk
 
# Build Main
$(prog): $(objectFiles)
	$(FC) $(FFLAGS) -o $(prog) $^

# Build objects
$(objectFiles): %.o:%.F 
	$(FC) $(FFLAGS) -c $< -o $@

# create phony dependancy if flags change
ifneq ($(lastFFLAGS),$(FFLAGS))
$(objectFiles): flagChange
.PHONY: flagChange
flagChange:
	@echo "Compiler options changed, recompile needed."
	@echo $(FFLAGS) >$(FFLAGSFILE)
endif

# Include/Header File Dependencies
# Calls headerDepends for very header file, and then evaluates the result as a makefile rule
ifdef includeFiles
  $(foreach header,$(includeFiles),$(eval $(call headerDepends,$(header))))
endif

# Other Actions
.PHONY: clean cleanall cleanobj
clean: cleanall
cleanall:
	-rm *.o $(prog)
cleanobj:
	-rm *.o

# Debuging
.PHONY: show
show:
	@echo command template to find header deps: $(grepSrcFiles)
	@echo command to find COMA.FOR deps: $(call grepSrcFiles,COMA.FOR)
	@echo files that depend on COMA.FOR: $(shell $(call grepSrcFiles,COMA.FOR))
#	@echo $(subst .F,.o,$(shell $(call grepSrcFiles,COMA.FOR))) : COMA.FOR
#	@echo $(call headerDepends,COMA.FOR)
#	@echo $(call headerDepends,COMM.FOR)
#	@echo $(call headerDepends,COMR.FOR)
	@echo "last used flags:"\"$(lastFFLAGS)\"
	@echo "current flags  :"\"$(FFLAGS)\"
