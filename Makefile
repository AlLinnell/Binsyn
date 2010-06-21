#Master Makefile for BINSYN

## Read in project default settings
include defaults.mk

## For now, ignore make errors, so that the whole project can
## be compiled with a single make.
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

# Helper Actions
.PHONY: prepB runGRPall
prepB:
	@echo "Running scriptB..."
	./scriptB
	@echo "...Updating PGBX3/6SV.DAT..."
	cd oa/; cp PGBX3.DAT PGBX3SV.DAT ; cp PGBX6.DAT PGBX6SV.DAT
	@echo "...done."
	@echo "...Updateting VVTSTBC6.DAT..."
	cd oa/; cp PGBX6.DAT VVTSTBC6.DAT
	@echo "...done"

runGRPall: runGRPA runGRPB runGRPC runGRPD runGRPE

runGRPA:
	@echo "Running GRPA..."
	cd grpa/; ./grpascr
	@echo "...done"

runGRPB:
	@echo "Running GRPB..."
	cd grpb/; ./grpbscr

runGRPC:
	@echo "Running GRPC..."
	cd grpc/; ./grpcscr
	@echo "...done"

runGRPD:
	@echo "Running GRPD..."
	cd grpd/; ./grpdscr
	@echo "...done"

runGRPE:
	@echo "Running GRPE..."
	cd grpe/; ./grpescr
	@echo "...done"

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
testDirs = $(wildcard systems/*)
.PHONY: $(testDirs)
$(testDirs):
	$(MAKE) $(MAKEOPTS) -C $@


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
