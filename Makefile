#Master Makefile for BINSYN

## Read in project default settings
include defaults.mk

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

runGRPall: runGRPA runGRPB runGRPC runGRPD runGRPE

runGRPA:
	@echo "Running GRPA..."
	cd grpa/; ./grpascr
	@echo "...done"

runGRPB: runGRPA
	@echo "Running GRPB..."
	cd grpb/; ./grpbscr

runGRPC: runGRPB
	@echo "Running GRPC..."
	cd grpc/; ./grpcscr
	@echo "...done"

runGRPD: runGRPA runGRPB runGRPC
	@echo "Running GRPD..."
	cd grpd/; ./grpdscr
	@echo "...done"

runGRPE:runGRPA runGRPB runGRPC runGRPD
	@echo "Running GRPE..."
	cd grpe/; ./grpescr
	@echo "...done"

iabackup:
	mkdir iabackup
oabackup:
	mkdir oabackup

backupIA: iabackup 
	rsync --verbose --times --inplace --delete --delete-excluded --exclude=".?*" --dirs ./ia/ ./iabackup
backupOA: oabackup
	rsync --verbose --times --inplace --delete --delete-excluded --exclude=".?*" --dirs ./oa/ ./oabackup
backup: backupIA backupOA

restoreIA: iabackup 
	rsync --verbose --times --inplace --delete --exclude=".?*" --dirs ./iabackup/ ./ia
restoreOA: oabackup
	rsync --verbose --times --inplace --delete --exclude=".?*" --dirs ./oabackup/ ./oa
restore: restoreIA restoreOA

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
