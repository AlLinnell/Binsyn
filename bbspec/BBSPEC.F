      PROGRAM BBSPEC
      USE MSFLIB
C     Program to produce file of black body fluxes, for an assigned
C     temperature, suitable for plotting
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION FL1(2)
	COMMON /NUNIT/ NU,NUOUT,IFOUT
	CHARACTER * 24 FL1
	CHARACTER * 7 LABEL
	INTEGER(4) RESULT
	DATA IN1,NOUT/1,2/
  100 FORMAT (A24)
  101 FORMAT (F10.3,D15.6)
  102 FORMAT (65X,I6)
C**********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
	OPEN (UNIT = 3, FILE = 'C:\LGTSYN\BBSPEC\BBSPEC.ILS',STATUS='OLD')
	READ (3,100)           (FL1(J),J=1,2)
	OPEN (UNIT = IN1,  FILE = FL1(1),  STATUS = 'OLD')
	OPEN (UNIT = NOUT, FILE = FL1(2),  STATUS = 'REPLACE')
C**********************************************************************
      LBLERR=0
	LABEL = '*BBSPEC'
	NU=IN1
	CALL GTLBL( LABEL, LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL DREAD (TEFF, 'F7.3')
	CALL DUMMY (1)
	CALL DREAD (VLAM1, 'F7.4')
	CALL DUMMY (1)
	CALL DREAD (VLAM2, 'F7.4')
	CALL DUMMY (1)
	READ (NU,102) NORD
	DV=NORD
	DLAM=VLAM2-VLAM1
	LDV=NORD+1
	DO J=1,LDV
	VLAM=VLAM1+(J-1)*DLAM/DV
	CALL BBDY(TEFF,VLAM,FLUX)
C     Convert to wavelength in angstroms
C     Convert to H(lambda) for comparison with Hubeny flux, as listed
C     in SYNSPEC, file SYNOUT.7
      WRITE (NOUT,101) VLAM*10000.D0,FLUX/40000.D0
	END DO
C**********************************************************************
      CLOSE (UNIT = IN1,   STATUS = 'KEEP')
	CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
C**********************************************************************
      STOP
	END