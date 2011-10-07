      PROGRAM BBSPEC
C     Program to produce file of black body fluxes, for an assigned
C     temperature, suitable for plotting
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION FL1(2)
	COMMON /NUNIT/ NU,NUOUT,IFOUT
	CHARACTER * 24 FL1
	CHARACTER * 7 LABEL
	DATA IN1,NOUT/1,2/
  100 FORMAT (A24)
  101 FORMAT (F10.3,D15.6)
  102 FORMAT (65X,I6)
C**********************************************************************
	OPEN (UNIT = 3, FILE = 'BBSPEC.ILS',STATUS='OLD')
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
C       Convert to Kelvins
        TEFF=TEFF*1.0D3
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
C     Convert to cm.
        VLAMA=VLAM/1.0D4
	CALL BBDY(TEFF,VLAMA,FLUX)
      WRITE (NOUT,101) VLAM*1.0D4,FLUX
	END DO
C**********************************************************************
      CLOSE (UNIT = IN1,   STATUS = 'KEEP')
	CLOSE (UNIT = NOUT,  STATUS = 'KEEP')
C**********************************************************************
      STOP 'BBSPEC Completed successfully.'
	END PROGRAM BBSPEC
