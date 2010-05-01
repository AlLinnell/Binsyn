      PROGRAM GENRMDAT
C     Program to generate temperature data for accretion disk rim.
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	COMMON /NUNIT/NU,NUOUT,IFOUT
	CHARACTER * 7 LABEL
	CHARACTER * 36 FL1
	DIMENSION FL1(5)
	DIMENSION BUF(501)
	DATA IN1/1/
	DATA NOUT/2/
  100 FORMAT (A36)
  101 FORMAT (10I5)
  102 FORMAT (8F10.1)
C     *****************************************************************
      OPEN (UNIT=5, FILE='GENRMDAT.ILS', 
     $STATUS='OLD')
	READ (5,100) (FL1(J),J=1,2)
	OPEN (UNIT=IN1,FILE=FL1(1),STATUS='OLD')
	OPEN (UNIT=NOUT,FILE=FL1(2),STATUS='REPLACE')
C     *****************************************************************
C
      LBLERR=0
	LABEL = '**RMDAT'
	NU=IN1
	CALL GTLBL(LABEL, LBLERR)
	IF (LBLERR.EQ.(-1)) CALL PMDSTOP
	CALL IREAD (NRM)
	CALL DUMMY (1)
	CALL IREAD (NSG)
	CALL DUMMY(1)
	CALL IREAD (NHT)
	CALL DUMMY(1)
	CALL DREAD (TEMPH,'F10.1')
	CALL DUMMY(1)
	CALL DREAD (TEMPL,'F10.1')
	CALL DUMMY(1)
	CALL DREAD (DAZ1,'F10.1')
	CALL DUMMY(1)
	CALL DREAD (DTMP1,'F10.1')
	CALL DUMMY(1)
	CALL DREAD (DAZ2,'F10.1')
	CALL DUMMY(1)
	CALL DREAD (DTMP2,'F10.1')
	TEMP1=TEMPL+DTMP1
	TEMP2=TEMPL+DTMP2
	DLAZ=360.D0/(NSG-1)
	ST1=90.D0-DAZ1/2.D0
	QT1=90.D0+DAZ1/2.D0
	ST2=270.D0-DAZ2/2.D0
	QT2=270.D0+DAZ2/2.D0
C     Set temperature for uppermost NHT rim rings
	DO K=1,NSG
		BUF(K)=TEMPH
	END DO
	NSTP=NRM-NHT
C     Actually assign temperatures to upper NHT rim rings
	DO J=1,NHT
		WRITE (NOUT,102) (BUF(K),K=1,NSG)
	END DO
C     Set temperature for middle rim rings
	RF=0.D0
	DO K=1,NSG
C     Set default value for temperature
		BUF(K)=TEMPL
C     If azimuth falls within first zone, reassign temperature
		IF ((RF.GE.ST1).AND.(RF.LT.QT1)) THEN
			BUF(K)=TEMP1
		END IF
C     If azimuth falls within second zone, reassign temperature
		IF ((RF.GE.ST2).AND.(RF.LT.QT2)) THEN
			BUF(K)=TEMP2
		END IF
	RF=RF+DLAZ
	END DO
C     Actually assign temperature to middle rim rings
	DO J=NHT+1,NSTP
		WRITE (NOUT,102) (BUF(K),K=1,NSG)
	END DO
C     Set temperature for bottom NHT rin rings
	DO K=1,NSG
		BUF(K)=TEMPH
	END DO
C     Actually assign temperature to bottom NHT rim rings
	DO J=NSTP+1,NRM
		WRITE (NOUT,102) (BUF(K),K=1,NSG)
	END DO
	STOP 'GENRMDAT Completed successfully.'
	END PROGRAM GENRMDAT
