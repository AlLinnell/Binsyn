      PROGRAM DATCONV
	USE MSFLIB
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Program to read output file from ACPGF6 and produce files for
C     plotting
C     Input file is first output file from ACPGF6
C     First output file is combined spectrum
C     Second output file is first stellar component
C     Third output file is second stellar component
C     Fourth output file is face of accretion disk
C     Fifth output file is rim of accretion disk
C     Sixth output file is scale factor for other output files
C*********************************************************************
  100 FORMAT (F11.6,5E13.6)
  101 FORMAT (F15.6,E15.6)
  102 FORMAT (A36)
  103 FORMAT (I2)
  105 FORMAT (1X,'Scale factor, ACPGF6,=',E15.6)
  106 FORMAT (10I5)
      DIMENSION FL1(10),WL(100000),FLX1(100000),FLX2(100000),
     $FLX3(100000),FLX4(100000),FLX5(100000)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 36 FL1
      CHARACTER * 7 LABEL
	INTEGER(4) RESULT
	INTEGER * 2 KEYGOOD, KEYBAD
      DATA IN1,NOUT1,NOUT2,NOUT3,NOUT4,NOUT5,NOUT6/1,2,3,4,5,6,7/
      DATA IN2,IN3,IN4/8,9,10/
	DATA VELC/2.997925D10/
	DATA KEYGOOD/0/
	DATA KEYBAD/-1/
C*********************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN (UNIT=11,FILE='C:\LGTSYN\DATCONV\DATCONV.ILS',STATUS='OLD')
      READ (11,102) (FL1(J),J=1,10)
      OPEN (UNIT=IN1,   FILE=FL1(1),STATUS='OLD')
      OPEN (UNIT=NOUT1, FILE=FL1(2),STATUS='REPLACE')
      OPEN (UNIT=NOUT2, FILE=FL1(3),STATUS='REPLACE')
      OPEN (UNIT=NOUT3, FILE=FL1(4),STATUS='REPLACE')
      OPEN (UNIT=NOUT4, FILE=FL1(5),STATUS='REPLACE')
      OPEN (UNIT=NOUT5, FILE=FL1(6),STATUS='REPLACE')
      OPEN (UNIT=NOUT6, FILE=FL1(7),STATUS='REPLACE')
      OPEN (UNIT=IN2,   FILE=FL1(8),STATUS='OLD')
      OPEN (UNIT=IN3,   FILE=FL1(9),STATUS='OLD')
	OPEN (UNIT=IN4,   FILE=FL1(10),STATUS='OLD')
	WRITE (IN4,103) KEYBAD
	CLOSE (UNIT=IN4, STATUS = 'KEEP')
      READ (IN2,106) KCTRL,NPSI
      LBLERR=0
      LABEL='DATCONV'
      NU=IN3
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD (IORD)
	CALL DUMMY (1)
	CALL IREAD (KEY)
	CALL DUMMY (1)
	CALL IREAD (ISW)
      IF (IORD.LT.1) CALL PMDSTOP
      IF (IORD.EQ.1) GO TO 11
      DO KK=1,IORD-1
         DO J=1,100000
            READ (IN1,100,END=4,ERR=2) Z1,Z2,Z3,Z4,Z5,Z6
         IF (Z1.EQ.0.0D0) GO TO 10
         END DO
         CALL PMDSTOP
   10 END DO
   11 CONTINUE
      FLMX=1.0D-7
      DO J=1,100000
         READ (IN1,100,END=4,ERR=2) WL(J),FLX1(J),FLX2(J),FLX3(J),
     $   FLX4(J),FLX5(J)
         IF (WL(J).EQ.0.0D0) GO TO 1
         IF (FLX1(J).GT.FLMX) FLMX=FLX1(J)
      END DO
    1 PW10=DLOG10(FLMX)
      I10=PW10
      PW10=I10
      FLMX=DEXP(PW10*2.30258509299D0)
      JLM=J-1
	IF (KEY.NE.1) FLMX=1.D0
      DO J=1,JLM
		VWL=WL(J)/1.D8
		VWLSQ=VWL*VWL
		CF=VELC/VWLSQ/FLMX/1.D8
		IF (ISW.NE.0) CF=1.D0
         WRITE (NOUT1,101) WL(J),FLX1(J)*CF
         WRITE (NOUT2,101) WL(J),FLX4(J)*CF
         WRITE (NOUT3,101) WL(J),FLX5(J)*CF
         WRITE (NOUT4,101) WL(J),FLX2(J)*CF
         WRITE (NOUT5,101) WL(J),FLX3(J)*CF
      END DO
    3 WRITE (NOUT6,105) FLMX
      CLOSE (UNIT=IN1, STATUS='KEEP')
      CLOSE (UNIT=IN2, STATUS='KEEP')
      CLOSE (UNIT=IN3, STATUS='KEEP')
      CLOSE (UNIT=NOUT1, STATUS='KEEP')
      CLOSE (UNIT=NOUT2, STATUS='KEEP')
      CLOSE (UNIT=NOUT3, STATUS='KEEP')
      CLOSE (UNIT=NOUT4, STATUS='KEEP')
      CLOSE (UNIT=NOUT5, STATUS='KEEP')
      CLOSE (UNIT=NOUT6, STATUS='KEEP')
	OPEN (UNIT=IN4, FILE=FL1(10), STATUS = 'OLD')
	WRITE (IN4,103) KEYGOOD
	CLOSE (UNIT=IN4, STATUS = 'KEEP')
      STOP
    2 CALL PMDSTOP
    4 CALL PMDSTOP
      END
