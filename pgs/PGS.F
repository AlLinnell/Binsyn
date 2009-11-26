      PROGRAM PGS
	USE MSFLIB
	USE PORTLIB
C     Program to accept input data,modulate it with simulated
C     observational error,and produce three output files.
C     One file contains data in magnitudes,one in light units,and
C     the third has residuals from the input data file.
      INCLUDE 'COMA.FOR'
      CHARACTER  FMT*80
      CHARACTER * 7 LABEL
      CHARACTER * 1 BFR
      CHARACTER * 23 HD1
      CHARACTER * 23 HD2
      CHARACTER * 23 HD3
      CHARACTER * 36 FL1
      DIMENSION SG(90),RFMG(90),
     $NCT(90),FL1(5)
      COMMON /RANDOM/IRANDOM
      COMMON /PI/PI,PI2,PIH,PI3H
      COMMON /NUNIT/ NU, NUOUT, IFOUT
	INTEGER(4) RESULT,IRANDOM
	INTEGER * 2 IHR,IMIN,ISEC,I100TH
  100 FORMAT (2E20.7)
  101 FORMAT (2I5)
  102 FORMAT (5F10.5)
  103 FORMAT (1A1)
  104 FORMAT (1H1)
  105 FORMAT (A23)
  106 FORMAT (A36)
      DATA PI/3.1415926535898D0/
      DATA PI2/6.2831853071796D0/
      DATA PIH/1.5707963267949D0/
      DATA PI3H/4.7123889803847D0/
      DATA ZR1,ZR2/0.D0,0.D0/
	DATA WT/1.D0/
      CALL GETTIM(IHR,IMIN,ISEC,I100TH)
      IRANDOM=-I100TH
      HD1= ' PGS OUTPUT,LIGHT UNITS'
      HD2= ' PGS OUTPUT,MAGNITUDES '
      HD3= ' PGS OUTPUT,RESIDUALS  '
C
C
C*********************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT=6, FILE='C:\LGTSYN\PGS\PGS.ILS',  STATUS= 'OLD')
      READ ( 6, 106) (FL1(J),J=1,5)
      OPEN ( UNIT=1, FILE= FL1(1), STATUS= 'OLD')
      OPEN ( UNIT=2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT=3, FILE= FL1(3), STATUS= 'REPLACE')
      OPEN ( UNIT=4, FILE= FL1(4), STATUS= 'REPLACE')
      OPEN ( UNIT=5, FILE= FL1(5), STATUS= 'REPLACE')
C*********************************************************
      WRITE (3,105) HD1
      WRITE (4,105) HD2
      WRITE (5,105) HD3
C     Read input control data
      LBLERR=0
      LABEL = ' ***PGS'
      NU = 1
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD ( NCOL)
      CALL DUMMY (2)
      CALL DREAD ( ELIGHT, 'F6.3')
      CALL DUMMY (2)
      CALL DREAD ( SG(1), 'F6.3')
      IF (NCOL.LE.1) GO TO 1
      DO K=2,NCOL
      CALL DREAD ( SG(K), 'F6.3')
      END DO
    1 CALL DUMMY (2)
      CALL DREAD ( RFMG(1), 'F6.3')
      IF (NCOL.LE.1) GO TO 2
      DO K=2,NCOL
      CALL DREAD ( RFMG(K), 'F6.3')
      END DO
    2 CALL DUMMY (2)
      CALL CREAD (FMT)
C*******************************************************
C     Read light curve to be modulated,light units
C     Read initial record.Label only.
      READ (2,103,END=92,ERR=93) BFR
      DO 7 IWL=1,NCOL
      DO 5 I=1,150000
      READ (2,FMT,END=6,ERR=93) VLT,PHS,WTA
      IF ((VLT.EQ.0.D0).AND.(PHS.EQ.0.D0)) GO TO 8
    5 CONTINUE
    9 CALL PMDSTOP
    8 NCT(IWL)=I-1
    7 CONTINUE
    6 REWIND 2
      READ (2,103,END=9,ERR=93) BFR
      DO 20 JCOL=1,NCOL
      NPT=NCT(JCOL)
      DO 10 I=1,NPT
      READ (2,FMT,END=94,ERR=93) VLT,PHS
      VLV  =  RNDFLUX ( SG(JCOL), VLT, ELIGHT )
      VMV  =  RFMG(JCOL) - 2.5D0 * DLOG10(VLV)
      WRITE ( 3,FMT )  VLV,  PHS,  WT
      WRITE ( 4,FMT )  VMV,  PHS,  WT
      WRITE ( 5,FMT )  VLV-VLT,  PHS,  WT
   10 CONTINUE
      READ (2,FMT,END=94,ERR=93) VLT,PHS
      IF ((VLT.NE.0.D0).OR.(PHS.NE.0.D0)) CALL PMDSTOP
      WRITE ( 3,FMT) ZR1,ZR2
      WRITE ( 4,FMT) ZR1,ZR2
      WRITE ( 5,FMT) ZR1,ZR2
   20 CONTINUE
      CLOSE ( UNIT=1, STATUS= 'KEEP')
      CLOSE ( UNIT=2, STATUS= 'KEEP')
      CLOSE ( UNIT=3, STATUS= 'KEEP')
      CLOSE ( UNIT=4, STATUS= 'KEEP')
      CLOSE ( UNIT=5, STATUS= 'KEEP')
      STOP
   92 CALL PMDSTOP
   93 CALL PMDSTOP
   94 CALL PMDSTOP
      END PROGRAM PGS
