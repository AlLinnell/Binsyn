      PROGRAM SETWT
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER  FMT*80
      CHARACTER * 7 LABEL
      CHARACTER * 80 BFR
      CHARACTER * 23 HD1
      CHARACTER * 23 HD2
      CHARACTER * 23 HD3
      CHARACTER * 36 FL1
      DIMENSION SG(90),RFMG(90),
     $NCT(90)
      DIMENSION FL1(7)
      DIMENSION VLT(5,150000)
      DIMENSION PHS(5,150000)
      DIMENSION WT(5,150000)
      COMMON /PI/PI,PI2,PIH,PI3H
      COMMON /NUNIT/ NU, NUOUT, IFOUT
  100 FORMAT (2E20.7,F5.1)
  101 FORMAT (2E20.7)
  102 FORMAT (5F10.5)
  103 FORMAT (A80)
  104 FORMAT (1H1)
  105 FORMAT (A23)
  106 FORMAT (A36)
      DATA PI/3.1415926535898/
      DATA PI2/6.2831853071796/
      DATA PIH/1.5707963267949/
      DATA PI3H/4.7123889803847/
      DATA ZR1,ZR2/0.D0,0.D0/
      DATA WTT/1.0/
      HD1= ' PGS OUTPUT,LIGHT UNITS'
      HD2= ' PGS OUTPUT,MAGNITUDES '
      HD3= ' PGS OUTPUT,RESIDUALS  '
C*********************************************************
      OPEN ( UNIT=8, FILE='SETWT.ILS',  STATUS= 'OLD')
      READ ( 8, 106) (FL1(J),J=1,7)
      OPEN ( UNIT=1, FILE= FL1(1), STATUS= 'OLD')
      OPEN ( UNIT=2, FILE= FL1(2), STATUS= 'OLD')
      OPEN ( UNIT=3, FILE= FL1(3), STATUS= 'OLD')
      OPEN ( UNIT=4, FILE= FL1(4), STATUS= 'OLD')
      OPEN ( UNIT=5, FILE= FL1(5), STATUS= 'REPLACE')
      OPEN ( UNIT=66, FILE= FL1(6), STATUS= 'REPLACE')
      OPEN ( UNIT=7, FILE= FL1(7), STATUS= 'REPLACE')
C*********************************************************
C     Read input control data
      LBLERR=0
      LABEL = ' *SETWT'
      NU = 1
      CALL GTLBL ( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL IREAD ( NCOL)
      CALL DUMMY (2)
      CALL CREAD (FMT)
      CALL DUMMY(2)
      CALL DREAD (Pphase1,'F8.3')
      CALL DREAD (Pphase2,'F8.3')
      CALL DREAD (Pphase3,'F8.3')
      CALL DREAD (Pphase4,'F8.3')
      CALL DUMMY(1)
      CALL DREAD (Sphase1,'F8.3')
      CALL DREAD (Sphase2,'F8.3')
      CALL DREAD (Sphase3,'F8.3')
      CALL DREAD (Sphase4,'F8.3')
      CALL DUMMY(1)
      CALL DREAD (PWT12,'F8.3')
      CALL DREAD (PWT23,'F8.3')
      CALL DREAD (PWT34,'F8.3')
      CALL DUMMY(1)
      CALL DREAD (SWT12,'F8.3')
      CALL DREAD (SWT23,'F8.3')
      CALL DREAD (SWT34,'F8.3')
      CALL DUMMY(1)
      CALL DREAD (OECWT,'F8.3')
C     First file
      READ (2,103,END=92,ERR=93) BFR
      DO 7 IWL=1,NCOL
      DO 5 I=1,150000
      READ (2,FMT,END=94,ERR=93) VLTA,PHSA,WTA
      VLT(IWL,I)=VLTA
      PHS(IWL,I)=PHSA
      WT(IWL,I)=WTA
      IF ((VLTA.EQ.0.D0).AND.(PHSA.EQ.0.D0)) GO TO 8
    5 CONTINUE
    9 CALL PMDSTOP
    8 NCT(IWL)=I-1
    7 CONTINUE
      WRITE (5,103) BFR
      DO 10 IWL=1,NCOL
      DO 11 I=1,NCT(IWL)
      VLTA=VLT(IWL,I)
      PHSA=PHS(IWL,I)
C     Assume an out-of-eclipse phase
      WTA=OECWT
      IF ((PHSA.GT.Pphase1).AND.(PHSA.LT.Pphase2)) THEN
      	WTA=PWT12
      END IF
      IF ((PHSA.GE.Pphase2).AND.(PHSA.LE.Pphase3)) THEN
      	WTA=PWT23
      END IF
      IF ((PHSA.GT.Pphase3).AND.(PHSA.LT.Pphase4)) THEN
      	WTA=PWT34
      END IF
      IF ((PHSA.GT.Sphase1).AND.(PHSA.LT.Sphase2)) THEN
      	WTA=SWT12
      END IF
      IF ((PHSA.GE.Sphase2).AND.(PHSA.LE.Sphase3)) THEN
      	WTA=SWT23
      END IF
      IF ((PHSA.GT.Sphase3).AND.(PHSA.LT.Sphase4)) THEN
      	WTA=SWT34
      END IF
      WRITE (5,FMT) VLTA,PHSA,WTA
   11 CONTINUE
      WRITE (5,101) ZR1,ZR2
   10 CONTINUE
C     Second file
      READ (3,103,END=92,ERR=93) BFR
      DO 17 IWL=1,NCOL
      DO 15 I=1,150000
      READ (3,FMT,END=94,ERR=93) VLTA,PHSA,WTA
      VLT(IWL,I)=VLTA
      PHS(IWL,I)=PHSA
      WT(IWL,I)=WTA
      IF ((VLTA.EQ.0.D0).AND.(PHSA.EQ.0.D0)) GO TO 18
   15 CONTINUE
   19 CALL PMDSTOP
   18 NCT(IWL)=I-1
   17 CONTINUE
      WRITE (66,103) BFR
      DO 20 IWL=1,NCOL
      DO 21 I=1,NCT(IWL)
      VLTA=VLT(IWL,I)
      PHSA=PHS(IWL,I)
C     Assume an out-of-eclipse phase
      WTA=OECWT
      IF ((PHSA.GT.Pphase1).AND.(PHSA.LT.Pphase2)) THEN
      	WTA=PWT12
      END IF
      IF ((PHSA.GE.Pphase2).AND.(PHSA.LE.Pphase3)) THEN
      	WTA=PWT23
      END IF
      IF ((PHSA.GT.Pphase3).AND.(PHSA.LT.Pphase4)) THEN
      	WTA=PWT34
      END IF
      IF ((PHSA.GT.Sphase1).AND.(PHSA.LT.Sphase2)) THEN
      	WTA=SWT12
      END IF
      IF ((PHSA.GE.Sphase2).AND.(PHSA.LE.Sphase3)) THEN
      	WTA=SWT23
      END IF
      IF ((PHSA.GT.Sphase3).AND.(PHSA.LT.Sphase4)) THEN
      	WTA=SWT34
      END IF
      WRITE (66,FMT) VLTA,PHSA,WTA
   21 CONTINUE
      WRITE (66,101) ZR1,ZR2
   20 CONTINUE
C     Third file
      READ (4,103,END=92,ERR=93) BFR
      DO 27 IWL=1,NCOL
      DO 25 I=1,150000
      READ (4,FMT,END=94,ERR=93) VLTA,PHSA,WTA
      VLT(IWL,I)=VLTA
      PHS(IWL,I)=PHSA
      WT(IWL,I)=WTA
      IF ((VLTA.EQ.0.D0).AND.(PHSA.EQ.0.D0)) GO TO 28
   25 CONTINUE
   29 CALL PMDSTOP
   28 NCT(IWL)=I-1
   27 CONTINUE
      WRITE (7,103) BFR
      DO 30 IWL=1,NCOL
      DO 31 I=1,NCT(IWL)
      VLTA=VLT(IWL,I)
      PHSA=PHS(IWL,I)
C     Assume an out-of-eclipse phase
      WTA=OECWT
      IF ((PHSA.GT.Pphase1).AND.(PHSA.LT.Pphase2)) THEN
      	WTA=PWT12
      END IF
      IF ((PHSA.GE.Pphase2).AND.(PHSA.LE.Pphase3)) THEN
      	WTA=PWT23
      END IF
      IF ((PHSA.GT.Pphase3).AND.(PHSA.LT.Pphase4)) THEN
      	WTA=PWT34
      END IF
      IF ((PHSA.GT.Sphase1).AND.(PHSA.LT.Sphase2)) THEN
      	WTA=SWT12
      END IF
      IF ((PHSA.GE.Sphase2).AND.(PHSA.LE.Sphase3)) THEN
      	WTA=SWT23
      END IF
      IF ((PHSA.GT.Sphase3).AND.(PHSA.LT.Sphase4)) THEN
      	WTA=SWT34
      END IF
      WRITE (7,FMT) VLTA,PHSA,WTA
   31 CONTINUE
      WRITE (7,101) ZR1,ZR2
   30 CONTINUE
      STOP 'SETWT Completed successfully'
  92  CALL PMDSTOP
  93  CALL PMDSTOP
  94  CALL PMDSTOP
      END PROGRAM SETWT
