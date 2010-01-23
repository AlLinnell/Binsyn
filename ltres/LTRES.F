      PROGRAM LTRES
C     PROGRAM TO PLOT RESIDUALS FROM LIGHT CURVES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (MOBS    =  10000,
     $           MFV     =      5)
      DIMENSION PHS(MOBS),VLT(MOBS),PHSA(MOBS),VLTA(MOBS)
      DIMENSION NCT1(MFV)
      DIMENSION FL1(9)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
      CHARACTER * 80 BFR, FMT
      CHARACTER * 36 FL1
      CHARACTER * 10 DATE,TIME,ZONE
      INTEGER * 2 IYR,IMON,IDAY,IHR,IMIN,ISEC,I100TH
      DIMENSION TIMEVALUES(8)
      INTEGER :: CLOCK1,CLOCK2,COUNT_RATE,COUNT_MAX,TIMEVALUES
      DATA IN1,IN2,IN3,NOUT1,NOUT3,NOUT4,NOUT5/1,2,3,4,5,6,7/
      DATA NOUT6,NOUT7/8,9/
C
  100 FORMAT (1H1,/,20X,'PROGRAM LTRES')
  101 FORMAT (2E20.7)   
  102 FORMAT (1X,'DATA INPUT ERROR')  
  103 FORMAT (8F10.6)   
  104 FORMAT (5I5)      
  105 FORMAT (1X,'END OF LTRES')
  112 FORMAT (A80)
  116 FORMAT (A20)
  119 FORMAT (A36)
  204 FORMAT (1X,'PROGRAM CALPT',26X,'DATE=',I2,':',I2,':',I4,2X,
     $'TIME=',I2,':',I2,':',I2,/,13X,'**OUT**',/,1X,'*')
  205 FORMAT (1X,'Program running time=',F12.3,2X,'seconds')
C
C*************************************************************************
      OPEN ( UNIT=  10, FILE= 'LTRES.ILS', 
     $STATUS ='OLD')
      READ ( 10, 119) (FL1(J),J=1,9)
      OPEN ( UNIT= IN1,   FILE=   FL1(1), STATUS= 'OLD')
      OPEN ( UNIT= IN2,   FILE=   FL1(2), STATUS= 'OLD')
      OPEN ( UNIT= IN3,   FILE=   FL1(3), STATUS= 'OLD')
      OPEN ( UNIT= NOUT1, FILE=   FL1(4), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT3, FILE=   FL1(5), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT4, FILE=   FL1(6), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT5, FILE=   FL1(7), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT6, FILE=   FL1(8), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT7, FILE=   FL1(9), STATUS= 'REPLACE')
C**************************************************************************
      LBLERR=0
      LABEL= '**LTCVA'
      NU=1
      CALL GTLBL( LABEL, LBLERR)
      IF (LBLERR.EQ.(-1)) CALL PMDSTOP
      CALL CREAD ( OBJ)
      CALL DUMMY (1)
      CALL DREAD ( TMN, 'F9.5')
      CALL DUMMY (1)
      CALL DREAD ( PER, 'F13.9')
      CALL DUMMY (1)
      CALL DREAD ( PHQD, 'F9.5')
      CALL DUMMY (1)
      CALL IREAD ( KCTRL)
      CALL DUMMY (1)
      CALL CREAD ( FMT)
      CALL DUMMY (1)
      CALL DREAD ( PL1, 'F7.2')
      CALL DUMMY (2)
      CALL DREAD ( PL2, 'F6.2')
      CALL DUMMY (2)
      READ (IN3,104,END=98,ERR=98) NCOL
      CALL SYSTEM_CLOCK(CLOCK1)
      CALL DATE_AND_TIME(DATE,TIME,ZONE,TIMEVALUES)
      IYR=TIMEVALUES(1)
      IMON=TIMEVALUES(2)
      IDAY=TIMEVALUES(3)
      IHR=TIMEVALUES(5)
      IMIN=TIMEVALUES(6)
      ISEC=TIMEVALUES(7)
      WRITE (NOUT1,100) 
      WRITE (NOUT1,204) IMON,IDAY,IYR,IHR,IMIN,ISEC
C*******************************************************************
C
      NPSI=0     
      NPT=0      
C     INPUT LIGHT RESIDUALS 
      READ (IN2,112) BFR
      DO 36 ICOL=1,NCOL
      NCT1(ICOL)=0
      DO 40 I=1,8000  
      READ (IN2,101,END=98,ERR=98) VLT(1),PHS(1)      
      IF (I.EQ.1) GO TO 40
      IF ((VLT(1).EQ.0.0D0).AND.(PHS(1)).EQ.0.0D0) GO TO 35
   40 CONTINUE   
      CALL PMDSTOP      
   35 NCT1(ICOL)=I-1
   36 CONTINUE
      REWIND IN2   
C************************************
      READ (IN2,112) BFR
      DO 30 ICOL=1,NCOL
      NPSI=NCT1(ICOL)
      DO I=1,NPSI
      READ (IN2,101,END=98,ERR=98) VLT(I),PHS(I)
      END DO
C     Read file separator
      READ (IN2,101) VM,PH
      IF ((VM.NE.0.0).OR.(PH.NE.0.0)) CALL PMDSTOP
C     Select points that fall within phase window
      N=NPSI
      J=0
      DO 12 I=1,N
      IF (PHS(I).GT.PHQD) PHS(I)=PHS(I)-1.D0
      IF ((PHS(I).LT.PL1).OR.(PHS(I).GT.PL2)) GO TO 12
      J=J+1
      PHSA(J)=PHS(I)
      VLTA(J)=VLT(I)
   12 CONTINUE
      LM=J-1     
C     SORT DATA INTO ASCENDING SEQUENCE
      DO 17 I=1,J
      DO 16 K=1,LM      
      IF (PHSA(K).LE.PHSA(K+1)) GO TO 16     
      SV=PHSA(K) 
      PHSA(K)=PHSA(K+1) 
      PHSA(K+1)=SV      
      SV=VLTA(K) 
      VLTA(K)=VLTA(K+1) 
      VLTA(K+1)=SV      
   16 CONTINUE   
   17 CONTINUE   
C     Write output   
      DO I=1,N
      IF (ICOL.EQ.1) WRITE (NOUT3,101) VLTA(I),PHSA(I)
      IF (ICOL.EQ.2) WRITE (NOUT4,101) VLTA(I),PHSA(I)
      IF (ICOL.EQ.3) WRITE (NOUT5,101) VLTA(I),PHSA(I)
      IF (ICOL.EQ.4) WRITE (NOUT6,101) VLTA(I),PHSA(I)
      IF (ICOL.EQ.5) WRITE (NOUT6,101) VLTA(I),PHSA(I)
      END DO
   30 CONTINUE
C*********************************************************************
      CLOSE ( UNIT=   IN1, STATUS= 'KEEP')
      CLOSE ( UNIT=   IN2, STATUS= 'KEEP')
      CLOSE ( UNIT=   IN3, STATUS= 'KEEP')
      CLOSE ( UNIT= NOUT1, STATUS= 'KEEP')
      CLOSE ( UNIT= NOUT3, STATUS= 'KEEP')
      IF (NCOL.GT.1) THEN
         CLOSE ( UNIT= NOUT4, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT4, STATUS = 'DELETE')
      END IF
      IF (NCOL.GT.2) THEN
         CLOSE ( UNIT= NOUT5, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT5, STATUS= 'DELETE')
      END IF
      IF (NCOL.GT.3) THEN
         CLOSE ( UNIT= NOUT6, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT6, STATUS= 'DELETE')
      END IF
      IF (NCOL.GT.4) THEN
         CLOSE ( UNIT= NOUT7, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT7, STATUS= 'DELETE')
      END IF
      STOP
   98 WRITE (2,102) 
      CALL PMDSTOP
      END  PROGRAM LTRES
