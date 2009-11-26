      PROGRAM LTCVA
	USE MSFLIB
C     PROGRAM TO WRITE PLOT FILES FOR LIGHT CURVES
C     THE FIRST SET OF POINTS ARE LIGHT RATIOS      
C     THE SECOND SET OF POINTS ARE MAGNITUDES
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (MOBS    =  10000,
     $           MFV     =      5)
      DIMENSION PHS(MOBS),VLT(MOBS),PHSA(MOBS),VLTA(MOBS)
	DIMENSION PHSB(MOBS),VLTB(MOBS)  
      DIMENSION RFMG(MFV),DPHS(MFV)
      DIMENSION NCT1(MFV),NCT2(MFV)
      DIMENSION FL1(17)
      COMMON /NUNIT/ NU, NUOUT, IFOUT
      CHARACTER * 7 LABEL
      CHARACTER * 20 OBJ
      CHARACTER * 80 BFR,FMT
      CHARACTER * 48 FL1
	INTEGER(4) RESULT
      DATA IN1,IN2,IN3,IN4,IN5/1,2,3,4,5/
      DATA NOUT1/6/
      DATA NOUT3,NOUT4,NOUT5,NOUT6,NOUT7,NOUT8/7,8,9,10,11,12/
      DATA NOUT9,NOUT10,NOUT11,NOUT12/13,14,15,16/
  100 FORMAT (1H1,/,20X,'PROGRAM LTCVA')
  101 FORMAT (2E20.7)   
  102 FORMAT (1X,'DATA INPUT ERROR')  
  104 FORMAT (5I5)      
  105 FORMAT (1X,'END OF LTCVA')      
  112 FORMAT (A80)
  113 FORMAT (8F10.5)
  119 FORMAT (A48)
C     FIRST SET OF POINTS REPRESENT REFERENCE CURVE
C     SECOND SET OF POINTS REPRESENT OBSERVATIONAL DATA
C     TMN=TIME OF MIDPRIMARY MINIMUM.IF INPUT DATA ARE IN PHASE UNITS,
C        TMN=0.0
C     PER=ORBITAL PERIOD.IF INPUT DATA ARE IN PHASE UNITS,PER=1.0  
C
C
C*************************************************************************
      RESULT=SETEXITQQ(QWIN$EXITNOPERSIST)
      OPEN ( UNIT= 18, FILE = 'C:\LGTSYN\LTCVA\LTCVA.ILS',
     $STATUS ='OLD')
      READ ( 18, 119)   (FL1(J),J=1,17)
      OPEN ( UNIT=  IN1,   FILE=   FL1(1), STATUS= 'OLD')
      OPEN ( UNIT=  IN2,   FILE=   FL1(2), STATUS= 'OLD')
      OPEN ( UNIT=  IN3,   FILE=   FL1(3), STATUS= 'OLD')
      OPEN ( UNIT=  IN4,   FILE=   FL1(4), STATUS= 'OLD')
      OPEN ( UNIT=  IN5,   FILE=   FL1(5), STATUS= 'OLD')
      OPEN ( UNIT=  NOUT1, FILE=   FL1(6), STATUS= 'REPLACE')
	OPEN ( UNIT=  NOUT2, FILE=   FL1(7), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT3, FILE=   FL1(8), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT4, FILE=   FL1(9), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT5, FILE=  FL1(10), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT6, FILE=  FL1(11), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT7, FILE=  FL1(12), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT8, FILE=  FL1(13), STATUS= 'REPLACE')
      OPEN ( UNIT=  NOUT9, FILE=  FL1(14), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT10, FILE=  FL1(15), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT11, FILE=  FL1(16), STATUS= 'REPLACE')
      OPEN ( UNIT= NOUT12, FILE=  FL1(17), STATUS= 'REPLACE')
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
      CALL DUMMY(1)
      CALL CREAD ( FMT)
      CALL DUMMY(1)
      CALL DREAD ( PL1, 'F7.2')
      CALL DUMMY (2)
      CALL DREAD ( PL2, 'F6.2')
      CALL DUMMY (2)
	CALL IREAD (KEY)
C
      READ (IN4,104,END=98,ERR=98) NCOL
      READ (IN4,113,END=98,ERR=98) (RFMG(K),K=1,NCOL)
      READ (IN5,104,END=98,ERR=98) KCOL
      IF (KCOL.NE.NCOL) CALL PMDSTOP
      READ (IN5,113,END=98,ERR=98) (DPHS(K),K=1,NCOL)
      IF (NCOL.GT.5) CALL PMDSTOP
      WRITE (NOUT1,100)    
C*******************************************************************
C
      NPSI=0     
      NPT=0      
C     INPUT THEORETICAL LIGHT CURVE, COUNT NUMBER OF OBSERVATIONS
      READ (IN2,112) BFR
      DO 6 JCOL=1,NCOL
      NCT1(JCOL)=0
      DO 40 I=1,20000  
      READ (IN2,101,END=98,ERR=98) PHS(I),VLT(I)
      IF (I.EQ.1) GO TO 40
C     Test for separator record between colors
      IF ((VLT(I).EQ.0.0D0).AND.(PHS(I)).EQ.0.0D0) GO TO 35
   40 CONTINUE   
      CALL PMDSTOP      
   35 NCT1(JCOL)=I-1
    6 CONTINUE
      REWIND IN2
C************************************
C     INPUT OBSERVED LIGHT CURVE, COUNT NUMBER OF OBSERVATIONS
C     THIS SECTION FOR INPUT AS VM,UT 
C     Read initial BCD record (identification)
   13 READ (IN3,112) BFR
      DO 38 JCOL=1,NCOL
      NCT2(JCOL)=0
      DO 42 I=1,20000
      IF (KCTRL.EQ.0) THEN
         READ (IN3,FMT,END=98,ERR=98) VM,UT
      ELSE
         READ (IN3,FMT,END=98,ERR=98) UT,VM
      END IF
C     Test for separator record between colors
      IF ((VM.EQ.0.0D0).AND.(UT.EQ.0.0D0)) GO TO 27
   42 CONTINUE   
      CALL PMDSTOP      
   27 NCT2(JCOL)=I-1
   38 CONTINUE
      REWIND IN3
C     Position file at start of observational data	  
      READ (IN3,112) BFR
C*******************************************
C
C
C
C
C     PRINCIPAL LONG LOOP
C*********************************************
C     Read header for theoretical light curve
      READ (IN2,112) BFR
      DO 30 JCOL=1,NCOL
C 
C  
C  
C     INPUT IN THIS SECTION IS IN LIGHT UNITS
C     INPUT THEORETICAL LIGHT CURVE 
C  
C  
      NPSI=NCT1(JCOL)
      DO 10 I=1,NPSI    
      READ (IN2,101,END=98,ERR=98) PHS(I),VLT(I)
C     Test for phase values that need to be reset
      IF ((PHS(I).LE.PL1).AND.(PL1.EQ.0.D0)) THEN
		IF ((I.LT.NPSI).AND.(I.GT.1)) PHS(I)=PHS(I)+1.D0
	END IF
	IF ((I.EQ.NPSI).AND.(PHS(I).LE.1.E-12)) PHS(I)=1.D0	     
   10 CONTINUE   
C     Read file separator
      READ (IN2,101,END=98,ERR=98) ZRA,ZRB
C     Must end with color separator
      IF ((ZRA.NE.0.D0).OR.(ZRB.NE.0.D0)) CALL PMDSTOP
      N=NPSI     
      J=0 
C     SORT DATA INTO ASCENDING SEQUENCE
      DO 17 I=1,J
      DO 16 K=1,LIM      
      IF (PHS(K).LE.PHS(K+1)) GO TO 16     
      SV=PHS(K) 
      PHS(K)=PHS(K+1) 
      PHS(K+1)=SV      
      SV=VLT(K) 
      VLT(K)=VLT(K+1) 
      VLT(K+1)=SV      
   16 CONTINUE   
   17 CONTINUE
      DO 12 I=1,N
      IF (PHS(I).GT.PHQD) PHS(I)=PHS(I)-1.D0   
      IF ((PHS(I).LT.PL1).OR.(PHS(I).GT.PL2))
     $ GO TO 12
      J=J+1      
      PHSA(J)=PHS(I)    
      VLTA(J)=VLT(I)
   12 CONTINUE
C     Repeat data for an additional cycle
C     Note that the loop starts with J set at the end of the
C     data file
      LIM=J-1
      DO I=1,LIM
		J=J+1
		PHSA(J)=PHSA(J-LIM)+1.D0
		VLTA(J)=VLTA(J-LIM)
	END DO
C     Repeat data for an additional cycle
C     Note that the loop starts with J set at the end of the
C     data file
      LIM=J-1
	DO I=1,LIM
		J=J+1
		PHSA(J)=PHSA(J-LIM)+2.D0
		VLTA(J)=VLTA(J-LIM)
	END DO   
C     Repeat data for an additional cycle
C     Note that the loop starts with J set at the end of the
C     data file
      LIM=J-1
	DO I=1,LIM
		J=J+1
		PHSA(J)=PHSA(J-LIM)+3.D0
		VLTA(J)=VLTA(J-LIM)
	END DO   
C
C     Restrict output data to limits PL1 and PL2
      LIM=J
	J=0
	DO I=1,LIM
		IF ((PHSA(I).GE.PL1).AND.(PHSA(I).LE.PL2))THEN
			J=J+1
			PHS(J)=PHSA(I)
			VLT(J)=VLTA(J)
		END IF
	END DO
	LIM=J     
C      
C  
C     INPUT SECOND SET OF POINTS
C     INPUT IN FOLLOWING SECTION IS IN MAGNITUDES, DEPENDING ON
C     VALUE OF KEY   
C  
C  
      NPT=NCT2(JCOL)
      DO 26 IC=1,NPT
      IF (KCTRL.EQ.0) THEN
         READ (IN3,FMT,END=99) VM,PH
      ELSE
         READ (IN3,FMT,END=98) PH,VM
      END IF
      IF (PH.GT.PHQD) PH=PH-1.D0
c      IF ((PH.LT.PL1).OR.(PH.GT.PL2)) GO TO 26
      IF (KEY.EQ.0) THEN
		VL=DEXP((VM-RFMG(JCOL))/(-1.085736205D0))  
		PHSB(IC)=PH
		VLTB(IC)=VL
	ELSE
		VLTB(IC)=VM
		PHSB(IC)=PH
	END IF
   26 CONTINUE   
C     Read file separator
      READ (IN3,FMT) VM,PH
      IF ((VM.NE.0.0D0).OR.(PH.NE.0.0D0)) CALL PMDSTOP
C     Sort into ascending sequence   
      LM=NPT-1
      DO 37 I=1,NPT
      DO 36 K=1,LM      
      IF (PHSB(K).LE.PHSB(K+1)) GO TO 36     
      SV=PHSB(K) 
      PHSB(K)=PHSB(K+1) 
      PHSB(K+1)=SV      
      SV=VLTB(K) 
      VLTB(K)=VLTB(K+1) 
      VLTB(K+1)=SV      
   36 CONTINUE   
   37 CONTINUE 
C     Write light curve to output file
      DO I=1,NPT
           IF (JCOL.EQ.1) THEN
			WRITE (NOUT3,101) VLTB(I),PHSB(I)-DPHS(JCOL)
           END IF
           IF (JCOL.EQ.2) THEN
			WRITE (NOUT5,101) VLTB(I),PHSB(I)-DPHS(JCOL)
           END IF
           IF (JCOL.EQ.3) THEN
			WRITE (NOUT7,101) VLTB(I),PHSB(I)-DPHS(JCOL)
           END IF
           IF (JCOL.EQ.4) THEN
			WRITE (NOUT9,101)  VLTB(I),PHSB(I)-DPHS(JCOL)
           END IF
           IF (JCOL.EQ.5) THEN
			WRITE (NOUT11,101) VLTB(I),PHSB(I)-DPHS(JCOL)
           END IF
      END DO
	DO I=1,LIM
		IF (JCOL.EQ.1) THEN
			WRITE (NOUT4,101) VLT(I),PHS(I)
		END IF
		IF (JCOL.EQ.2) THEN
			WRITE (NOUT6,101) VLT(I),PHS(I)
		END IF
		IF (JCOL.EQ.3) THEN 
			 WRITE (NOUT8,101) VLT(I),PHS(I)
		END IF
		IF (JCOL.EQ.4) THEN
			 WRITE (NOUT10,101) VLT(I),PHS(I)
		END IF
		IF (JCOL.EQ.5) THEN
			 WRITE (NOUT12,101) VLT(I),PHS(I)
		END IF
	END DO
   30 CONTINUE
   99 WRITE (NOUT1,105)                                   
      CLOSE ( UNIT= IN1, STATUS= 'KEEP')
      CLOSE ( UNIT= IN2, STATUS= 'KEEP')
      CLOSE ( UNIT= IN3, STATUS= 'KEEP')
      CLOSE ( UNIT= IN4, STATUS= 'KEEP')
      CLOSE ( UNIT= IN5, STATUS= 'KEEP')
      CLOSE ( UNIT= NOUT1, STATUS= 'KEEP')
      CLOSE ( UNIT= NOUT3, STATUS= 'KEEP')
      CLOSE ( UNIT= NOUT4, STATUS= 'KEEP')
      IF (NCOL.GT.1) THEN
         CLOSE (UNIT= NOUT5, STATUS= 'KEEP')
         CLOSE (UNIT= NOUT6, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT5, STATUS= 'DELETE')
         CLOSE ( UNIT= NOUT6, STATUS= 'DELETE')
      END IF
      IF (NCOL.GT.2) THEN
         CLOSE ( UNIT= NOUT7, STATUS= 'KEEP')
         CLOSE ( UNIT= NOUT8, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT7, STATUS= 'DELETE')
         CLOSE ( UNIT= NOUT8, STATUS= 'DELETE')
      END IF
      IF (NCOL.GT.3) THEN
         CLOSE ( UNIT= NOUT9, STATUS= 'KEEP')
         CLOSE ( UNIT= NOUT10, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT9, STATUS= 'DELETE')
         CLOSE ( UNIT= NOUT10, STATUS= 'DELETE')
      END IF
      IF (NCOL.GT.4) THEN
         CLOSE ( UNIT= NOUT11, STATUS= 'KEEP')
         CLOSE ( UNIT= NOUT12, STATUS= 'KEEP')
      ELSE
         CLOSE ( UNIT= NOUT11, STATUS= 'DELETE')
         CLOSE ( UNIT= NOUT12, STATUS= 'DELETE')
      END IF
      STOP
   98 WRITE (NOUT1,102)
      CALL PMDSTOP
      END  PROGRAM LTCVA
