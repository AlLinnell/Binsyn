      PROGRAM DVFLCR
C     Program to receive derivative value data from program PEDCS
C     and prepare separate files for plots of derivatives.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION FL1(21),NCT(5)
	DIMENSION PHV(90000),DLDIV(90000),DLDECCV(90000),DLDOMV(90000),
     $DLDPT1V(90000),
     $DLDPT2V(90000),DLDQV(90000),DLDA1V(90000),DLDA2V(90000),
     $DLDB1V(90000),
     $DLDB2V(90000),DLDT1V(90000),DLDT2V(90000),DLDUC1V(90000),
     $DLDUC2V(90000),
     $DLDTZ1V(90000),DLDTZ2V(90000),DLDL31V(90000)
      CHARACTER * 36 BFR,FL1
      DATA KEYBAD/-1/
      DATA KEYGOOD/0/
  100 FORMAT (8D15.8)
  101 FORMAT (A36)
  102 FORMAT (I2)
  103 FORMAT (2E12.4)
  104 FORMAT (2D20.7,F5.2)
  106 FORMAT (10I5)
C******************************************************************
C
      OPEN ( UNIT= 23, FILE='DVFLCR.ILS',STATUS='OLD')
      READ (23, 101) (FL1(K),K=1,21)
      OPEN ( UNIT=  1, FILE= FL1(1), STATUS=  'OLD')
      WRITE ( 1, 102) KEYBAD
      REWIND 1
      OPEN ( UNIT=  2, FILE= FL1(2), STATUS=  'OLD')
      OPEN ( UNIT=  3, FILE= FL1(3), STATUS=  'OLD')
      OPEN ( UNIT=  4, FILE= FL1(4), STATUS=  'REPLACE')    !DLDI
      OPEN ( UNIT=  5, FILE= FL1(5), STATUS=  'REPLACE')    !DLDECC
      OPEN ( UNIT=  66, FILE= FL1(6), STATUS=  'REPLACE')    !DLDOM
      OPEN ( UNIT=  7, FILE= FL1(7), STATUS=  'REPLACE')    !DLDPT1
      OPEN ( UNIT=  8, FILE= FL1(8), STATUS=  'REPLACE')    !DLDPT2
      OPEN ( UNIT=  9, FILE= FL1(9), STATUS=  'REPLACE')    !DLDQ
      OPEN ( UNIT= 10, FILE= FL1(10), STATUS=  'REPLACE')    !DLDA1
      OPEN ( UNIT= 11, FILE= FL1(11), STATUS=  'REPLACE')    !DLDA2
      OPEN ( UNIT= 12, FILE= FL1(12), STATUS=  'REPLACE')    !DLDB1
      OPEN ( UNIT= 13, FILE= FL1(13), STATUS=  'REPLACE')    !DLDB2
      OPEN ( UNIT= 14, FILE= FL1(14), STATUS=  'REPLACE')    !DLDT1
      OPEN ( UNIT= 15, FILE= FL1(15), STATUS=  'REPLACE')    !DLDT2
      OPEN ( UNIT= 16, FILE= FL1(16), STATUS=  'REPLACE')    !DLDUC1
      OPEN ( UNIT= 17, FILE= FL1(17), STATUS=  'REPLACE')    !DLDUC2
      OPEN ( UNIT= 18, FILE= FL1(18), STATUS=  'REPLACE')    !DLDTZ1
      OPEN ( UNIT= 19, FILE= FL1(19), STATUS=  'REPLACE')    !DLDTZ2
      OPEN ( UNIT= 20, FILE= FL1(20), STATUS=  'REPLACE')    !DLDL31
      OPEN ( UNIT= 21, FILE= FL1(21), STATUS=  'OLD')    !obs. data
C******************************************************************
      READ (2,106) I,NT,NWL
C     Determine number of observations in each wavelength
      READ (21,101) BFR
      DO 2 IWL=1,NWL
         NCT(IWL)=0
         DO I=1,90000
            READ (21,104,END=1,ERR=199) A1,A2,A3
            IF ((A1.EQ.0.D0).AND.(A2.EQ.0.D0)) GO TO 3
         END DO
    1    CALL PMDSTOP
    3    NCT(IWL)=I-1
    2 CONTINUE
      REWIND 21
      READ (21,101) BFR
      DO 50 IWL=1,NWL
      NLM=NCT(IWL)
      DO 10 I=1,NLM
      READ (3,100,END=11,ERR=199) PH,DLDI,DLDECC,DLDOM,DLDPT1,DLDPT2,
     $DLDQ,DLDA1,DLDA2,DLDB1,DLDB2,DLDT1,DLDT2,DLDUC1,DLDUC2,DLDTZ1,
     $DLDTZ2,DLDL31
      IF (PH.GT.0.70D0) PH=PH-1.D0
	PHV(I)=PH
	DLDIV(I)=DLDI
	DLDECCV(I)=DLDECC
	DLDOMV(I)=DLDOM
	DLDPT1V(I)=DLDPT1
	DLDPT2V(I)=DLDPT2
	DLDQV(I)=DLDQ
	DLDA1V(I)=DLDA1
	DLDA2V(I)=DLDA2
	DLDB1V(I)=DLDB1
	DLDB2V(I)=DLDB2
	DLDT1V(I)=DLDT1
	DLDT2V(I)=DLDT2
	DLDUC1V(I)=DLDUC1
	DLDUC2V(I)=DLDUC2
	DLDTZ1V(I)=DLDTZ1
	DLDTZ2V(I)=DLDTZ2
	DLDL31V(I)=DLDL31
   10 CONTINUE
C     Sort by phase
      DO I=1,NLM
		DO J=1,NLM-1
			IF (PHV(J).GT.PHV(J+1)) THEN
				SV=PHV(J)
				PHV(J)=PHV(J+1)
				PHV(J+1)=SV
				SV=DLDIV(J)
				DLDIV(J)=DLDIV(J+1)
				DLDIV(J+1)=SV
				SV=DLDECCV(J)
				DLDECCV(J)=DLDECCV(J+1)
				DLDECCV(J+1)=SV
				SV=DLDOMV(J)
				DLDOMV(J)=DLDOMV(J+1)
				DLDOMV(J+1)=SV
				SV=DLDPT1V(J)
				DLDPT1V(J)=DLDPT1V(J+1)
				DLDPT1V(J+1)=SV
				SV=DLDPT2V(J)
				DLDPT2V(J)=DLDPT2V(J+1)
				DLDPT2V(J+1)=SV
				SV=DLDQV(J)
				DLDQV(J)=DLDQV(J+1)
				DLDQV(J+1)=SV
				SV=DLDA1V(J)
				DLDA1V(J)=DLDA1V(J+1)
				DLDA1V(J+1)=SV
				SV=DLDA2V(J)
				DLDA2V(J)=DLDA2V(J+1)
				DLDA2V(J+1)=SV
				SV=DLDB1V(J)
				DLDB1V(J)=DLDB1V(J+1)
				DLDB1V(J+1)=SV
				SV=DLDB2V(J)
				DLDB2V(J)=DLDB2V(J+1)
				DLDB2V(J+1)=SV
				SV=DLDT1V(J)
				DLDT1V(J)=DLDT1V(J+1)
				DLDT1V(J+1)=SV
				SV=DLDT2V(J)
				DLDT2V(J)=DLDT2V(J+1)
				DLDT2V(J+1)=SV
				SV=DLDUC1V(J)
				DLDUC1V(J)=DLDUC1V(J+1)
				DLDUC1V(J+1)=SV
				SV=DLDUC2V(J)
				DLDUC2V(J)=DLDUC2V(J+1)
				DLDUC2V(J+1)=SV
				SV=DLDTZ1V(J)
				DLDTZ1V(J)=DLDTZ1V(J+1)
				DLDTZ1V(J+1)=SV
				SV=DLDTZ2V(J)
				DLDTZ2V(J)=DLDTZ2V(J+1)
				DLDTZ2V(J+1)=SV
				SV=DLDL31V(J)
				DLDL31V(J)=DLDL31V(J+1)
				DLDL31V(J+1)=SV
			END IF
		END DO
	END DO
C     Check
      DO I=1,NLM-1
        AAA=0.D0
      	IF (PHV(I).GT.PHV(I+1)) THEN
	AAA=PHV(I)
        BBB=PHV(I+1)
        CCC=0.D0
        END IF
      END DO
	DO I=1,NLM
C     Write derivatives for plotting only for the first wavelength
      IF (IWL.EQ.1) THEN
		WRITE (4,103) PHV(I),DLDIV(I)
		WRITE (5,103) PHV(I),DLDECCV(I)
		WRITE (66,103) PHV(I),DLDOMV(I)
		WRITE (7,103) PHV(I),DLDPT1V(I)
		WRITE (8,103) PHV(I),DLDPT2V(I)
		WRITE (9,103) PHV(I),DLDQV(I)
		WRITE (10,103) PHV(I),DLDA1V(I)
		WRITE (11,103) PHV(I),DLDA2V(I)
		WRITE (12,103) PHV(I),DLDB1V(I)
		WRITE (13,103) PHV(I),DLDB2V(I)
		WRITE (14,103) PHV(I),DLDT1V(I)
		WRITE (15,103) PHV(I),DLDT2V(I)
		WRITE (16,103) PHV(I),DLDUC1V(I)
		WRITE (17,103) PHV(I),DLDUC2V(I)
		WRITE (18,103) PHV(I),DLDTZ1V(I)
		WRITE (19,103) PHV(I),DLDTZ2V(I)
		WRITE (20,103) PHV(I),DLDL31V(I)
	END IF
        IF ((I.GE.2).AND.(PHV(I).LT.PHV(I-1)))THEN
        ZZZ=0.D0
        END IF
      END DO
   50 CONTINUE
C*****************************************************************
      CLOSE ( UNIT=  2, STATUS=  'KEEP')
      CLOSE ( UNIT=  3, STATUS=  'KEEP')
      CLOSE ( UNIT=  4, STATUS=  'KEEP')
      CLOSE ( UNIT=  5, STATUS=  'KEEP')
      CLOSE ( UNIT=  66, STATUS=  'KEEP')
      CLOSE ( UNIT=  7, STATUS=  'KEEP')
      CLOSE ( UNIT=  8, STATUS=  'KEEP')
      CLOSE ( UNIT=  9, STATUS=  'KEEP')
      CLOSE ( UNIT= 10, STATUS=  'KEEP')
      CLOSE ( UNIT= 11, STATUS=  'KEEP')
      CLOSE ( UNIT= 12, STATUS=  'KEEP')
      CLOSE ( UNIT= 13, STATUS=  'KEEP')
      CLOSE ( UNIT= 14, STATUS=  'KEEP')
      CLOSE ( UNIT= 15, STATUS=  'KEEP')
      CLOSE ( UNIT= 16, STATUS=  'KEEP')
      CLOSE ( UNIT= 17, STATUS=  'KEEP')
      CLOSE ( UNIT= 18, STATUS=  'KEEP')
      CLOSE ( UNIT= 19, STATUS=  'KEEP')
      CLOSE ( UNIT= 20, STATUS=  'KEEP')
      CLOSE ( UNIT= 21, STATUS=  'KEEP')
	CLOSE ( UNIT= 23, STATUS=  'KEEP')
      WRITE ( 1, 102) KEYGOOD
      CLOSE ( UNIT = 1, STATUS=  'KEEP')
C*****************************************************************
      STOP 'DVFLCR Completed successfully.'
  199 CALL PMDSTOP
   11 CALL PMDSTOP
      END PROGRAM DVFLCR
