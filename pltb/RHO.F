      SUBROUTINE RHO(ALF,RYA,RYB,RYC,RLP,RRS,J,IRC,IFC,NKP,RDS)     
C     Subroutine to calculate radius of projected stellar disk,using
C     parabolic fit coefficients to projected horizon points
C     IF J=2,REFER TO SMALL THETA BRANCH   
C     IF J=1,REFER TO LARGE THETA BRANCH    
	INCLUDE 'COMA.FOR'
	DIMENSION RYA(MTHM,MTR),RYB(MTHM,MTR),RYC(MNKP,MTR),
     $RLP(MTHM,MTR),
     $RRS(MTHM,MTR)
      DATA TWPI/6.2831853071796D0/
      A=ALF
      K=0
C     Accept initial choice of branch wanted
      GO TO (1,30,50) J
C************************************************************
C     Start of sequence for large theta branch
    1 IF ((A.GE.RLP(1,1)).AND.(A.LE.RLP(IFC,1))) GO TO 2
      IF ((A.LE.RLP(1,1)).AND.(A.GE.RLP(IFC,1))) GO TO 2
C     Permit angle to be a bit sloppy
      IF ((DABS(RLP(IFC,1)-A)).LT.0.000001D0)THEN
         RDS=RRS(IFC,1)
         GO TO 100
      END IF
      IF ((DABS(RLP(1,1)-A)).LT.0.000001D0) THEN
         RDS=RRS(1,1)
         GO TO 100
      END IF
      IF (A.GT.RLP(IFC,1)) THEN
         TS=A-TWPI
         IF ((TS.LE.RLP(1,2)).AND.(TS.GE.RLP(IRC,2))) THEN
            A=TS
            GO TO 30
         END IF
      END IF
      IF (A.LT.RLP(1,1)) THEN
		IF (RLP(2,1).GT.RLP(1,1)) THEN
			IF ((A.LE.RLP(1,2)).AND.(A.GE.RLP(IRC,2))) GO TO 30
		END IF
      END IF         
      CALL PMDSTOP
C     Loop for locating angle in large theta branch
    2 ILM=IFC-1
      IF (ILM.LE.0) CALL PMDSTOP
      DO 10 L=1,ILM
    9 IF ((A-RLP(L,1).GE.0.D0).AND.(A-RLP(L+1,1)).LT.0.D0) GO TO 11
      IF ((A-RLP(L,1).LE.0.D0).AND.(A-RLP(L+1,1)).GT.0.D0) GO TO 11
   10 CONTINUE
      IF ((DABS(RLP(IFC,1)-A)).GT.0.000001D0) GO TO 20
      RDS=RRS(IFC,1)
      GO TO 100
   11 RDS=RYA(L,1)+RYA(L,2)*A+RYA(L,3)*A**2
      GO TO 100
C****************************************************************
C     Start of sequence for small theta branch
   30 IF ((A.LE.RLP(1,2)).AND.(A.GE.RLP(IRC,2))) GO TO 4
      IF ((A.GE.RLP(1,2)).AND.(A.LE.RLP(IRC,2))) GO TO 4
C     Permit angle to be a bit sloppy
      IF (DABS(A-RLP(IRC,2)).LT.0.000001D0) THEN
         RDS=RRS(IRC,2)
         GO TO 100
      END IF
      IF (DABS(A-RLP(1,2)).LT.0.000001D0) THEN
         RDS=RRS(1,2)
         GO TO 100
      END IF
      IF (A.LT.RLP(IRC,2)) THEN
         TS=A+TWPI
         IF ((TS.GE.RLP(1,1)).AND.(TS.LE.RLP(IFC,1))) THEN
            A=TS
            GO TO 1
         END IF
      END IF
      IF (A.GT.RLP(1,2)) THEN
         IF ((A.GE.RLP(1,1)).AND.(A.LE.RLP(IFC,1))) GO TO 1
      END IF         
      CALL PMDSTOP
C     Loop for locating angle in small theta branch
    4 ILM=IRC-1
      IF (ILM.LE.0) CALL PMDSTOP
      DO 40 L=1,ILM
   39 IF ((A.LE.RLP(L,2)).AND.(A.GE.RLP(L+1,2))) GO TO 41
      IF ((A.GE.RLP(L,2)).AND.(A.LE.RLP(L+1,2))) GO TO 41
	IF (DABS(A-RLP(L,2)).LT.1.D-12) GO TO 41
   40 CONTINUE
      IF (DABS(A-RLP(IRC,2)).GT.0.000001D0) GO TO 23
      RDS=RRS(IRC,2)
      GO TO 100
   41 RDS=RYB(L,1)+RYB(L,2)*A+RYB(L,3)*A**2
      GO TO 100
C**************************************************************
C     Start of neck sequence
   50 IF (RLP(1,3).LT.0.0) GO TO 55
C     Neck angles are positive
      IF (A.GE.RLP(1,3)) GO TO 56
      IF ((DABS(RLP(1,3)-A).GT.0.000001D0).AND.(A.GE.0.D0)) GO TO 1
      IF ((DABS(RLP(1,3)-A).GT.0.000001D0).AND.(A.LT.0.D0)) GO TO 30
      RDS=RRS(1,3)
      GO TO 100
   56 IF (A.LE.RLP(NKP,3)) GO TO 57
      IF ((DABS(RLP(NKP,3)-A).GT.0.000001D0).AND.(A.GT.0.D0)) CALL 
     $PMDSTOP
      RDS=RRS(NKP,3)
      GO TO 100
   57 DO L=1,NKP-1
      IF ((A.GE.RLP(L,3)).AND.(A.LT.RLP(L+1,3))) GO TO 54
      END DO
      IF (A.GT.RLP(NKP,3)) CALL PMDSTOP
      RDS=RRS(NKP,3)
      GO TO 100
   55 IF (A.LT.RLP(1,3)) GO TO 51
      IF (DABS(A-RLP(1,3)).GT.0.000001D0) GO TO 24
      RDS=RRS(1,3)
      GO TO 100
   51 IF (A.GT.RLP(NKP,3)) GO TO 52
      IF (DABS(A-RLP(NKP,3)).GT.0.000001D0) GO TO 25
      RDS=RRS(NKP,3)
      GO TO 100
C     Loop for locating angle in neck sequence
   52 ILM=NKP-1
      IF (ILM.LE.0) CALL PMDSTOP
      DO 53 L=1,ILM
      IF ((A.LE.RLP(L,3)).AND.(A.GE.RLP(L+1,3))) GO TO 54
   53 CONTINUE
      CALL PMDSTOP
   54 RDS=RYC(L,1)+RYC(L,2)*A+RYC(L,3)*A**2
      GO TO 100
C*****************************************************************
C     Test angle modulo (-TWPI)
   20 A=A-TWPI
      IF (NKP.LE.1) GO TO 14
      IF ((A.LE.RLP(1,3)).AND.(A.GE.RLP(NKP,3))) GO TO 52
   14 B=1.0
      DO L=1,IRC
      IF (B.GT.RLP(L,2)) B=RLP(L,2)
      END DO
      IF (A.GE.B) GO TO 30
C     Angle falls between end of RLP(IFC,1) and RLP(NKP,3)
      IF (NKP.LE.1) CALL PMDSTOP
      A=A+TWPI
      CALL LNTER(RLP(IFC,1),RRS(IFC,1),RLP(NKP,3)+TWPI,RRS(NKP,3),A,RDS)
      GO TO 100
C*****************************************************************
C     Test for angle modulo (TWPI)
   25 A=A+TWPI
      IF (NKP.LE.1) GO TO 15
      IF ((A.LE.RLP(1,3)).AND.(A.GE.RLP(NKP,3))) GO TO 52
   15 B=-1.0
      DO L=1,IFC
      IF (B.LT.RLP(L,1)) B=RLP(L,1)
      END DO
      IF (A.LE.B) GO TO 1
      IF (NKP.LE.1) CALL PMDSTOP
      A=A-TWPI
   16 CALL LNTER(RLP(IRC,2),RRS(IRC,2),RLP(1,3),RRS(1,3),A,RDS)
      GO TO 100
C*****************************************************************
C     Test for placement in neck
   23 IF (NKP.LE.1) GO TO 25
      IF ((A.LT.RLP(IRC,2)).AND.(A.GT.RLP(1,3))) GO TO 16
      IF (A.LT.RLP(NKP,3)) GO TO 25
      GO TO 50
C     Too large for neck
   24 IF (A.GE.RLP(1,1)) GO TO 1
      GO TO 30
  100 RETURN     
      END 
