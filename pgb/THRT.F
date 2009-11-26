      SUBROUTINE THRT(I,PH1,PH2,PHLL,PHLU,THVL,PHBG,VCS,N,KY)     
C     SRT TO DETERMINE THE PHI VALUE OF THE POINT OF MAXIMUM OR MINIMUM  
C     COS(GAMMA) ON THETA CIRCLE OF VALUE THVL      
C     THE EXTREMUM MUST BE INCLUDED BETWEEN LONGITUDE VALUES PH1 AND PH2 
C     THE PHI RANGE IS DIVIDED INTO N INTERVALS
C     KY INDICATES THE TYPE EXTREMUM SOUGHT  
C     IF KY=0,FIND A MAXIMUM   
C     IF KY.EQ.1,FIND A MINIMUM
	INCLUDE 'COMA.FOR'
      DIMENSION PHV(MPHM),VCSG(MPHM)
      DIMENSION AR4(MFR)  
      DIMENSION AR5(MFR), AR6(MTR,MTR),AR8(MFR),AR9(MFR)
      DATA TWPI/6.2831853071796D0/
	DATA ERLT/1.D-13/      
C     FIRST APPROX.TO THE EXTREMUM IS THE MIDPOINT
      VN=N
      PHV(2)=0.5D0*(PH1+PH2)     
      DFF=0.4D0*(PH2-PH1)
    6 PHV(1)=PHV(2)-DFF 
      PHV(3)=PHV(2)+DFF 
      IF (DABS(PHLL).LE.ERLT) THEN
	 IF (DABS(PHLU-TWPI).LE.ERLT)  GO TO 9
	END IF
      IF ((PHV(1).GE.PHLL).AND.(PHV(3).LE.PHLU))  GO TO 9  
      IF (PHV(1).GE.PHLL) GO TO 7     
      PHV(1)=PHLL
      PHV(3)=PHLL+2.D0*DFF      
      PHV(2)=0.5D0*(PHV(1)+PHV(3))      
      GO TO 9    
    7 PHV(3)=PHLU
      PHV(1)=PHLU-2.D0*DFF      
    9 DO 4 K=1,3 
      CALL SGLTP(THVL,PHV(K),.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,      
     $VCSG(K),I,NTH) 
    4 CONTINUE   
      IF (KY.EQ.0) GO TO 1     
      IF ((VCSG(2).LE.VCSG(1)).AND.(VCSG(2).LE.VCSG(3))) GO TO 35 
      GO TO 2    
    1 IF ((VCSG(2).GE.VCSG(1)).AND.(VCSG(2).GE.VCSG(3))) GO TO 35 
    2 IF (VCSG(3).NE.VCSG(1)) GO TO 5 
      IF (DABS(DFF-0.5D0*(PH2-PH1)).LE.ERLT) GO TO 25
      DFF=0.5D0*(PH2-PH1)
      GO TO 6    
C     WE HAVE DONE THE BEST WE CAN
   25 PHBG=PHV(2)
      VCS=VCSG(2)
      GO TO 18
    5 IF ((KY.EQ.0).AND.(VCSG(3).GT.VCSG(1))) DL=DFF
      IF ((KY.EQ.0).AND.(VCSG(3).LT.VCSG(1))) DL=-DFF      
      IF ((KY.EQ.1).AND.(VCSG(3).LT.VCSG(1))) DL=DFF
      IF ((KY.EQ.1).AND.(VCSG(3).GT.VCSG(1))) DL=-DFF      
      IF (DL.GT.0.D0) GO TO 30  
      IF (DL.LT.0.D0) GO TO 40  
      CALL PMDSTOP      
C     ARRIVE HERE IF LONGITUDE INCREMENTS ARE POSITIVE     
   30 DO 32 K=1,MPHM     
      DO 31 L=1,2
      PHV(L)=PHV(L+1)   
      VCSG(L)=VCSG(L+1) 
   31 CONTINUE   
      PHV(3)=PHV(2)+DL  
      IF ((PHLU.LE.TWPI).AND.(PHV(3).LE.PHLU)) GO TO 34      
      PHV(3)=PHLU 
   34 CALL SGLTP(THVL,PHV(3),.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,      
     $VCSG(3),I,NTH) 
      IF (KY.EQ.0) GO TO 33    
      IF ((VCSG(2).LE.VCSG(1)).AND.(VCSG(2).LE.VCSG(3))) GO TO 35 
      GO TO 51   
   33 IF ((VCSG(2).GE.VCSG(1)).AND.(VCSG(2).GE.VCSG(3))) GO TO 35 
   51 IF (DABS(PHLU-TWPI).LE.ERLT) GO TO 32      
      IF (PHV(3).LT.PHLU) GO TO 32    
      PHBG=PHLU  
      VCS=VCSG(3)
      GO TO 18   
   32 CONTINUE   
      CALL PMDSTOP      
C     ARRIVE HERE IF LONGITUDE INCREMENTS ARE NEGATIVE     
   40 DO 42 K=1,MPHM     
      DO 41 L=1,2
      PHV(4-L)=PHV(3-L) 
      VCSG(4-L)=VCSG(3-L)      
   41 CONTINUE   
      PHV(1)=PHV(2)+DL  
      IF (DABS(PHLL).LE.ERLT) GO TO 36
      IF (PHV(1).LT.PHLL) PHV(1)=PHLL 
   36 CALL SGLTP(THVL,PHV(1),.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,      
     $VCSG(1),I,NTH) 
      IF (KY.EQ.0) GO TO 43    
      IF ((VCSG(2).LE.VCSG(1)).AND.(VCSG(2).LE.VCSG(3))) GO TO 35 
      GO TO 52   
   43 IF ((VCSG(2).GE.VCSG(1)).AND.(VCSG(2).GE.VCSG(3))) GO TO 35 
   52 IF (DABS(PHLL).LE.ERLT) GO TO 42
      IF (PHV(1).GT.PHLL) GO TO 42    
      PHBG=PHLL  
      VCS=VCSG(1)
      GO TO 18   
   42 CONTINUE   
      CALL PMDSTOP      
C  
C     IF WE ARRIVE HERE,WE HAVE BRACKETED THE EXTREMUM     
C  
   35 LM=N
      PHS=PHV(1) 
      DV=VN
      DFF=PHV(3)-PHV(1) 
C     SUBDIVIDE INTERVAL
    3 DO 10 K=1,LM      
      L=K+1      
      VTP=K      
      PHV(L)=PHS+VTP/DV*DFF  
      CALL SGLTP(THVL,PHV(L),.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,      
     $VCSG(L),I,NTH) 
   10 CONTINUE   
      CSGM=VCSG(1)      
      KM=1
      LM=N+1
C     LOCATE EXTREME TABULAR VALUE    
      DO 11 K=2,LM      
      IF (KY.NE.0) GO TO 21    
      IF (VCSG(K).LE.CSGM) GO TO 11   
      CSGM=VCSG(K)      
      KM=K
      GO TO 11   
   21 IF (VCSG(K).GT.CSGM) GO TO 11   
      CSGM=VCSG(K)      
      KM=K
   11 CONTINUE
      DFF=DABS(PHV(2)-PHV(1))
      IF ((KM.EQ.1).AND.(DABS(PHV(1)-PH1).GT.DFF)) GO TO 26
      DFF=DABS(PHV(LM)-PHV(LM-1))
      IF ((KM.EQ.LM).AND.(DABS(PHV(KM)-PH2).GT.DFF)) GO TO 27
      IF (KM.NE.1) GO TO 8
   26 PHBG=PHV(1)
      VCS=VCSG(1)
      GO TO 18
    8 IF (KM.NE.LM) GO TO 23
   27 PHBG=PHV(KM)
      VCS=VCSG(KM)
      GO TO 18
C     SET UP SOLUTION FOR EXTREMUM OF PARABOLA INCLUDING THE CRITICAL ORDINATE  
   23 L=KM-1     
      LK=L+2     
   12 DO 13 K=L,LK      
      AR5(K-L+1)=VCSG(K)
      AR4(K-L+1)=PHV(K) 
   13 CONTINUE   
      PHS=AR4(1) 
      PHN=AR4(3) 
      AR6(1,1)=1.D0      
      AR6(2,1)=1.D0     
      AR6(3,1)=1.D0      
      AR6(1,2)=AR4(1)   
      AR6(1,3)=AR4(1)*AR4(1)   
      AR6(2,2)=AR4(2)   
      AR6(2,3)=AR4(2)*AR4(2)   
      AR6(3,2)=AR4(3)   
      AR6(3,3)=AR4(3)*AR4(3)   
      DO 19 K=1,3
      AR9(K)=AR5(K)     
   19 CONTINUE   
      AR4(4)=1000.D0     
      AR5(4)=1000.D0    
      CALL LEQ(AR6,AR9,3,1,3,3,DET)
   22 PHBG=-AR9(2)/AR9(3)/2.D0  
      IF ((PHBG.GE.PHS).AND.(PHBG.LE.PHN)) GO TO 17 
      CALL PMDSTOP      
   17 CALL SGLTP(THVL,PHBG,.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,VCS,I,
     $NTH)  
      DO 29 K=1,3
C     TEST FOR COINCIDENCE WITH A GRID POINT.IF YES,WE ARE CLOSE ENOUGH  
      IF (DABS(PHBG-AR4(K)).LT.0.000000001D0) GO TO 18 
   29 CONTINUE   
      CALL SRTA(PHBG,VCS,AR4,AR5,1,4) 
      CSGM=AR5(1)
      KL=1
C     LOCATE THE EXTREME TABULAR VALUE
      DO 14 K=2,4
      IF (KY.NE.0) GO TO 20    
      IF (AR5(K).LE.CSGM) GO TO 14    
      CSGM=AR5(K)
      KL=K
      GO TO 14   
   20 IF (AR5(K).GT.CSGM) GO TO 14    
      CSGM=AR5(K)
      KL=K
   14 CONTINUE   
C     SET UP FOR A SECOND PARABOLIC APPROXIMATION   
      L=KL-1     
      IF (L.LE.0) L=L+1 
      LK=L+2     
      IF (LK.LE.4) GO TO 15    
      L=L-1      
      LK=LK-1    
   15 DO 16 K=L,LK      
      AR9(K-L+1)=AR5(K) 
      AR8(K-L+1)=AR4(K) 
   16 CONTINUE   
      AR6(1,1)=1.D0
      AR6(2,1)=1.D0     
      AR6(3,1)=1.D0     
      AR6(1,2)=AR8(1)   
      AR6(1,3)=AR8(1)*AR8(1)   
      AR6(2,2)=AR8(2)   
      AR6(2,3)=AR8(2)*AR8(2)   
      AR6(3,2)=AR8(3)   
      AR6(3,3)=AR8(3)*AR8(3)   
C     SOLVE FOR EXTREMUM
      CALL LEQ(AR6,AR9,3,1,3,3,DET)
   24 PHBG=-AR9(2)/AR9(3)/2.D0  
      CALL SGLTP(THVL,PHBG,.000000001D0,RV,GV,VLV,VMV,VNV,VCSB,VCS,I,
     $NTH)  
      IF ((PHBG.GE.PHS).AND.(PHBG.LE.PHN)) GO TO 18 
      CALL PMDSTOP      
   18 RETURN     
      END 
