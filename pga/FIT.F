      SUBROUTINE FIT(N,AR6,AR7,AR4,KR)
C     SRT TO FIT A SECOND DEGREE POLYNOMIAL TO DATA IN ARRAY AR7  
C     AR6 IS THE ARRAY OF INDEPENDENT VARIABLE VALUES      
C     AR7 IS THE ARRAY OF DEPENDENT VARIABLE VALUES 
C     N IS THE NUMBER OF ITEMS IN ARRAYS AR6 AND AR7
C     ON RETURN AR4 CONTAINS THE SOLUTION VECTOR    
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (MTR    =    3)
      DIMENSION AR6(N),AR7(N),AR3(MTR,MTR),AR4(MTR)
      DIMENSION AR1(N),AR2(N)
	DATA ERLT/1.E-13/
      KR=0
      IF (N.LT.3) CALL PMDSTOP 
      DO 1 J=1,N 
      AR1(J)=AR6(J)     
      AR2(J)=AR7(J)     
    1 CONTINUE   
      KLM=N-1    
      DO 9 J=1,2 
      DO 8 I=1,KLM      
      IF ((AR1(I+1)-AR1(I)).GE.ERLT) GO TO 8 
      SV=AR1(I)  
      AR1(I)=AR1(I+1)   
      AR1(I+1)=SV
      SV=AR2(I)  
      AR2(I)=AR2(I+1)   
      AR2(I+1)=SV
    8 CONTINUE   
    9 CONTINUE   
      IF (N.GT.3) GO TO 3      
      S1=AR1(1)-AR1(3)  
      S2=AR1(2)-AR1(3)  
      S3=AR1(1)**2-AR1(3)**2   
      S4=AR1(2)**2-AR1(3)**2   
      S5=AR2(1)-AR2(3)  
      S6=AR2(2)-AR2(3)  
      S7=S5*S2   
      S8=S6*S1   
      S9=S3*S2   
      S10=S4*S1  
      S11=S9-S10 
      S12=S7-S8  
      IF (S11.NE.0.D0) GO TO 20 
  200 FORMAT (1X,'**FITA**')   
      WRITE (2,200)     
      AR4(3)=0.D0
      GO TO 11   
   20 AR4(3)=S12/S11    
   11 IF (S1.NE.0.D0) GO TO 30  
  102 FORMAT (1X,'**FITB**')   
      WRITE (2,102)     
      AR4(2)=0.D0
      GO TO 21   
   30 AR4(2)=(S5-AR4(3)*S3)/S1 
   21 AR4(1)=AR2(1)-AR4(2)*AR1(1)-AR4(3)*AR1(1)**2  
      GO TO 6    
C     INITIALIZE ARRAYS 
    3 DO 5 J=1,3 
      AR4(J)=0.D0
      DO 4 K=1,3 
      AR3(J,K)=0.D0      
    4 CONTINUE   
    5 CONTINUE   
C     PREPARE COEFFICIENT MATRICES    
      DO 10 J=1,N
      AR3(1,2)=AR3(1,2)+AR1(J) 
      AR3(1,3)=AR3(1,3)+AR1(J)*AR1(J) 
      AR3(2,3)=AR3(2,3)+AR1(J)**3     
      AR3(3,3)=AR3(3,3)+AR1(J)**4     
      AR4(1)=AR4(1)+AR2(J)     
      AR4(2)=AR4(2)+AR2(J)*AR1(J)     
      AR4(3)=AR4(3)+AR2(J)*AR1(J)**2  
   10 CONTINUE   
      AR3(1,1)=N 
C     ALLOW FOR SYMMETRY
      AR3(2,1)=AR3(1,2) 
      AR3(2,2)=AR3(1,3) 
      AR3(3,1)=AR3(1,3) 
      AR3(3,2)=AR3(2,3) 
C     SOLVE THE SYSTEM OF EQUATIONS   
      CALL LEQ(AR3,AR4,3,1,3,3,DET)
    6 RETURN     
      END 
