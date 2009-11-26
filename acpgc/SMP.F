      SUBROUTINE SMP(ARY,NST,NSTP,H,YM1,YNP1,KY,ER,MPHD,SM)     
C     SRT TO INTEGRATE USING SIMPSONS 1/3 RULE      
C     ARY IS THE NAME OF THE ARRAY CONTAINING ORDINATES    
C     NST IS THE ORDINAL LOCATION TO START INTEGRATION IN ARY     
C     NSTP IS THE ORDINAL LOCATION OF THE FINAL ORDINATE TO USE IN ARY   
C     H IS THE INCREMENT
C     KY IS A CONTROL INTEGER.IF KY=0,DO NOT CALCULATE THE ERROR IN      
C     THE INTEGRATION,OTHERWISE YES
C     YM1 IS THE ORDINATE IMMEDIATELY BEFORE THE NST ORDINATE
C     YNP1 IS THE ORDINATE IMMEDIATELY AFTER THE NSTP ORDINATE   
C     ER IS THE CALCULATED ERROR,IF SPECIFIED
C     SM IS THE INTEGRATION VALUE     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (MTR    =   3,
     $           MFR    =   4,
     $           MFV    =   5)
      DIMENSION ARY(MPHD),C(MTR),D(MTR),E(MFV),F(MFR) 
      DATA C(1),C(2),C(3)/1.D0,4.D0,1.D0/
      DATA D(1),D(2),D(3)/-4.D0,6.D0,-4.D0/     
      DATA E(1),E(2),E(3),E(4),E(5)/-4.D0,7.D0,-8.D0,7.D0,-4.D0/
      DATA F(1),F(2),F(3),F(4)/-4.D0,7.D0,-8.D0,8.D0/   
      ER=0.D0
      SM=0.D0    
      M=NSTP-NST
      K=M/2      
      L=M-2*K    
      IF (L.NE.0) CALL PMDSTOP 
      N=NST-1    
    5 DO 10 K=1,3
      L=N+K      
      SM=SM+C(K)*ARY(L) 
   10 CONTINUE   
      N=N+2      
      IF ((N+1).LT.NSTP) GO TO 5      
      SM=SM*H/3.D0
      IF (KY.EQ.0) GO TO 1     
      IF (M.GT.2) GO TO 6      
      ER=YM1+YNP1+D(1)*ARY(NST)+D(2)*ARY(NST+1)+D(3)*ARY(NST+2)   
      GO TO 90   
    6 IF (M.GT.4) GO TO 7      
      ER=YM1+YNP1+E(1)*ARY(NST)+E(2)*ARY(NST+1)+E(3)*ARY(NST+2)+E(4)*
     $ARY(NST+3)+E(5)*ARY(NST+4)      
      GO TO 90   
    7 ER=YM1+YNP1
      K=0 
      ER=ER+F(1)*ARY(NST+K)+F(2)*ARY(NST+K+1)+F(3)*ARY(NST+K+2)+F(4)*
     $ARY(NST+K+3)      
      K=K+4      
      IF ((NST+K).GT.(NSTP-4)) GO TO 9
    8 ER=ER+F(3)*ARY(NST+K)+F(4)*ARY(NST+K+1)
      K=K+2      
      IF ((NST+K).LE.(NSTP-4)) GO TO 8
    9 ER=ER+F(3)*ARY(NSTP-2)+F(2)*ARY(NSTP-1)+F(1)*ARY(NSTP)      
   90 ER=-ER*H/90.D0     
    1 RETURN     
      END 
