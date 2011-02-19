      SUBROUTINE ITPL(PH,AR1,AR2,AR3,AR4,N,LG,CL,U,V)      
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AR1(N),AR2(N),AR3(LG),AR4(LG),CG(4),AL(6) 
      KL=(N+1)/2 
      IF (KL.GT.5) KL=5 
      IF (2.0D0*PH.GT.(AR1(1)+AR1(N))) GO TO 10
      CALL INF(PH,AR1,AR2,AR4,AL,N,LG,KL,CG,U,V)
      CL=CG(1)   
      GO TO 20   
   10 CALL INB(PH,AR1,AR2,AR3,AL,N,LG,KL,CG,U,V)
      CL=CG(1)   
   20 RETURN     
      END 
