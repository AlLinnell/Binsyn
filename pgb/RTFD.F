      SUBROUTINE RTFD(C,B,G,D,E,RT,DRT,N)     
C     SRT TO LOCATE ROOT OF FUNCTION FTHETA      
C     THE ROOT IS BETWEEN THE INDEPENDENT VARIABLE VALUES C AND B 
C     THE ROOT MUST BE LOCATED TO WITHIN AN ACCURACY LIMIT DRT    
C     UP TO N ITERATIONS ARE ALLOWED TO FIND THE ROOT      
C     ON EXIT THE VALUE OF THE INDEPENDENT VARIABLE FOR THE ROOT IS IN RT
C     The parameter G is the orbital eccentricity ECC
C     The parameter D is the longitude of periastron, OM
C     The parameter E is the orbital inclinatio VI.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (MFR    =    4)
      DIMENSION A(MFR),V(MFR)      
      IF (C.GT.B) GO TO 200    
      A(1)=C     
      A(3)=B     
      GO TO 210  
  200 A(1)=B     
      A(3)=C     
  210 CALL FTHETA(G,D,A(1),E,V(1)) 
      IF (DABS(V(1)).LT.DRT) GO TO 100 
      CALL FTHETA(G,D,A(3),E,V(3)) 
      IF (DABS(V(3)).LT.DRT) GO TO 110 
      DO 40 I=1,N
      A(2)=0.5D0*(A(1)+A(3))     
      CALL FTHETA(G,D,A(2),E,V(2)) 
      IF (DABS(V(2)).LT.DRT) GO TO 120 
      CALL LAGIN(0.D0,RT,V,A,2,3,NR,0.D0)      
      IF (NR.LT.3) CALL PMDSTOP
      IF ((RT.GT.A(1)).AND.(RT.LT.A(3))) GO TO 77   
      DO 70 J=1,2
      IF ((V(J).LT.0.D0).AND.(V(J+1).GT.0.D0)) GO TO 71      
      IF ((V(J).GT.0.D0).AND.(V(J+1).LT.0.D0)) GO TO 71      
   70 CONTINUE   
      CALL PMDSTOP      
   71 CALL LNTER(V(J),A(J),V(J+1),A(J+1),0.D0,RT)    
   77 CALL FTHETA(G,D,RT,E,RTA)    
      IF (DABS(RTA).LT.DRT) GO TO 99   
      A(4)=A(3)+10000000.D0
      CALL SRTA(RT,RTA,A,V,1,4)
      DO 81 K1=1,3      
      IF ((V(K1).LT.0.D0).AND.(V(K1+1).GT.0.D0)) GO TO 85    
      IF ((V(K1).GT.0.D0).AND.(V(K1+1).LT.0.D0)) GO TO 85    
   81 CONTINUE   
      CALL PMDSTOP      
   85 A(1)=A(K1) 
      A(3)=A(K1+1)      
      V(1)=V(K1) 
      V(3)=V(K1+1)      
   40 CONTINUE   
      CALL PMDSTOP      
  100 RT=A(1)    
   99 RETURN     
  110 RT=A(3)    
      GO TO 99   
  120 RT=A(2)    
      GO TO 99   
      END 
