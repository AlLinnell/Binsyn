      SUBROUTINE DKFF(W,NGR,NTMP,NRF,T,VA,UA,UB)    
C     VERSION OF 8/11/83
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'COMD.FOR'
      V=T
      NL=NGR-1   
C     INTERPOLATE IN LOG10 OF GRAVITY 
      IF (VA.LT.0.000001D0) GO TO 4
      VG=DLOG10(DABS(VA))
      IF (VG.GE.RGRAV(1)) GO TO 1     
    4 J=1 
      VG=RGRAV(1)
      GO TO 2    
    1 DO 10 J=1,NL      
      IF ((VG.GE.RGRAV(J)).AND.(VG.LE.RGRAV(J+1))) GO TO 2 
   10 CONTINUE   
      J=NGR      
      VG=RGRAV(NGR)     
    2 IF (V.GE.RTMP(1)) GO TO 11      
      K=1 
      V=RTMP(1)  
      GO TO 3    
   11 NL=NTMP-1  
      DO 12 K=1,NL      
      IF ((V.GE.RTMP(K)).AND.(V.LE.RTMP(K+1))) GO TO 3     
   12 CONTINUE   
      K=NTMP     
      V=RTMP(NTMP)      
    3 IF (W.GE.RWL(1)) GO TO 13
      L=1 
      W=RWL(1)   
      GO TO 15   
   13 NL=NRF-1   
      DO 14 L=1,NL      
      IF ((W.GE.RWL(L)).AND.(W.LE.RWL(L+1))) GO TO 15      
   14 CONTINUE   
      L=NRF      
      W=RWL(NRF) 
   15 CALL TDTPM1(J,K,L,VG,V,W,UA)    
      CALL TDTPM2(J,K,L,VG,V,W,UB)    
      RETURN     
      END 
