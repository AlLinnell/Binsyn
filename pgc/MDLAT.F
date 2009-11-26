      SUBROUTINE MDLAT(WL,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,TM,VA,B)     
C     SRT TO PRODUCE OUTWARD FLUX,AT    
C     SPECIFIED WAVELENGTH,TEMPERATURE,AND PHOTOSPHERIC GRAVITY,BY 
C     LINEAR
C     INTERPOLATION IN MODEL ATMOSPHERE VALUES,STORED IN ARRAY RFLX      
C     THE VALUES IN RFLX HAVE BEEN READ IN BY AN EXTERNAL PROGRAM 
      INCLUDE 'COMA.FOR'
      DIMENSION GRPH(MTW,MFV),TMPH(MTW,MSX),KGR(MTW),KTMP(MTW)
      DATA CF/4000.D0/
C     Convert to wavelengths in angstroms      
      W=WL*10000.D0
      T=TM
C     INTERPOLATE IN LOG10 OF GRAVITY
      IF (DABS(VA).LT.0.000001D0) GO TO 2
      VG=DLOG10(DABS(VA))
      IF (VG.GE.GRPH(ICOMP,1)) GO TO 1     
    2 J=1
C     Note that entries in GRPH(ICOMP,J) are logarithmic     
      VG=GRPH(ICOMP,1)
      GO TO 9
    1 NLMT=KGR(ICOMP)-1
      IF (NLMT.LT.1) THEN
		NLMT=1
		GO TO 2
	END IF		  
      DO 10 J=1,NLMT 
      IF ((VG.GE.GRPH(ICOMP,J)).AND.(VG.LT.GRPH(ICOMP,J+1))) GO TO 9 
   10 CONTINUE   
      VG=GRPH(ICOMP,KGR(ICOMP))     
      J=KGR(ICOMP)    
    9 IF (T.GE.TMPH(ICOMP,1)) GO TO 11      
   15 T=TMPH(ICOMP,1)
      K=1  
      GO TO 23
   11 NLMT=KTMP(ICOMP)-1
      IF (NLMT.LT.1) THEN 
		NLMT=1
		GO TO 15
	END IF		   
      DO 12 K=1,NLMT
      IF ((T.GE.TMPH(ICOMP,K)).AND.(T.LT.TMPH(ICOMP,K+1))) GO TO 23    
   12 CONTINUE
      K=KTMP(ICOMP)     
      T=TMPH(ICOMP,KTMP(ICOMP))      
   23 IF (W.GE.WLVRY(1)) GO TO 13
   17 L=1 
      W=WLVRY(1)   
      GO TO 16
   13 NLMT=NWLV-1
      IF (NLMT.LT.1) THEN
		NLMT=1
		GO TO 17
	END IF		   
      DO 14 L=1,NLMT
      IF ((W.GE.WLVRY(L)).AND.(W.LT.WLVRY(L+1))) GO TO 16      
   14 CONTINUE   
      L=NWLV      
      W=WLVRY(NWLV) 
   16 CALL TDTPL3A(J,K,L,ICOMP,KGR,KTMP,NWLV,GRPH,TMPH,VG,T,W,B)
C     Convert to physical flux and to a bandwidth of one micron
      B=B*CF   
   20 RETURN     
      END 
