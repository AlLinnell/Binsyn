      SUBROUTINE RCHERB(I,ITH,K,ERP)
C     
C     THIS SRT CALCULATES THE DISTANCE FROM THE ORIGIN TO THE SURFACE    
C     OF SPECIFIED POTENTIAL,FOR A DOUBLE STAR SYSTEM      
C     DIRECTION COSINES OF THE LINE MUST BE STORED IN ADVANCE IN  
C     ARRAYS ALM,AMU,ANU
C     THE PARAMETER I DESIGNATES THE COMPONENT IN QUESTION AND K  
C     SEQUENTIALLY NUMBERS POINTS ON THE SURFACE OF THAT COMPONENT
C     THE DETERMINED DISTANCE IS RETURNED VIA THE VR ARRAY 
C     L DESIGNATES THE THETA VALUE    
C     THE ERROR LIMIT AND NUMBER OF PERMITTED ITERATIONS ARE ERP AND KLM1 
      INCLUDE 'COMA.FOR'
      INCLUDE 'COMM.FOR'
      DIMENSION ERRV(MFV)
	DATA ERLT/1.D-13/  
C     Nominal upper limit on radius is RM, set equal to that X axis
C     intersection with the specified potential that lies between the
C     component in question and its companion.
      SV=ALM(I,ITH,K)
      ICT=0
	I4CT=0
	I3CT=0
C     Following specification allows for case of overcontact component.
C     Approximate neck cross-section as circle of radius YMN.
      RM=DSQRT(XMN(I)**2+2.D0*YMN**2)*1.000000001D0
C     Skip following RM value if the point lies on the opposed
C     hemisphere of the star.
      IF (ALM(I,ITH,K).LT.0.D0) GO TO 21
C     Special treatment if overcontact:test for it by YMN value.
C     If overcontact, YMN .GT. 0.0.
    8 IF (YMN.GT.0.D0) GO TO 7
C     Not overcontact and on hemisphere facing companion.
C     Maximum radius is intersection of critical potential with X axis
C     in this hemisphere. Take RM very slightly beyond point to allow
C     a test. 
      RM=XMN(I)*1.0000000001D0
      RLMT=RM
C     Exit if we are sufficiently close to the neck in overcontact case
      IF ((DABS(1.D0-SV).LT.1.D-9).AND.(DABS(OM(I)-OMC(I,1)).LT.
     $0.00001D0).AND.(YMN.GT.0.D0)) GO TO 12
C     Use standard procedure for all cases of non-contact systems.
      IF (DABS(ALMX(I)-1.D0).LE.ERLT) GO TO 50
C     In overcontact case, go to st. 21 for all directions outside of
C     directions into the neck.    
    7 IF (ALM(I,ITH,K).LE.ALMX(I)) GO TO 21
C     Special case of vectors pointing into neck.
C     Calculate a cross-sectional interface.          
    5 CSPH=ALM(I,ITH,K)/DSQRT(1.D0-ANU(I,ITH,K)**2)    
      XVL=YMN*ALMY(I)/CSPH     
      IF (DABS(YMN).GT.ERLT) VR(I,ITH,K)=ZMN**2+XVL**2*(1.D0-ZMN**2/
     $YMN**2)+XMN(I)**2  
      IF (DABS(YMN).LE.ERLT) VR(I,ITH,K)=XMN(I)**2
      VR(I,ITH,K)=DSQRT(VR(I,ITH,K))
C     Exit	    
      GO TO 2    
C     The radius must be larger than the following RVS.
C     The value of RVS is calculated with only the leading term in the 
C     expression for the Roche potential.      
   21 RVS=1.D0/OM(I)
C     Choose initial approximation that is mean of smallest possible
C     and largest possible.
      RVLG=0.5D0*(RVS+RM)
	DLR=RM-RVLG      
C     Calculate the difference between the potential for the specified
C     distance, direction, and Q value, and the required Roche 
C     potential for the star. The difference is in the sense
C     calculated potential-Roche potential.
C     If the point is an infinitesimal distance beyond the Roche
C     potential, ERRV(I) will be negative.
C     If the point is inside the Roche potential surface for the 
C     specified direction, ERRV(I) will be positive.
C     The indices I and 3-I for ERV have no specific connection to the
C     components.
    4 DLR=DLR/4.D0	      
   41 CALL RCH(I,ITH,K,RVLG,ERRV(I))
C     If we happen to hit the point on the Roche potential surface, 
C     exit.
      IF (DABS(ERRV(I)).LT.ERP) GO TO 11
	IF ((DABS(SV-1.D0).LE.ERLT).AND.(DABS(ERRV(I)).LT.0.00001D0)) 
     $GO TO 11
C	IF ((DABS(RVLG-RM).LT.ERP).AND.(ICT.GT.10)) GO TO 11
C     If following condition is true the value of r is too large.
      IF (ERRV(I).LT.0.D0) GO TO 3
C     The value of r is too small.
C     Must choose larger initial value.
      RVLG=RVLG+0.5D0*DLR
	IF (RVLG.GT.RM) THEN
		RVLG=RVLG-0.5D0*DLR
		DLR=DLR/4.D0
		IF (DLR.LT.ERP) GO TO 11
	END IF
      ICT=ICT+1
	I4CT=I4CT+1
C     Continue until we find an RVLG that is too large	  
      IF (ICT.LT.500) GO TO 41
	CALL PMDSTOP
C     If arrive here, the value of RVLG is too large.	    
    3 DLR=DLR/4.D0
C     Set initial value of RVSM to ending value RVLG.
C     The initial value of RVSM then is too large.
C     Decrease it stepwise until we cross the root.
	RVSM=RVLG+0.5D0*DLR
   31 RVSM=RVSM-0.5D0*DLR
      CALL RCH(I,ITH,K,RVSM,ERRV(3-I))
C     Test whether we have finished.        
      IF (DABS(ERRV(3-I)).LT.ERP) GO TO 13
C     Special test for overcontact case.
      IF (ALMX(I).NE.1.D0) THEN	     
		IF ((DABS(ERRV(3-I)).LT.0.00005D0).AND.(ALM(I,ITH,K).GT.
     $	(ALMX(I)-0.005D0))) GO TO 5
      END IF   
C     If ERRV(3-I) is .GT.0.0, the value of RMSV is too small.
C     Continue looping until we cross over the root.
      IF (ERRV(3-I).LT.0.D0) THEN
      I3CT=I3CT+1
	ICT=ICT+1
	GO TO 31
	END IF
C     The value of RVSM is now too small.      
C     Test whether to go through the loop for increasing the radius.
C      
	RVLG=RVSM
      GO TO 4
    2 ALM(I,ITH,K)=SV
   24 RETURN
   11 VR(I,ITH,K)=RVLG
      GO TO 2     
   12 VR(I,ITH,K)=RLMT
    	GO TO 2
   13 VR(I,ITH,K)=RVSM
      GO TO 2
C     The radius must be larger than the following RVS.
C     The value of RVS is calculated with only the leading term in the 
C     expression for the Roche potential.      
   50 RVS=1.D0/OM(I)
C     Choose initial approximation that is mean of smallest possible
C     and largest possible.
      RVLG=0.5D0*(RVS+RM)
	DLR=RM-RVLG
   54 DLR=DLR/4.D0
   55 CALL RCH(I,ITH,K,RVLG,ERRV(I))
      IF (DABS(ERRV(I)).LT.ERP) GO TO 11
	IF ((DABS(SV-1.D0).LE.ERLT).AND.(DABS(ERRV(I)).LT.ERP)) 
     $GO TO 11
      IF (ERRV(I).LT.0.D0) GO TO 53
C     The value of r is too small.
C     Must choose larger initial value.
      RVLG=RVLG+0.5D0*DLR
	IF (RVLG.GT.RM) THEN
		RVLG=RVLG-0.5D0*DLR
		DLR=DLR/4.D0
		IF (DLR.LT.ERP) GO TO 11
	END IF
      ICT=ICT+1
C     Continue until we find an RVLG that is too large	  
      IF (ICT.LT.500) GO TO 55
	CALL PMDSTOP
C     Set initial value of RVSM to ending value RVLG.
C     The initial value of RVSM then is too large.
C     Decrease it stepwise until we cross the root.
   53 DLR=DLR/4.D0
	RVSM=RVLG+0.5D0*DLR
   51 RVSM=RVSM-0.5D0*DLR
      CALL RCH(I,ITH,K,RVSM,ERRV(3-I))
C     Test whether we have finished.        
      IF (DABS(ERRV(3-I)).LT.ERP) GO TO 13
C     If ERRV(3-I) is .GT.0.0, the value of RMSV is too small.
C     Continue looping until we cross over the root.
      IF (ERRV(3-I).LT.0.D0) THEN
	ICT=ICT+1
	GO TO 51
	END IF
C     The value of RVSM is now too small.      
C     Test whether to go through the loop for increasing the radius.
	RVLG=RVSM
      GO TO 54
      END
