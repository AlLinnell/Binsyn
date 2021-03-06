      SUBROUTINE RCHERC(I,ITH,K) 
C     
C     THIS SRT CALCULATES REMAINING GEOMETRIC QUANTITIES FOLLOWING RCHER 
C     THE PARAMETER I DESIGNATES THE COMPONENT IN QUESTION AND K  
C     SEQUENTIALLY NUMBERS POINTS ON THE SURFACE OF THAT COMPONENT
C     L DESIGNATES THE THETA VALUE    
      INCLUDE 'COMA.FOR'
	DATA ERLT/1.D-13/
      R=VR(I,ITH,K)  
      CALL RCHP(I,ITH,K,R,VOMP)
      BKT1(I)=1.D0-2.D0*ALM(I,ITH,K)*R+R*R  
      POPL(I,ITH,K)=Q*(VR(I,ITH,K)/BKT1(I)**1.5D0-VR(I,ITH,K))
      POPN(I,ITH,K)=-(1.D0+Q)*ANU(I,ITH,K)*VR(I,ITH,K)*VR(I,ITH,K)
      PLPT=ANU(I,ITH,K)*CSP 
      PMPT=ANU(I,ITH,K)*SNP 
      PNPT=-SNT(I,ITH)    
      PLPP=-SNT(I,ITH)*SNP
      PMPP=ALM(I,ITH,K)*SNT(I,ITH)     
      POPT=POPL(I,ITH,K)*PLPT+POPN(I,ITH,K)*PNPT     
      POPP=POPL(I,ITH,K)*PLPP      
      CALL RCHP(I,ITH,K,R,VOMP)
C     For a point to be inside the Roche lobe, POPR() must be (-)
      POPR(I,ITH,K)=-DABS(POPR(I,ITH,K))
	IF (DABS(SNT(I,ITH)).GT.ERLT) THEN  
      G(I,ITH,K)=-DSQRT(POPR(I,ITH,K)*POPR(I,ITH,K)+POPT/VR(I,ITH,K)*
     $POPT/VR(I,ITH,K)+1.D0/VR(I,ITH,K)/VR(I,ITH,K)/SNT(I,ITH)
     $/SNT(I,ITH)*POPP*POPP)
      POPX=ALM(I,ITH,K)*POPR(I,ITH,K)+PLPT/VR(I,ITH,K)*POPT-SNP
     $/VR(I,ITH,K)/SNT(I,ITH)*POPP
      POPY=AMU(I,ITH,K)*POPR(I,ITH,K)+PMPT/VR(I,ITH,K)*POPT+CSP
     $/VR(I,ITH,K)/SNT(I,ITH)*POPP
      POPZ=ANU(I,ITH,K)*POPR(I,ITH,K)+PNPT/VR(I,ITH,K)*POPT
	ELSE
	G(I,ITH,K)=-DSQRT(POPR(I,ITH,K)*POPR(I,ITH,K)+POPT/VR(I,ITH,K)
     $*POPT/VR(I,ITH,K))
	POPX=ALM(I,ITH,K)*POPR(I,ITH,K)+PLPT/VR(I,ITH,K)*POPT
	POPY=AMU(I,ITH,K)*POPR(I,ITH,K)+PMPT/VR(I,ITH,K)*POPT
	POPZ=ANU(I,ITH,K)*POPR(I,ITH,K)+PNPT/VR(I,ITH,K)*POPT
	END IF 
    	ZL(I,ITH,K)=POPX/G(I,ITH,K)      
	ZM(I,ITH,K)=POPY/G(I,ITH,K)      
	ZN(I,ITH,K)=POPZ/G(I,ITH,K)      
    	CSB(I,ITH,K)=ALM(I,ITH,K)*ZL(I,ITH,K)+AMU(I,ITH,K)*ZM(I,ITH,K)
     $	+ANU(I,ITH,K)*ZN(I,ITH,K)
	RETURN
      END 
