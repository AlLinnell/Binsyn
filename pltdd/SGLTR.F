      SUBROUTINE SGLTR(I,ITH,PHV,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG)
C     SRT to calculate VCSB and VCSG for tabular ITH but non-tabular
C     phi value on that theta circle.
      INCLUDE 'COMA.FOR'
      SV1=ALM(I,1,1)   
      SV2=AMU(I,1,1)   
      SV3=ANU(I,1,1)     
      SV4=VR(I,1,1)    
      SV5=G(I,1,1)     
      SV6=ZL(I,1,1)    
      SV7=ZM(I,1,1)    
      SV8=ZN(I,1,1)    
      SV9=CSB(I,1,1)   
      SV10=SNT(I,1)
C     Store parameters for specified PHV
      SNP=DSIN(PHV)      
      CSP=DCOS(PHV)
	SNT(I,1)=SNT(I,ITH)
	ANU(I,1,1)=ANU(I,ITH,1)
	ALM(I,1,1)=SNT(I,1)*CSP
	AMU(I,1,1)=SNT(I,1)*SNP
C     Calculate tidally distorted radius for the given PHV
	CALL RCHER(I,1,1,ER)
C     Calculate related parameters
	CALL RCHERA(I,1,1)
	IF (FV(I).GT.1.D0) THEN
C     Get tidally distorted radius
		RVTD=VR(I,1,1)
C     Reset polar radius for use by ROTDIST
		VR(I,1,1)=SV4
C     Use data temporarily stored in (I,1,1).
		AVL=0.5D0*(1.D0+Q)*FV(I)**2*(1.D0-ANU(I,1,1)**2)
		CALL ROTDIST(I,AVL,Q,ALM(I,1,1),SV,1.D-9)
C     Calculate amount of rotational distortion
		SVDF=SV-SV4
C     Calculate amount of tidal distortion
		RDDIFF=RVTD-SV4
C     XRVL is the combined tidal and rotational distortion
		RDCORR=SVDF+RDDIFF
C     RV is the corrected radius at the specified PHV
		IF (RDCORR.GT.0.D0) THEN
			RV=SV4+RDCORR
		ELSE
			RV=SV4
		END IF
		VR(I,1,1)=RV
		TERM1=-1.D0/VR(I,1,1)**2
C     Calculate the radial gravity term due to centripital acceleration
		TERM2=FV(I)**2*(1.D0+Q)*VR(I,1,1)*
     $		(1.D0-ANU(I,1,1)**2)
		BKT1P=(1.D0-2.D0*ALM(I,1,1)*VR(I,1,1)+VR(I,1,1)**2)**1.5D0
		TERM3=Q*((ALM(I,1,1)-VR(I,1,1))/BKT1P-ALM(I,1,1))
		POPR(I,1,1)=TERM1+TERM2+TERM3
		POPR(I,1,1)=-DABS(POPR(I,1,1))
		POPL(I,1,1)=Q*(VR(I,1,1)/BKT1P-VR(I,1,1))
		POPN(I,1,1)=-(1.D0+Q)*FV(I)**2*ANU(I,1,1)*VR(I,1,1)**2
		PLPT=ANU(I,1,1)*CSP
		PMPT=ANU(I,1,1)*SNP
		PNPT=-SNT(I,1)
		PLPP=PNPT*SNP
		PMPP=ALM(I,1,1)*SNT(I,1)
		POPT=POPL(I,1,1)*PLPT+POPN(I,1,1)*PNPT
		POPP=POPL(I,1,1)*PLPP
	IF(DABS(SNT(I,1)).GT.1.D-11) THEN
		G(I,1,1)=-DSQRT(POPR(I,1,1)**2+POPT**2/VR(I,1,1)**2+
     $		1.D0/VR(I,1,1)**2/SNT(I,1)**2*POPL(I,1,1)**2)
		POPX=ALM(I,1,1)*POPR(I,1,1)+PLPT/VR(I,1,1)*POPT+
     $		SNP**2/VR(I,1,1)*POPL(I,1,1)
		POPY=AMU(I,1,1)*POPR(I,1,1)+PMPT/VR(I,1,1)*POPT-
     $		CSP*SNP/VR(I,1,1)*POPL(I,1,1)
		POPZ=ANU(I,1,1)*POPR(I,1,1)+PNPT/VR(I,1,1)*POPT
	ELSE
		G(I,1,1)=-DSQRT(POPR(I,1,1)**2+POPT**2/VR(I,1,1)**2+
     $		1.D0/VR(I,1,1)**2*SNP**2*POPL(I,1,1)**2)
		POPX=ALM(I,1,1)*POPR(I,1,1)+PLPT/VR(I,1,1)*POPT+
     $		SNP**2/VR(I,1,1)*POPL(I,1,1)
		POPY=AMU(I,1,1)*POPR(I,1,1)+PMPT/VR(I,1,1)*POPT-
     $		SNP*CSP/VR(I,1,1)*POPL(I,1,1)
		POPZ=ANU(I,1,1)*POPR(I,1,1)+PNPT/VR(I,1,1)*POPT		
	END IF
		ZL(I,1,1)=POPX/G(I,1,1)      
		ZM(I,1,1)=POPY/G(I,1,1)      
		ZN(I,1,1)=POPZ/G(I,1,1)
		CSB(I,1,1)=ALM(I,1,1)*ZL(I,1,1)+AMU(I,1,1)*ZM(I,1,1)+
     $	ANU(I,1,1)*ZN(I,1,1)
      IF (CSB(I,1,1).GT.1.D0) CSB(I,1,1)=1.D0
		RV=VR(I,1,1)
		GV=G(I,1,1)
		VLV=ZL(I,1,1)
		VMV=ZM(I,1,1)
		VNV=ZN(I,1,1)
		VCSB=CSB(I,1,1)
	ELSE
C     Interpolate linearly between bracketing entries previously
C     calculated for photospheric grid.
          N=NPH(I,ITH)
		IF (N.EQ.1) THEN
			RV=VR(I,ITH,1)
			GV=G(I,ITH,1)
			VLV=ZL(I,ITH,1)
			VMV=ZM(I,ITH,1)
			VNV=ZN(I,ITH,1)
			VCSB=CSB(I,ITH,1)
			GO TO 1
		END IF
		DO K=1,N-1
			IF ((PHV.GE.PH(I,ITH,K)).AND.(PHV.LE.PH(I,ITH,K+1))) THEN
				RVRG=VR(I,ITH,K+1)-VR(I,ITH,K)
				ZLRG=ZL(I,ITH,K+1)-ZL(I,ITH,K)
				ZMRG=ZM(I,ITH,K+1)-ZM(I,ITH,K)
				ZNRG=ZN(I,ITH,K+1)-ZN(I,ITH,K)
				CSBRG=CSB(I,ITH,K+1)-CSB(I,ITH,K)
				GVRG=G(I,ITH,K+1)-G(I,ITH,K)
				PHRG=PH(I,ITH,K+1)-PH(I,ITH,K)
				FRACT=(PHV-PH(I,ITH,K))/PHRG
				RV=VR(I,ITH,K)+FRACT*RVRG
				GV=G(I,ITH,K)+FRACT*GVRG
				VLV=ZL(I,ITH,K)+FRACT*ZLRG
				VMV=ZM(I,ITH,K)+FRACT*ZMRG
				VNV=ZN(I,ITH,K)+FRACT*ZNRG
				VCSB=CSB(I,ITH,K)+FRACT*CSBRG
				GO TO 1
			END IF  
		END DO
		IF (PHV-PH(I,ITH,N).LT.1.D-10) THEN
			RV=VR(I,ITH,N)
			GV=G(I,ITH,N)
			VLV=ZL(I,ITH,N)
			VMV=ZM(I,ITH,N)
			VNV=ZN(I,ITH,N)
			VCSB=CSB(I,ITH,N)
			GO TO 1
		END IF
		CALL PMDSTOP
	END IF	   
1     VCSG=VLV*VL0+VMV*VM0+VNV*VN0    
      ALM(I,1,1)=SV1   
      AMU(I,1,1)=SV2   
      ANU(I,1,1)=SV3     
      VR(I,1,1)=SV4    
      G(I,1,1)=SV5     
      ZL(I,1,1)=SV6    
      ZM(I,1,1)=SV7    
      ZN(I,1,1)=SV8    
      CSB(I,1,1)=SV9   
      SNT(I,1)=SV10
      RETURN     
      END 
