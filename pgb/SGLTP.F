      SUBROUTINE SGLTP(THV,PHV,ER,RV,GV,VLV,VMV,VNV,VCSB,VCSG,I,NTH) 
C     SRT to calculate surface parameters for values of theta (THV)
C     and phi (PHV) that do not correspond to a grid point.    
      INCLUDE 'COMA.FOR'
	DATA PI/3.141592653589793238463D0/
C     Use the storage values at (I,1,1) for temporary storage.
C     Save current values
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
	IF (THV.LT.1.D-11) THEN
		RV=VR(I,1,1)
		GV=G(I,1,1)
		VLV=ZL(I,1,1)
		VMV=ZM(I,1,1)
		VNV=ZN(I,1,1)
		VCSB=CSB(I,1,1)
		VCSG=CSG(I,1,1)
		RETURN
	END IF
	IF (PI-THV.LT.1.D-11) THEN
		RV=VR(I,NTH,1)
		GV=G(I,NTH,1)
		VLV=ZL(I,NTH,1)
		VMV=ZM(I,NTH,1)
		VNV=ZN(I,NTH,1)
		VCSB=CSB(I,NTH,1)
		VCSG=CSG(I,NTH,1)
		RETURN
	END IF
C     Store parameters for specified THV and PHV	    
      SNTH=DSIN(THV)     
      SNT(I,1)=SNTH    
      ANU(I,1,1)=DCOS(THV)
      SNP=DSIN(PHV)      
      CSP=DCOS(PHV)      
      ALM(I,1,1)=SNTH*CSP     
      AMU(I,1,1)=SNTH*SNP
C     Calculate tidally distorted radius for the given THV and PHV
      CALL RCHER(I,1,1,ER)
C     Calculate related parameters, store in (I,1,1)
	CALL RCHERA(I,1,1)
	IF (FV(I).GT.1.D0) THEN
C     Get calculated tidally distorted radius
		RVTD=VR(I,1,1)
C     Reset polar radius for use by ROTDIST
		VR(I,1,1)=SV4				    
C     Use data temporarily stored in (I,1,1)
		AVL=0.5D0*(1.D0+Q)*FV(I)**2*(1.D0-ANU(I,1,1)**2)
		CALL ROTDIST(I,AVL,Q,ALM(I,1,1),SV,1.D-9)
C     Calculate amount of rotational distortion
		SVDF=SV-SV4
C     Calculate amount of tidal distortion
		RDDIFF=RVTD-SV4
C     RDCORR is the radius correction due to combined tidal and
C     rotational distortion.
		RDCORR=RDDIFF+SVDF
C     The calculated radius is the polar radius plus correction
		IF (RDCORR.GT.0.D0) THEN
			RV=VR(I,1,1)+RDCORR
		ELSE
			RV=SV4
		END IF
		VR(I,1,1)=RV
		TERM1=-1.D0/VR(I,1,1)**2
C     Calculate the radial gravity term due to centripital acceleration
		TERM2=FV(I)**2*(1.D0+Q)*VR(I,1,1)*
     $		(1.D0-ANU(I,1,1)**2)
		BKT1P=(1.D0-2.D0*ALM(I,1,1)*VR(I,1,1)+
     $		VR(I,1,1)**2)**1.5D0
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
	END IF
	RV=VR(I,1,1)     
	GV=G(I,1,1)      
	VLV=ZL(I,1,1)    
	VMV=ZM(I,1,1)    
	VNV=ZN(I,1,1)    
	VCSB=CSB(I,1,1)
	VCSG=VLV*VL0+VMV*VM0+VNV*VN0    
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
