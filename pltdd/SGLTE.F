      SUBROUTINE SGLTE(I,ITH,PHV,ER,SN,CS,RV)
C     SRT to calculate values for RV, SN, and CS, given a value of
C     PHV and a tabular ITH.	      
      INCLUDE 'COMA.FOR'
C     Save values in (I,1,1). Use (I,1,1) for temporary storage.
      SV1=ALM(I,1,1)   
      SV2=AMU(I,1,1)   
      SV3=ANU(I,1,1)     
      SV4=VR(I,1,1)    
      SV5=SNT(I,1)
C
      SN=DSIN(PHV)
      CS=DCOS(PHV)
      SNT(I,1)=SNT(I,ITH)     
      ANU(I,1,1)=ANU(I,ITH,1)     
    2 ALM(I,1,1)=SNT(I,1)*CS
      AMU(I,1,1)=SNT(I,1)*SN
C     Calculate tidally distorted radius at that PHV, Store in (I,1,1).
      CALL RCHER(I,1,1,ER)
	IF (FV(I).GT.1.D0) THEN
C     Get tidally distorted radius.
		RVTD=VR(I,1,1)
C     Reset polar radius for use by ROTDIST
		VR(I,1,1)=SV4
		AVL=0.5D0*(1.D0+Q)*FV(I)**2*(1.D0-ANU(I,1,1)**2)
		CALL ROTDIST(I,AVL,Q,ALM(I,1,1),SV,1.D-9)
C     Calculate amount of rotational distortion
		SVDF=SV-SV4
C     Calculate amount of tidal distortion
		RDDIFF=RVTD-SV4
C     Calculate combined tidal and rotational distortion
		RDCORR=SVDF+RDDIFF
C     Calculate corrected radius
		IF (RDCORR.GT.0.D0) THEN
			RV=VR(I,1,1)+RDCORR
		ELSE
			RV=SV4
		END IF
	ELSE    
		RV=VR(I,1,1)
	END IF     
      ALM(I,1,1)=SV1   
      AMU(I,1,1)=SV2   
      ANU(I,1,1)=SV3     
      VR(I,1,1)=SV4    
      SNT(I,1)=SV5
      RETURN     
      END 
