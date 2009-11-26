      SUBROUTINE FIFR(I,ITH,NTH,K,L,X,Y,Z,F,FX,RFL,SEPCM2,NOUT5)
C     SRT TO CALCULATE IRRADIATION EFFECT FOR A COMPONENT OF A CLOSE     
C     BINARY SYSTEM
C     I=Designation of star undergoing irradiation
C     K=Designation of point on star being irradiated where irradiation
C       is being evaluated.
C     FX=Incident flux of irradiation from companion at area element K,
C       output from FIFR
C     X,Y,Z=coordinates of point on irradiated star being evaluated for
C     irradiation.
C     RFL=correction factor to permit iteration of irradiative effect.
C     SEPCM2=square of separation of components, in cm^2.
C     L=Designation of irradiating star.
C     XA,YA,ZA=point on irradiating star
C     ITHA=designation of colatitude on irradiating star
C     F=Normal intensity at point ITHA, KA on irradiating star
C
      INCLUDE 'COMA.FOR'
      DIMENSION SMB(MPHM),VXI(MPHM)
	DIMENSION RFL(2,MTHM,MPHM)   
      DATA PI/3.1415926535898D0/ 
      DATA TWPI/6.2831853071796D0/
	DATA SGMA/5.6696D-5/
C     Note in the next section that we
C     have integrated over sine(theta). That is, we consider the polar
C     cap to be a small irradiating segment. The area of a polar cap
C     segment is 	TWPI*r^2*(1-cos(theta))*SEPCM2, where theta is the
C     polar angle of the cap edge.
C     XA, YA, ZA are the coordinates of a point on the photosphere of
C     the irradiating star.
C     Initialize flux
      FX=0.D0	    
C     Initialize SMB(1)
		SMB(1)=0.D0
		XA=VR(L,1,1)*ALM(L,1,1)
		YA=VR(L,1,1)*AMU(L,1,1)
		ZA=VR(L,1,1)*ANU(L,1,1)
C     The coordinates of the point on the irradiating star are XA,YA,ZA
C     RCP is the distance between the irradiating segment and the
C     irradiated segment. VLDC, VMDC, VNDC are the direction cosines of
C     a unit vector at the irradiating segment, directed toward the
C     irradiated segment.
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle
C     between the vector toward XA,YA,ZA and the local normal at X,Y,Z.
		CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
		IF (CSX.LT.0.D0) GO TO 48
C     Calculate the cosine of the angle between this vector and the
C     local normal at the polar cap of the irradiating star.
		CSNR=VLDC*ZL(L,1,1)+VMDC*ZM(L,1,1)+VNDC*ZN(L,1,1)
C     Only continue if CSNR is not negative.
		IF (CSNR.LT.0.D0) GO TO 48
C     There is an irradiation contribution only if CSX is not negative.
			CSXA=VLDC*ZL(L,1,1)+VMDC*ZM(L,1,1)+VNDC*ZN(L,1,1)
C     DAP is the area of the cap, projected normal to the connecting 
C     vector, in cm^2. 
			DAP=VR(L,1,1)*VR(L,1,1)*TWPI*(1.D0-ANU(L,2,1))*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
C     Calculate normal intensity from star L
			F=SGMA/PI*(TMPR(L,1,1)*1.0D3)**4
C     Calculate contribution to incident flux
			SMB(1)=F*DAP*(1.D0-U1(L,1,1)*(1.D0-CSXA)-
     $			U2(L,1,1)*(1.D0-CSXA)**2)/RCP**2/SEPCM2*CSX
     $			*RFL(L,1,1)
C
C     *****************************************************************
C     Now do lower polar cap.
C     Initialize SMB(MTH)
   48		SMB(NTH)=0.D0
		XA=VR(L,NTH,1)*ALM(L,NTH,1)
		YA=VR(L,NTH,1)*AMU(L,NTH,1)
		ZA=VR(L,NTH,1)*ANU(L,NTH,1)
		RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
		VLDC=(X-XA)/RCP
		VMDC=(Y-YA)/RCP
		VNDC=(Z-ZA)/RCP
C     Calculate cosine of angle between vector toward XA, YA, ZA at
C     X,Y,Z and local normal at X,Y,Z.
		CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
		IF (CSX.LT.0.D0) GO TO 49
C     Calculate the cosine of the angle between the vector toward X,Y,Z
C     and the local normal.
		CSNR=VLDC*ZL(L,NTH,1)+VMDC*ZM(L,NTH,1)+VNDC*ZN(L,NTH,1)
C     Only continue if CSNR is not negative.
		IF (CSNR.LT.0.D0) GO TO 49
			CSXA=VLDC*ZL(L,NTH,1)+VMDC*ZM(L,NTH,1)+VNDC*ZN(L,NTH,1)
			DAP=VR(L,NTH,1)*VR(L,NTH,1)*TWPI*(1.D0-ANU(L,NTH,1))
     $			*CSXA*SEPCM2
			IF (DAP.LT.0.D0) DAP=0.D0
C     Calculate normal intensity from star L
			F=SGMA/PI*(TMPR(L,NTH,1)*1.0D3)**4
C     Calculate contribution to incident flux
			SMB(NTH)=F*DAP*(1.D0-U1(L,NTH,1)*(1.D0-CSXA)-
     $		U2(L,NTH,1)*(1.D0-CSXA)**2)/RCP**2/SEPCM2*CSX
     $			*RFL(L,NTH,1)
C
C     *****************************************************************
C     Do the remainder of star L.
C
   49		VNTH=NTH-1
		DTH=PI/VNTH
C     Initialize SMB
		DO ITHA=2,NTH-1
			SMB(ITHA)=0.D0
		END DO
C     Loop over remainder of star L for the point X,Y,Z
		DO ITHA=2,NTH-1
			NA=NPH(L,ITHA)
			VN=NA-1
C     Calculate the phi increment, DOMG, in radians.
			DOMG=TWPI/VN
C     Initialize VXI
			DO KA=1,NA
				VXI(KA)=0.D0
			END DO
			DO KA=1,NA
C     Determine the coordinates of the photospheric segment on star L.
				XA=VR(L,ITHA,KA)*ALM(L,ITHA,KA)
				YA=VR(L,ITHA,KA)*AMU(L,ITHA,KA)
				ZA=VR(L,ITHA,KA)*ANU(L,ITHA,KA)
C     Calculate the separation of the irradiated point and the 
C     irradiating segment.
				RCP=DSQRT((X-XA)**2+(Y-YA)**2+(Z-ZA)**2)
C     Calculate direction cosines of vector directed from (XA,YA,ZA)
C     to (X,Y,Z).
				VLDC=(X-XA)/RCP
				VMDC=(Y-YA)/RCP
				VNDC=(Z-ZA)/RCP
C     Calculate the cosine of the angle between the vector, directed
C     toward XA,YA,ZA, and the surface normal at (X,Y,Z).
C     Note that we are at the point (X,Y,Z) and are looking back toward
C     the irradiating star, so we reverse the direction of the vector
C     connecting (XA,YA,ZA) and (X,Y,Z).
			   CSX=-VLDC*ZL(I,ITH,K)-VMDC*ZM(I,ITH,K)-VNDC*ZN(I,ITH,K)
C     If CSX.LT 0.D0, then no irradiation of X,Y,Z is possible for that
C     XA,YA,ZA.
			   IF (CSX.LT.0.D0) GO TO 50 
C     Calculate cosine of angle between vector toward X,Y,Z and the
C     local normal to point XA,YA,ZA.
				CSNR=VLDC*ZL(L,ITHA,KA)+VMDC*ZM(L,ITHA,KA)+VNDC*
     $				ZN(L,ITHA,KA)
C     Calculate the cosine of the angle between the vector and the
C     surface normal at (XA,YA,ZA).
					CSXA=VLDC*ZL(L,ITHA,KA)+VMDC*ZM(L,ITHA,KA)+VNDC*
     $					ZN(L,ITHA,KA)
C     Calculate irradiative contribution.
C     The integrand, for constant phi increment of DOMG, is VXI().
					DAP=VR(L,ITHA,KA)*VR(L,ITHA,KA)*SNT(L,ITHA)*
     $					CSXA*SEPCM2
					IF (DAP.LT.0.D0) DAP=0.D0
C     Calculate normal intensity from star L
					F=SGMA/PI*(TMPR(L,ITHA,KA)*1.0D3)**4
C     Calculate contribution to incident flux
					VXI(KA)=F*DAP*(1.D0-U1(L,ITHA,KA)*(1.D0-CSXA)
     $						-U2(L,ITHA,KA)*(1.D0-CSXA)**2)/
     $						RCP**2/SEPCM2*CSX*RFL(L,ITHA,KA)
   50				CONTINUE
C     End of loop for phi-wise stepping on irradiating star.
   			END DO
C     Perform phi-wise integration for one strip on irradiating star.
C     Store integrated results, as a function of the index ITHA, in
C     SMB(ITHA).
			CALL ITF(VXI,1,NA,DOMG,0.D0,0.D0,0,ER,SMB(ITHA),
     $					  NSG,KRT)
C     End of loop for theta-wise stepping on irradiating star.
		END DO
C
C     *****************************************************************
C     Perform theta-wise integration with theta increment DTH.
C     Store integrated result for photospheric element at X,Y,Z in CSTFX
		CALL ITF(SMB,1,NTH,DTH,0.D0,0.D0,0,ER,FX,NTH,KRT)
		IF (FX.LT.0.D0) FX=0.D0
      RETURN
       END 
