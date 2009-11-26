      SUBROUTINE FIRM(TMPRR,VMU,I,NTH,BALB,VMUD,NRM,NSG,NOUT1)
	INCLUDE 'COMA.FOR'
	INCLUDE 'ACOMA.FOR'
	DIMENSION TMPRR(MRM,MSG)
      DIMENSION DSG2(MTW,MTHM,MPHM)
	DIMENSION XVL(MTW,MTHM,MPHM),YVL(MTW,MTHM,MPHM),ZVL(MTW,MTHM,MPHM)
	DIMENSION VXARP(MRM,MSG)
C     SGMA= Stefan-Boltzmann radiation constant.
	DATA SGMA/5.6696D-5/
C     CF=3.0/PI,normalization constant for first order limb darkening.
	DATA CF/0.9549296585D0/
C     Displace coordinates of accretion disk rim to center of mass.
C     The coordinates of a rim grid point, relative to the center of
C     the accretion disk, are VXAR, VYAR, VZAR.
C     The center of the accretion disk, in center of mass coods., is
C     at 1.0-VMU, 0.0, 0.0.
C     The X coods. of a rim grid point are at VXARP.
	DO J=1,NRM
		DO K=1,NSG
			VXARP(J,K)=VXAR(J,K)+1.D0-VMU
		END DO
	END DO
C     Calculate the complete grid of points covering the companion.
      NTS=0
C     The X axis at the companion points toward the center of the
C     accretion disk.
C     PH() is the equatorial plane azimuth of the point, measured CCW
C     looking down from above the upper pole.
C     VR(I,ITH,K) is the radius vector to the grid point.
C     ALM, AMU, and ANU are the direction cosines of the radius vector.
C     ZL, ZM, and ZN are the direction cosines of the surface
C     normal at the grid point.
C
C     Calculate the center of mass coods. of the points on the 
C     companion star.      
	DO ITH=1,NTH
		N=NPH(I,ITH)
		DO K=1,N
			XVL(I,ITH,K)=VR(I,ITH,K)*ALM(I,ITH,K)-VMU
			YVL(I,ITH,K)=VR(I,ITH,K)*AMU(I,ITH,K)
			ZVL(I,ITH,K)=VR(I,ITH,K)*ANU(I,ITH,K)
		END DO
	END DO
C     Starting at central meridian and upper edge of rim, calculate
C     distances and direction cosines of lines joining a point on the
C     accretion disk rim and points on the companion star.
C     Also calculate the cosine of the angle between a given line and
C     the surface normal at the accretion disk.
C     If the included angle is less than PI, continue with the 
C     calculation.
C     NOTE.  The positive X axis points away from the companion star at
C     the accretion disk. The points in arrays VXARP, VYAR, VZAR go 
C     around a complete circle for each azimuthal profile. Then the
C     point on a given circle closest to the companion is at the
C     midpoint of the circle index. Locate that point with pointer
C     KHF. The indexer relative to the start of a circle is KRF.
      DO J=1,NRM
		KHF=NSG/2+1
		DO K=1,NSG
C     Initialize accumulator for incoming flux on accretion disk.
		SMFLX=0.D0
		KRF=KHF+K-1
C     Remember that the first point in the array duplicates NSG
		IF (KRF.GT.NSG) KRF=KRF-NSG+1
C     Obtain coods. of a point on the accretion disk, starting with
C     the point closest to the companion star.
			X1=VXARP(J,KRF)
			Y1=VYAR(J,KRF)
			ZZ1=VZAR(J,KRF)
C     Now cycle over the entire companion star.
			DO ITH=1,NTH
			IF (ITH.EQ.1) DSNTH=0.D0
			IF (ITH.GT.1) DSNTH=DABS(SNT(I,ITH)-SNT(I,ITH-1))
				N=NPH(I,ITH)
				DO KK=1,N
C     Obtain the coods. of a grid point on the companion.
					X2=XVL(I,ITH,KK)
					Y2=YVL(I,ITH,KK)
					ZZ2=ZVL(I,ITH,KK)
C     Determine differences of X, Y, and Z coods.
					DX=X2-X1
					DY=Y2-Y1
					DZ=ZZ2-ZZ1
C     Calculate distance between the two points, DST.
					DST=DSQRT(DX**2+DY**2+DZ**2)
C     Calculate direction cosines, at the accretion disk, of the
C     line connecting the two points.
C     Note that the positive X axis points from the companion to the 
C     center of the accretion disk.
					VLAM=DX/DST
					VLMU=DY/DST
					VLNU=DZ/DST
C     Calculate cosine of angle between connecting line and the local
C     normal to the accretion disk, at the accretion disk.
					CSGM1=VLAM*ZLAR(J,KRF)+VLMU*ZMAR(J,KRF)+
     $						VLNU*ZNAR(J,KRF)
C	Calculate incoming flux from the associated surface element.
C     Set the direction cosines for the opposite direction, to give
C     the direction of the connecting line at the companion star 
C     grid point.	Note that the directions of the X and Y axes also
C     reverse.
					VLAMP=-VLAM
					VLMUP=-VLMU
					VLNUP=-VLNU
C     Calculate the cosine of the angle between the connecting line and
C     the local surface normal on the companion star.
					CSGM2=VLAMP*ZL(I,ITH,KK)+VLMUP*ZM(I,ITH,KK)+
     $					VLNUP*ZN(I,ITH,KK)
C	Calculate element of area at companion.
					IF (KK.EQ.1) DPHP=0.D0
					IF (KK.GT.1) DPHP=PH(I,ITH,KK)-PH(I,ITH,KK-1)
					DSG2(I,ITH,KK)=VR(I,ITH,KK)*VR(I,ITH,KK)/
     $					CSB(I,ITH,KK)*SNT(I,ITH)*DPHP*DSNTH
					IF (CSGM1.GT.0.D0) THEN
						IF (CSGM2.GT.0.D0) THEN
C     Determine normal intensity at grid point.
							TMP=TMPR(I,ITH,KK)*1.0D3
							FLX=SGMA*TMP**4
C     The bolometric limb darkening on the companion star rim is VMUD.
C     VINRM is the bolometric intensity normal to the local photosphere.
							VINRM=CF*FLX/(3.D0-VMUD)
C     DFLX is the flux contribution per unit irradiated area from the
C     area element on the companion star.
							DFLX=(1.D0-VMUD+VMUD*CSGM2)*
     $							VINRM*DSG2(I,ITH,KK)/DST/DST*CSGM2
C     Multiply by the cosine of the angle between the connecting line
C     and the local normal to the accretion disk rim to get the 
C     incoming flux per unit area on the accretion disk rim.
C     SMFLX accumulates the flux received from the companion star, per
C     unit area on the accretion disk rim, at the rim point currently
C     considered.
							SMFLX=SMFLX+DFLX*CSGM1
C      
						END IF
					END IF
				END DO

			END DO
C     End of integration over companion for a given area element on
C     accretion disk rim.
C     EMFLX, initially, is the flux emerging from the rim without
C     irradiation.
			EMFLX=SGMA*TMPRR(J,K)**4
			EMFLXP=EMFLX
C     Add the reemitted flux due to irradiation.
C     BALB is the bolometric albedo of the accretion disk rim.
			EMFLX=EMFLX+BALB*SMFLX
C     Calculate the modified effective temperature to include
C     reemitted flux due to irradiation.
			TMPRR(J,K)=(EMFLX/SGMA)**0.25D0
		END DO
	END DO
	RETURN
	END