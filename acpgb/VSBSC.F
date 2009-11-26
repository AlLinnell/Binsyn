      SUBROUTINE VSBSC(I,DX,DY,STX,STY,PXF,PYF,PXR,PYR,AGLD,RD,STVS,
     $XRMC,PRFH,CSGR,MTH,NSG,NRM,DDX,DDY,ISTAR,
     $ISV,NOUT7)
C     Subroutine to set visibility data for central star face,
C     based on view blocking by accretion disk.
C     NSG=number of division points on a given ring of accretion disk.
C     NRING=number of ring division points on face of accretion disk.
C     XRMC=the projected distance between the center of the ellipse
C	    representing the projected bottom edge of the accretion
C		disk and the projected center of the gainer.
C     Note that the X cood. is measured along the projected rotation
C         axis
C         of the central star (and the rotation axis of the accret.
C         disk).
C     YMX=maximum sideways extent of projected innermost face circle.
C     XREF=X cood.corres. to max sideways extent of the projected
C         innermost face circle.
C     XLSL=smallest X cood. on the lower rim of the accretion disk,
C          relative to the projected center of the gainer.
C     XNIM=the relative X cood. of the uppermost projected point on
C          the innermost accretion disk face circle. The X cood. is
C          relative to the projected center of the accretion disk.
C     RDMIN=projected distance along rotation axis of central star from
C         projected center of lower rim to upper projected intersection
C         of rotation axis and lower rim of accret. disk.
C     RDMX=projected distance from center of lower rim to max. sideways
C         extent of projection of lower rim.
C     PXR,PYR= X and Y coods. of a point on the accret. disk rim.
C     PXF,PYF= X and Y coods. of a point on the lower accret. disk face.
C     STX,STY= X and Y coods. of a point on the central star face.
C         Note that the coods. have been displaced from the origin if
C         the central star is the eclipsing object (in ACPGB).
C     RD=array of projected distances of points on the lower rim of
C         accret. disk from projected center of lower accret. disk rim.
C     AGL=corresponding array of position angles of lower rim points.
C     RVL=array of projected radii of the upper rim of the accret. disk.
C         Radii measured from proj. center of central star.
C     ANG=corresponding array of position angles of upper rim points.
C     RHO=array of projected distances from projected center of lower
C         rim of accret. disk to points on the face of the central star.
C     AGL2=corrersponding array of position angles.
C     PRFH=array of visibility keys for face of accret. disk.
C     PRSH=array of visibility keys for central star boundary.
C     STVS=array of visibility keys for central star face points.
C     VI=orbital inclination
C     IFLAG designates the first rim ring which has at least one rim
C     point visible to the observer.
	INCLUDE 'COMA.FOR'	
	INCLUDE 'ACOMA.FOR'
      DIMENSION PXF(MRING,MSG),PYF(MRING,MSG),STX(MTW,MTHM,MPHM),
     $STY(MTW,MTHM,MPHM),
     $STVS(MTW,MTHM,MPHM),AGLD(MSG),
     $RD(MSG),ZX(MTR),ZR(MTR),PXR(MRM,MSG),PYR(MRM,MSG),
     $ANG(MSG),ANGL(MSG),RDS(MSG),PRFH(MRING,MSG),CSGR(MRM,MSG),
     $RVLP(MSG),ANGP(MSG),RVL(MSG)
	DATA PI/3.141592653589793238463D0/
	DATA PIO2/1.5707963267948966192D0/
	DATA TWPI/6.2831853071795864769D0/
      IF (NSG.EQ.0) RETURN
      NTH=(MTH+1)/2
C     Find XLL, the center of mass X cood. of the top rim point with
C     the most negative Y cood., YLL.
C     Do the same for YUL, XUL on the lower rim.
      YLL=10.D0
	YUL=-10.D0
      DO J=1,NSG
		IF (PYR(1,J).LT.YLL) THEN
			YLL=PYR(1,J)
			XLL=PXR(1,J)
		END IF
		IF (PYR(NRM,J).GT.YUL) THEN
			YUL=PYR(NRM,J)
			XUL=PXR(NRM,J)
		END IF
	END DO
C
C     Find YMX and XREF
C     PYF and PXF are projected coods.of points on lower face
C        of accretion disk. The coods. are in the center of mass
C        system.
C     Find the point on the lower rim of the 	accretion disk which
C     is farthest from the projected center of the accretion disk in
C     the negative X direction. XLSL is the (negative) X distance
C     from the projected center.
      YMX=-1.0D0
	DO J=1,NSG
	IF (ISTAR.EQ.ISV) THEN
		X=PXR(NRM,J)+DX
		Y=PYR(NRM,J)+DY
	ELSE
		X=PXR(NRM,J)+DDX
		Y=PYR(NRM,J)+DDY
	END IF
	IF (Y.GT.YMX) YMX=Y
	END DO
C     YMX now has the Y cood., relative to the projected center of the
C     accretion disk component, that is most positive.
C     Calculate the corresponding X distance and place in XLSL
C     Note that the X cood. of the center of the projected lower rim
C     is at XUL.
      XLSL=-YMX*DCOS(VI)	       
C     Determine the maximum sideways extent of the central star.
C     Place in YMX. Determine the corresponding projected distance
C     to the uppermost equatorial point, relative to the projected
C     center of the gainer. Place this distance in XREF.
      YMX=-1.0D0
	ITH=NTH
	N=NPH(I,ITH)
      DO 55 J=1,N
C     Calculate X,Y projected coods. of given point relative to the
C        projected center of the accretion disk. STY values are relative
C        to the center of mass.
      IF (ISTAR.EQ.ISV) THEN
      Y=STY(I,ITH,J)+DY
      ELSE
      Y=STY(I,ITH,J)+DDY
      END IF
      IF (DABS(Y).LT.YMX) GO TO 55
      YMX=DABS(Y)
   55 CONTINUE
      XREF=0.D0
C
C     XREF now contains the relative X cood. corres. to the maximum
C     sideways extent of the gainer equator, relative to the
C     projected center of the central star.
C
C     Find XNIM
      XNIM=-YMX*DCOS(VI)
C     XNIM now contains the relative X cood. of the uppermost
C     projected point on the innermost face circle. This point coincides
C     with the most negative (X direction) projected point on the equator
C     of the central star, This assumes the innermost face circle coincides
C     with the equator of the central star.The corresponding relative
C     Y cood.is 0.0, from the proj.center of the central star.
C
C
C     Find extreme values of RD.
C     The minimum RD corresponds to point on the rotation
C     axis of the central star that intersects the projected lower
C     rim. The maximum is the max. projected sideways extent.
C     Note that the RD points sequence by scanning the rim in the CW
C     sense, starting at a position angle equal to the orbital
C     longitude.
      RDMN=5.0D0
C     Choose the one of the two intersections of the rotation axis and
C         the projected lower rim which is in the (-) X direction
C         (i.e., toward the hidden N. pole of the central star).
      DO 2 J=1,NSG
      IF (ISTAR.EQ.ISV) THEN
      X=PXR(NRM,J)+DX
      ELSE
      X=PXR(NRM,J)+DDX
      END IF
      IF ((RD(J).GT.RDMN).OR.(X.GT.XRMC)) GO TO 2
	XMNV=PXR(NRM,J)
      RDMN=RD(J)
   2  CONTINUE
C     RDMN now contains the projected distance from the center of
C     the projected lower rim to the
C     closest point on the lower rim that is in the (-) X direction.
C     XMNV is the center of mass X-coordinate of the point on the lower
C     rim.
      RDMX=-5.0D0
      DO 3 J=1,NSG
      IF (RD(J).LT.RDMX) GO TO 3
      RDMX=RD(J)
    3 CONTINUE
C     NOTE: The above procedure for RDMN is inaccurate if NSG is
C     less than about 400. The following procedure is accurate.
C     We maintain the above procedure to preserve the value of XMNV.
      RDMN=RDMX*DCOS(VI)
C     RDMX now contains the max. projected sideways extent of the
C     lower rim.
C     YMX now contains the the maximum sideways
C     extent of the projected innermost face circle.
C     Test whether the projected uppermost point of disk lower rim
C        (at X cood. XRMC-RDMN)
C     lies above or below the uppermost projected point on the upper
C     branch of the innermost face circle, at X cood. XNIM.
C     (Explanation follows)
C     The relative coods. of the projected center of the lower outer
C     rim are in XRMC, 0.0, actual CM coods. in XRMC+DX, DY.
C     The uppermost projected point on the lower outer rim is the
C     point with the numerically smallest X cood., XRMC-RDMN.
C     RDMN is measured from the projected center of the lower rim.
C     The relative X cood. of that center is XRMC, and it always is
C         positive.
C     The uppermost projected point on the upper branch of the
C     innermost face circle has relative X cood. XNIM.
      IF ((XRMC-RDMN)-XNIM.LT.0.D0) GO TO 1
C     Arrive here if disk lower rim sets visibility limits of south
C     latitude visible segment of central star
C     Scan face points of gainer, latitude profile by latitude profile
      DO 10 ITH=1,MTH
      N=NPH(I,ITH)
         DO 9 K=1,N
C     Exclude consideration of points on the gainer that are above the 
C     most negative
C     X cood. projected point on the lower rim.
		IF (STX(I,ITH,K)-XLL.LT.0.D0) GO TO 9 
C     Check whether back side invisibility key has been set
         IF (STVS(I,ITH,K).LE.0.0D0) GO TO 9
C     Arrive here if point is on visible face of gainer.
C     Note that X,Y must be relative to center of component I.
         IF (ISTAR.EQ.ISV) THEN
         X=STX(I,ITH,K)+DX
         Y=STY(I,ITH,K)+DY
         ELSE
         X=STX(I,ITH,K)+DDX
         Y=STY(I,ITH,K)+DDY
         END IF
C     Test for case of point on star lying between XLL and (XUL-XLSL).
C     Restrict to case of large inclination. Note that XLSL is a
C     distance, and is negative.
		IF (VI.GT.1.53) THEN
			IF ((X.LT.(XUL+XLSL)).AND.(X.GT.XLL)) THEN
				STVS(I,ITH,K)=-1.D0
				GO TO 9
			END IF
		END IF
C     Begin procedure to locate the possible intersections of the star
C        disk and the projected lower rim.
C     XPM is X cood. of gainer face point, relative to X cood. of
C     center of projected lower rim.
          XPM=X-XRMC
          RHOV=DSQRT(XPM*XPM+Y*Y)
          IF (DABS(Y).GT.1.0D-12) GO TO 14
          IF ((DABS(Y).LT.1.0D-12).AND.(K.EQ.N)) GO TO 14
          IF (XPM.LT.0.D0) AGL2=0.D0
          IF (XPM.GE.0.D0) AGL2=PI
          GO TO 15
   14     AGL2=DASIN(DABS(Y)/RHOV)
          IF ((Y.LT.0.D0).AND.(XPM.LT.0.D0)) GO TO 15
          IF ((Y.LT.0.D0).AND.(XPM.GE.0.D0)) AGL2=PI-AGL2
          IF ((Y.GE.0.D0).AND.(XPM.GE.0.D0)) AGL2=AGL2+PI
          IF ((Y.GE.0.D0).AND.(XPM.LT.0.D0)) AGL2=TWPI-AGL2
   15     CONTINUE
C     AGL2 is position angle of point on latitude profile of star
C     AGL is array giving position angles of accret. disk rim points
C     relative to projected center of rim.
         DO J=1,NSG-1
         LPCT=0
         IF (DABS(AGL2-AGLD(J)).LT.1.D-8) GO TO 18
         IF ((AGL2.GT.AGLD(J)).AND.(AGL2.LT.AGLD(J+1))) GO TO 16
         END DO
	   J=NSG
         IF (DABS(AGL2-AGLD(NSG)).LT.1.D-9) GO TO 18
	   IF ((AGL2.GT.AGLD(NSG)).AND.(AGL2.LT.AGLD(1))) GO TO 70
		ANGMX=-1.D0
		ANGMN=10.D0
		DO J=1,NSG
			IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
			IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
		END DO
		DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 220
		END DO
		CALL PMDSTOP
  220		IF (J.GT.1) THEN
		IF ((AGLD(J).LT.AGL2).AND.(AGLD(J+1).LT.AGL2)) THEN
			ZR(1)=RD(J-1)
			ZR(2)=RD(J)
			ZR(3)=RD(J+1)
			ZX(1)=AGLD(J-1)
			ZX(2)=AGLD(J)
			ZX(3)=AGLD(J+1)+TWPI
			GO TO 71
		END IF 
		END IF
		IF (J.EQ.1) THEN
		IF ((AGLD(J).LT.AGL2).AND.(AGLD(J+1).LT.AGL2)) THEN
			ZR(1)=RD(J)
			ZR(2)=RD(J+1)
			ZR(3)=RD(J+2)
			ZX(1)=AGLD(J)
			ZX(2)=AGLD(J)+TWPI
			ZX(3)=AGLD(J+2)+TWPI
			GO TO 71
		END IF 
		END IF
		DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 221
		END DO
		CALL PMDSTOP
  221		IF (J.GT.1) THEN
		IF ((AGLD(J).GT.AGL2).AND.(AGLD(J+1).GT.AGL2)) THEN
			ZR(1)=RD(J-1)
			ZR(2)=RD(J)
			ZR(3)=RD(J+1)
			ZX(1)=AGLD(J-1)-TWPI
			ZX(2)=AGLD(J)-TWPI
			ZX(3)=AGLD(J+1)
			GO TO 71
		END IF
		END IF
		IF (J.EQ.1) THEN
		IF ((AGLD(J).GT.AGL2).AND.(AGLD(J+1).GT.AGL2)) THEN
			ZR(1)=RD(J)
			ZR(2)=RD(J+1)
			ZR(3)=RD(J+2)
			ZX(1)=AGLD(J)-TWPI
			ZX(2)=AGLD(J+1)
			ZX(3)=AGLD(J+2)
			GO TO 71
		END IF 
		END IF
		CALL PMDSTOP
   70    ZR(1)=RD(NSG)
         ZR(2)=RD(1)
C     Provide for repeat of end point.
	   ZR(3)=RD(3)
	   ZX(1)=AGLD(NSG)
	   ZX(2)=AGLD(1)
	   ZX(3)=AGLD(3)
	   IF ((ZX(1).LT.ZX(2)).AND.(ZX(2).LT.ZX(3))) GO TO 71
	   ZX(3)=ZX(3)+TWPI
	   GO TO 71
   18    RR=RD(J)
         GO TO 17
   16    M=2
         IF (J.EQ.1) M=1
C     Set 3-point arrays of rim radii and rim position angles,
C     bracketing the position angle of the star circumference point.
   28    DO L=1,3
         ZX(L)=AGLD(J-M+L)
         ZR(L)=RD(J-M+L)
         END DO
C     Check range of ZX
	   IF ((ZX(1).LT.ZX(2)).AND .(ZX(2).LT.ZX(3))) THEN
			GO TO 71
		END IF
	   IF ((ZX(1).GT.ZX(3)).AND.(M.EQ.2)) THEN
			IF (J.LT.NSG-1) THEN
				M=1
				GO TO 28
			END IF
		END IF
		IF (J.EQ.1) THEN
			ZX(3)=ZX(3)+TWPI
			IF ((ZX(1).LT.ZX(2)).AND.(ZX(2).LT.ZX(3))) GO TO 71
		END IF
		IF (J.EQ.NSG-1) THEN
			ZX(1)=AGLD(NSG-1)
			ZX(2)=AGLD(NSG)
			ZX(3)=AGLD(1)
			ZR(1)=RD(NSG-1)
			ZR(2)=RD(NSG)
			ZR(3)=RD(1)
			IF ((ZX(1).LT.ZX(2)).AND.(ZX(2).LT.ZX(3))) GO TO 71
			ZX(3)=ZX(3)+TWPI
			GO TO 71
		END IF
	CALL PMDSTOP
C
C     Interpolate in rim radii for rim radius corres. to position angle
C     of star circumference point.
   71    CALL INTEP(AGL2,RR,ZX,ZR,3,IER)
         IF (IER.EQ.1) GO TO 17
         RR=ZR(3)
         DF=ZX(3)-AGL2
         IF (DABS(DF).LT.0.25D0) GO TO 17
         M=M+3
         LPCT=LPCT+1
         IF ((LPCT.LE.1).AND.((J-M+1).GT.0)) GO TO 28
         DF=ZX(2)-AGL2
         IF (DABS(DF).GT.0.25) GO TO 29
         RR=ZR(2)
         GO TO 17
   29    DF=ZX(1)-AGL2
         IF (DABS(DF).GT.0.25D0) CALL PMDSTOP
         RR=ZR(1)
C     RR is radius to point on rim relative to projected center of
C     rim profile.
C     RHOV is radius to point on star face at same position angle
C     as RR and relative to same point as RR.
C     If ER is (+), we are inside rim projection, and the
C     star is visible.
   17    ER=RR-RHOV
         STVS(I,ITH,K)=ER
C     If the point is above XLSL, the rim hides it
         IF ((X.LT.XLSL).AND.(STVS(I,ITH,K).GT.0.D0)) 
     $	STVS(I,ITH,K)=-1.D0
C      An exception is if the star disk extends above the back edge of
C      the projected rim. In this case, all of that part of the star
C      is visible.
         IF (X.GT.XRMC) STVS(I,ITH,K)=1.D0
    9 CONTINUE
   10 CONTINUE
      GO TO 11
C
C**********************************************************************
C     Arrive here if disk lower rim lies above XNIM. All of central
C     star is visible to inner ring of accretion disk.
    1 CONTINUE
C
C     Fill arrays ANGL and RDS with representation of inner boundary of
C     accretion disk
C     The X,Y coods. of the bounding profile are in PXF(1,K), PYF(1,K).
C     Must measure distances from (XRMC, 0.0).
      KCT=0
      DO K=1,NSG
C     PRFH contains visibility keys for face of accretion disk.
C     PRFH(1,K) is for innermost circle, set in SRT VSB.
      IF (PRFH(1,K).LT.0.D0) GO TO 32
      KCT=KCT+1
      IF (ISTAR.EQ.ISV) THEN
      X=PXF(1,K)+DX
      Y=PYF(1,K)+DY
      ELSE
      X=PXF(1,K)+DDX
      Y=PYF(1,K)+DDY
      END IF
      XPM=X-XRMC
      RDS(KCT)=DSQRT(XPM*XPM+Y*Y)
      IF (DABS(Y).GT.1.0D-12) GO TO 34
      IF ((DABS(Y).LT.1.0D-12).AND.(K.EQ.NSG)) GO TO 34
      IF (XPM.LT.0.D0) AGL2=0.D0
      IF (XPM.GE.0.D0) AGL2=PI
      GO TO 33
   34 AGL2=DASIN(DABS(Y)/RDS(KCT))
      IF ((Y.LT.0.D0).AND.(XPM.LT.0.D0)) GO TO 33
      IF ((Y.LT.0.D0).AND.(XPM.GE.0.D0)) AGL2=PI-AGL2
      IF ((Y.GE.0.D0).AND.(XPM.GE.0.D0)) AGL2=AGL2+PI
      IF ((Y.GE.0.D0).AND.(XPM.LT.0.D0)) AGL2=TWPI-AGL2
   33 CONTINUE
      ANGL(KCT)=AGL2
   32 END DO
C     Make sure ANGL sequences properly.
      DO J=1,KCT-1
      IF (ANGL(J+1).LT.ANGL(J)) ANGL(J+1)=ANGL(J+1)+TWPI
      END DO
      IF (ANGL(KCT).LT.ANGL(KCT-1)) ANGL(KCT)=ANGL(KCT)+TWPI
C     Now test for visibility of an upper segment of star disk
C     The arrays that follow, RVL and ANG, represent polar coods. of
C     the top rim relative to the origin at the projected center.
C     The direction of the point listing is toward the left, starting
C     at the center point on the projected top rim. 
C     Test for whether any part of
C     the star projects above the top rim.
C     RBP is the polar radius of the spherical star.
   11 LMN=1
C     IFLAG designates the first rim ring which has at least one rim
C     point visible to the observer.
      IFLAG=0
	ITST=0
	DO K=1,NRM
		IF (IFLAG.EQ.0) THEN
			DO J=1,NSG
				IF (CSGR(K,J).GE.0.D0) ITST=ITST+1
			END DO
			IF (ITST.GT.0) IFLAG=K
		END IF
	END DO
      PYRM=2.0D0
C     Fill arrays RVL() and ANG() with points on the topmost
C     visible rim ring.
      DO L=1,NSG
		IF (ISTAR.EQ.ISV) THEN
			X=PXR(IFLAG,L)+DX
			Y=PYR(IFLAG,L)+DY
		ELSE
			X=PXR(IFLAG,L)+DDX
			Y=PYR(IFLAG,L)+DDY
		END IF
		RVL(L)=DSQRT(X**2+Y**2)
		ANG(L)=DASIN(Y/RVL(L))
    		IF (CSGR(IFLAG,L).LT.0.D0) THEN
			GO TO 52
	    ELSE
			IF (RVL(L)-PYRM.LT.0.D0)THEN
				PYRM=RVL(L)
				LMN=L
			END IF
	    END IF
   52		CONTINUE
      END DO
C     We have found the minimum value of PYR. This will also correspond
C     with the point on the upper rim closest to the origin. The value
C     of that distance will be RVL(LMN).
C     Reset the position angles so they start at the minimum value of
C     RVL.
      KK=1
      IF (LMN.EQ.NSG) KK=0
      IF (LMN.EQ.NSG) GO TO 60
      DO J=LMN,NSG
		RVLP(J-LMN+1)=RVL(J)
		ANGP(J-LMN+1)=ANG(J)
      END DO
   60 DO J=1,LMN-1
		RVLP(J+NSG-LMN+KK)=RVL(J)
		ANGP(J+NSG-LMN+KK)=ANG(J)
      END DO
      LMT=(NSG+1)/4
C     Now set visibility keys on face of central star
C     Cannot go past equator of star, so upper limit of ITH=NTH
      DO 49 ITH=1,NTH
		N=NPH(I,ITH)
          DO K=1,N
			IF (STVS(I,ITH,K).LT.0.D0) GO TO 48
			LPCT=0
			IF(ISTAR.EQ.ISV) THEN
				X=STX(I,ITH,K)+DX
				Y=STY(I,ITH,K)+DY
			ELSE
				X=STX(I,ITH,K)+DDX
				Y=STY(I,ITH,K)+DDY
			END IF
C     X,Y are relative to the center of the gainer.
C     RHOV is the radius of the projected stellar disk at X,Y.
			RHOV=DSQRT(X*X+Y*Y)
			IF (DABS(Y).GT.1.D-12) GO TO 42
			IF ((DABS(Y).LT.1.D-12).AND.(K.EQ.N)) GO TO 42
			IF (X.LT.0.D0) AGL2=0.D0
			IF (X.GE.0.D0) AGL2=PI
			GO TO 43
   42			AGL2=DABS(DASIN(Y/RHOV))
   43			CONTINUE
C     AGL2 is the position angle of point X,Y relative to the projected
C     polar axis of the gainer. ANGP(J) are position angles of points on
C     the upper accretion disk rim.
          DO J=1,LMT
		IF (J.EQ.1) THEN
			IF (AGL2.LT.DABS(ANGP(J))) GO TO 44
		END IF
          IF (DABS(AGL2-DABS(ANGP(J))).LT.1.0D-12) GO TO 44
C         ANGP is in ascending order
          IF ((AGL2.GT.DABS(ANGP(J))).AND.(AGL2.LT.DABS(ANGP(J+1)))) 
     $	GO TO 45
    8     END DO
C         If we reach here, we have the maximum RR possible for a point
C         on the rim, to remain within the position angle limits.
   44     RR=RVLP(J)
          GO TO 46
   45     M=2
          IF (J.EQ.1) M=1
          IF (DABS(ANGP(J-1)-ANGP(J)).GT.PIO2) M=1
          IF (J.EQ.NSG) M=3
   47     DO L=1,3
          ZX(L)=DABS(ANGP(J-M+L))
          ZR(L)=RVLP(J-M+L)
          END DO
C     RR is the polar radius to the rim point at the same position
C     angle as the star face point in question.
C     RHOV is the polar radius to the star face point in question.
          CALL INTEP(AGL2,RR,ZX,ZR,3,IER)
          IF (IER.EQ.1) GO TO 46
          RR=ZR(3)
          DF=ZX(3)-AGL2
          IF (DABS(DF).LT.0.25D0) GO TO 46
          M=M+3
          LPCT=LPCT+1
          IF ((LPCT.LE.1).AND.((J-M+1).GT.0)) GO TO 47
          CALL PMDSTOP
   46     ER=RHOV-RR
          STVS(I,ITH,K)=ER
   48     END DO
   49 CONTINUE
   51 CONTINUE
C     Do final purge to omit possible single visible points on each
C     half of a theta profile.
      DO ITH=1,NTH
      N=NPH(I,ITH)
      DO 13 K=1,N
      IF (N.EQ.1) GO TO 13
      IF (STVS(I,ITH,K).LT.0.D0) GO TO 13
C     If arrive here, STVS(I,ITH,K) is positive
      IF ((K.EQ.1).AND.(STVS(I,ITH,K+1).GT.0.D0)) GO TO 13
      IF (K.EQ.1) GO TO 20
      IF (STVS(I,ITH,K+1).GT.0.D0) GO TO 13
C     If arrive here, STVS(I,ITH,K) is (+), STVS(I,ITH,K+1) is (-),
C      and K.NE.1
      IF (STVS(I,ITH,K-1).GT.0.D0) GO TO 13
C     If arrive here, STVS(I,ITH,K) is a single (+) value between
C      (-) values
      STVS(I,ITH,K)=-0.5D0
      GO TO 13
C     If arrive here, the initial STVS(I,ITH,K) value is (+) and
C     the remaining
C     half theta profile points are (-)
C     Check the symmetric point on the other half theta profile
   20 IF (STVS(I,ITH,N).LT.0.0) GO TO 13
      STVS(I,ITH,1)=-0.5D0
      STVS(I,ITH,N)=-0.5D0
   13 CONTINUE
      END DO
   50 RETURN
      END
