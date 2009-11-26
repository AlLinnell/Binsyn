      SUBROUTINE VSBSD(I,STX,STY,PXF,PYF,PXR,PYR,AGLD,RD,STVS,
     $XRMC,YRMC,PRFH,CSGR,NTH,NSG,NRING,NRM,PRRH,IEC,
     $IRCS,IFCS,NKPS,
     $XG,YG,RXHZL,RYHZL,RXHZG,RYHZG,
     $PRVFG,PRVFL,RVL,ANL,XUL,XLL,YUL,YLL,DX,DY,DDX,DDY,ISTAR,ISV,ISEQ,
     $NTYPE,IFLAG,MRM,MSG,MRING,NOUT7)
C     Subroutine to set visibility keys in PRRH (complete rim of
C     acc. disk), PRFH (lower face of acc. disk), PRVFG (one vertical
C     edge of acc. disk, plotted separately), PRVFL (other vertical
C     edge of acc. disk, plotted separately), due to eclipse by
C     companion star.
C     Also set visibility keys in STVS (face of either star), due to
C     eclipse of central star by companion and also due to eclipse of
C     companion by combination of central star and accretion disk.
      INCLUDE 'COMA.FOR'
      DIMENSION PXF(MRING,MSG),PYF(MRING,MSG),STX(MTW,MTHM,MPHM),
     $STY(MTW,MTHM,MPHM),
     $STVS(MTW,MTHM,MPHM),RD(MSG),PXR(MRM,MSG),PYR(MRM,MSG),AGLD(MSG),
     $PRFH(MRING,MSG),CSGR(MRM,MSG),PRRH(MRM,MSG),
     $ANL(MSG),RXHZL(MRM),RYHZL(MRM),RMGX(MRM),RMGY(MRM),RMLX(MRM),
     $RMLY(MRM),RVL(MSG),
     $PRVFG(MRM),PRVFL(MRM),RXHZG(MRM),RYHZG(MRM),RDRY(3),AGRY(3)
      DIMENSION AR3(MTR),AR2(MTR),XGRY(MTN),YGRY(MTN),XLRY(MTN),
     $YLRY(MTN)
      DIMENSION DV(20)
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
      IF (NRM.EQ.0) RETURN
      RMN=2.D0
      DO J=1,IFCS
      IF (RSS(J,1).LT.RMN) RMN=RSS(J,1)
      END DO
      DO J=1,IRCS
      IF (RSS(J,2).LT.RMN) RMN=RSS(J,2)
      END DO
C     RMN now contains the smallest projected radius of the eclipsing
C        star at the current orbital longitude.
      RMX=0.D0
      DO J=1,IFCS
      IF (RSS(J,1).GT.RMX) RMX=RSS(J,1)
      END DO
      DO J=1,IRCS
      IF (RSS(J,2).GT.RMX) RMX=RSS(J,2)
      END DO
C     RMX now contains the largest projected radius of the eclipsing
C       star at the current orbital longitude.
C     Note that RMX and RMN differ from each other only because of
C       rotational or tidal distortion of the eclipsing star.
C     Note that RMX and RMN are measured from the center of the
C     eclipsing star.
C     First locate rim horizon points. Apply DX and DY to the accretion
C     disk coordinates. RMGX,RMGY,RMLX,RMLY are measured relative to
C     the center of the star with the accretion disk.
C     Knock out rim points hidden by the rim itself
C     First determine which rim ring is the upper visible boundary
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
C     Set radii and position angles for the upper rim of the accretion
C     disk.
C
      DO K=1,NSG
      IF (ISTAR.EQ.ISV) THEN
      X=PXR(IFLAG,K)+DX
      Y=PYR(IFLAG,K)+DY
      ELSE
      X=PXR(IFLAG,K)+DDX
      Y=PYR(IFLAG,K)+DDY
      END IF
      RVL(K)=DSQRT(X*X+Y*Y)
      ANL(K)=DASIN((Y)/RVL(K))
      END DO
      DO K=1,NRM/2
		DO L=1,NSG-1
			IF ((CSGR(K+1,L).LT.0.D0).AND.(CSGR(K+1,L+1).GE.0.D0)) 
     $		L1=L+1
			IF ((CSGR(K+1,L).GE.0.D0).AND.(CSGR(K+1,L+1).LT.0.D0)) 
     $		L2=L
		END DO
		DO L=1,10
			IF (L2.GE.10) THEN
				XGRY(L)=PXR(K+1,L2-10+L)
				YGRY(L)=PYR(K+1,L2-10+L)
			END IF
			IF (L2.LT.10) THEN
				IF (L2-10+L.GE.1) THEN
					XGRY(L)=PXR(K+1,L2-10+L)
					YGRY(L)=PYR(K+1,L2-10+L)
				END IF
				IF(L2-10+L.LT.1) THEN
					XGRY(L)=PXR(K+1,NSG-10+L2+L-1)
					YGRY(L)=PYR(K+1,NSG-10+L2+L-1)
				END IF
			END IF
		END DO
		DO L=1,10
			IF (L1.LE.NSG-10) THEN
				XLRY(L)=PXR(K+1,L1+10-L)
				YLRY(L)=PYR(K+1,L1+10-L)
			END IF
			IF (L1.GT.NSG-10) THEN
				IF (L1+L-1.LE.NSG) THEN
					XLRY(L)=PXR(K+1,L1+L-1)
					YLRY(L)=PYR(K+1,L1+L-1)
				END IF
				IF(L1+L-1.GT.NSG) THEN
					XLRY(L)=PXR(K+1,L-NSG+L1)
					YLRY(L)=PYR(K+1,L-NSG+L1)
				END IF
			END IF
		END DO
	IF (L1.GT.NSG/2) THEN
		DO L=1,NSG/2
			Y=PYR(K,L)
			CALL LAGIN(Y,X,YGRY,XGRY,3,10,M,0.D0)
			IF ((X.LT.PXR(K,L)).AND.(M.GT.2)) THEN
				IF (CSGR(K,L).GE.0.D0)CSGR(K,L)=-1.D0
			END IF
		END DO
		DO L=NSG/2,NSG
			Y=PYR(K,L)
			CALL LAGIN(Y,X,YLRY,XLRY,3,10,M,0.D0)
			IF ((X.LT.PXR(K,L)).AND.(M.GT.2)) THEN
				IF (CSGR(K,L).GE.0.D0)CSGR(K,L)=-1.D0
			END IF
		END DO
	END IF
	IF (L1.LE.NSG/2) THEN
		DO L=1,NSG/2
			Y=PYR(K,L)
			CALL LAGIN(Y,X,YLRY,XLRY,3,10,M,0.D0)
			IF ((X.LT.PXR(K,L)).AND.(M.GT.2)) THEN
				IF (CSGR(K,L).GE.0.D0)CSGR(K,L)=-1.D0
			END IF
		END DO
		DO L=NSG/2,NSG
			Y=PYR(K,L)
			CALL LAGIN(Y,X,YGRY,XGRY,3,10,M,0.D0)
			IF ((X.LT.PXR(K,L)).AND.(M.GT.2)) THEN
				IF (CSGR(K,L).GE.0.D0)CSGR(K,L)=-1.D0
			END IF
		END DO
	END IF
	END DO
C
      IF (IFLAG.GT.1) THEN
		KFLAG=IFLAG-1
	ELSE
		KFLAG=1
	END IF
      DO K=1,KFLAG
      RMGY(K)=-1.0
      RMLY(K)=1.0
         DO 20 J=1,NSG
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(K,J)+DX
         Y=PYR(K,J)+DY
         ELSE
         X=PXR(K,J)+DDX
         Y=PYR(K,J)+DDY
         END IF
         IF (Y.LE.RMGY(K)) GO TO 11
		RMGX(K)=X
		RMGY(K)=Y
   11    IF (Y.GE.RMLY(K)) GO TO 20
		RMLX(K)=X
		RMLY(K)=Y
   20	CONTINUE
      END DO
      DO K=IFLAG,NRM-1
		DO J=1,NSG-1
			IF ((CSGR(K,J).GE.0.D0).AND.(CSGR(K,J+1).LT.0.D0)) THEN
				IF (ISTAR.EQ.ISV) THEN
					RMLX(K)=PXR(K,J)+DX
					RMLY(K)=PYR(K,J)+DY
				ELSE
					RMLX(K)=PXR(K,J)+DDX
					RMLY(K)=PYR(K,J)+DDY
				END IF
			END IF
			IF ((CSGR(K,J).LT.0.D0).AND.(CSGR(K,J+1).GE.0.D0)) THEN
				IF (ISTAR.EQ.ISV) THEN
					RMGX(K)=PXR(K,J+1)+DX
					RMGY(K)=PYR(K,J+1)+DY
				ELSE
					RMGX(K)=PXR(K,J+1)+DDX
					RMGY(K)=PYR(K,J+1)+DDY
				END IF
			END IF
		END DO
	END DO
C     Predict the coordinates of RMGX(NRM), RMGY(NRM)
      XPRD=RMGX(NRM-1)+(RMGX(NRM-1)-RMGX(NRM-2))
	YPRD=RMGY(NRM-1)+(RMGY(NRM-1)-RMGY(NRM-2))
	DFF=1.D0
	DO J=1,NSG
		IF (ISTAR.EQ.ISV) THEN
			X=PXR(NRM,J)+DX
			Y=PYR(NRM,J)+DY
		ELSE
			X=PXR(NRM,J)+DDX
			Y=PYR(NRM,J)+DDY
		END IF
		DFFTS=DSQRT((X-XPRD)**2+(Y-YPRD)**2)
		IF (DFFTS.LT.DFF) THEN
			DFF=DFFTS
			XSV=X
			YSV=Y
		END IF
	END DO
  	RMGX(NRM)=XSV
	RMGY(NRM)=YSV
C     The horizon limits are symmetrical about the vertical axis
C     through the central star.
      DFF=1.D0
	DO J=1,NSG
		IF (ISTAR.EQ.ISV) THEN
			X=PXR(NRM,J)+DX
			Y=PYR(NRM,J)+DY
		ELSE
			X=PXR(NRM,J)+DDX
			Y=PYR(NRM,J)+DDY
		END IF
		A=RMGX(NRM)-X
		B=RMGY(NRM)+Y
		DFFTS=DSQRT(A**2+B**2)
		IF (DFFTS.LT.DFF) THEN
			DFF=DFFTS
			XSV=X
			YSV=Y
		END IF
	END DO
  	RMLX(NRM)=XSV
	RMLY(NRM)=YSV
      NRMHZ=NRM-IFLAG+1
C     RMLX(K) has the relative X coods. of points on one horizon of
C        accretion disk rim.
C     RMLY(K) has the corres. Y coods.
C     RMGX(K) has the relative X coods. of points on the other horizon
C        of the accretion disk rim.
C     RMGY(K) has the corres. Y coods.
C
C     Now set points defining one (vertical) horizon of the rim.
C     The horizon will be represented by NRMHZ-1 line segments.
      DO J=IFLAG,NRM
C     Note that it is necessary to displace the horizon points by the
C       displacement of the center of the star with the accretion disk.
		RXHZG(J-IFLAG+1)=RMGX(J)
		RYHZG(J-IFLAG+1)=RMGY(J)
		IF (ISTAR.EQ.ISV) THEN
			RXHZG(J-IFLAG+1)=RXHZG(J-IFLAG+1)-DX
			RYHZG(J-IFLAG+1)=RYHZG(J-IFLAG+1)-DY
		ELSE
			RXHZG(J-IFLAG+1)=RXHZG(J-IFLAG+1)-DDX
			RYHZG(J-IFLAG+1)=RYHZG(J-IFLAG+1)-DDY
		END IF
      END DO
C     Now do the other vertical horizon. Use the same number of line
C       segments.
      DO J=IFLAG,NRM
		RXHZL(J-IFLAG+1)=RMLX(J)
		RYHZL(J-IFLAG+1)=RMLY(J)
		IF (ISTAR.EQ.ISV) THEN
			RXHZL(J-IFLAG+1)=RXHZL(J-IFLAG+1)-DX
			RYHZL(J-IFLAG+1)=RYHZL(J-IFLAG+1)-DY
		ELSE
			RXHZL(J-IFLAG+1)=RXHZL(J-IFLAG+1)-DDX
			RYHZL(J-IFLAG+1)=RYHZL(J-IFLAG+1)-DDY
		END IF
      END DO
C     Special case of bottom rim, for toroidal rim profile.
C     Choosing the leftmost point on the bottom rim is not optimum.
C     Rather, need to have horizon boundary on "vertical" section a
C     single smooth curve. Determine slopes of line segments of 
C     horizon contour ending on rim circles NRMHZ-1 (last before the
C     bottom edge) and NRMHZ-2. Calculate the increment in the slopes
C     and apply that increment to the slope of the NRMHZ-1 line to 
C     determine the slope to use for the line from rim circle NRMHZ-1 
C     to NRMHZ. Then find the point on the NRMHZ circle closest to the 
C     intersection of the line just determined and the NRMHZ circle.
C     That point is the terminus of the line just determined.
C
C     First locate the index of the point on the bottom rim 
C     corresponding to RXHZL(NRMHZ), RYHZL(NRMHZ).
      IF (NTYPE.NE.1) THEN
      DO J=1,NSG
		IF ((PXR(NRM,J).EQ.RXHZL(NRMHZ)).AND.
     $    (PYR(NRM,J).EQ.RYHZL(NRMHZ))) JLM=J
	END DO     
      DLX=RXHZL(NRMHZ-1)-RXHZL(NRMHZ-2)
	DLY=RYHZL(NRMHZ-1)-RYHZL(NRMHZ-2)
	DV1=DLX/DLY   !Note that DLX is measured vertically
      DLX=RXHZL(NRMHZ)-RXHZL(NRMHZ-1)
	DLY=RYHZL(NRMHZ)-RYHZL(NRMHZ-1)
	DV2=DLX/DLY
	SLOPE=(DV1+DV2)/2.D0
	DLY=RYHZL(NRMHZ)-RYHZL(NRMHZ-1)
	DLX=DLY*SLOPE
	X0=RXHZL(NRMHZ-1)+DLX
	Y0=RYHZL(NRMHZ)
	DO K=1,20
		JRF=JLM+K-1
		IF (JRF.LE.NSG) THEN
			XAA=PXR(NRM,JRF)
			YAA=PYR(NRM,JRF)
			DV(K)=DSQRT((X0-XAA)**2+(Y0-YAA)**2)
		END IF
		IF (JRF.GT.NSG) THEN
			XAA=PXR(NRM,JRF-NSG+1)
			YAA=PYR(NRM,JRF-NSG+1)
			DV(K)=DSQRT((X0-XAA)**2+(Y0-YAA)**2)
		END IF
	END DO
	VMIN=10.D0
	KMN=0
	DO K=1,20
		IF (VMIN.GT.DV(K)) VMIN=DV(K)
		IF (VMIN.EQ.DV(K)) KMN=K
	END DO
	JLM=JLM+KMN-1
	IF (JLM.GT.NSG) JLM=JLM-NSG+1
	RXHZL(NRMHZ)=PXR(NRM,JLM)
	RYHZL(NRMHZ)=PYR(NRM,JLM)
C
      DO J=1,NSG
		IF ((PXR(NRM,J).EQ.RXHZG(NRMHZ)).AND.
     $    (PYR(NRM,J).EQ.RYHZG(NRMHZ))) JLM=J
	END DO
	JLM=JLM-KMN+1
	IF (JLM.LT.1) JLM=1     
	RXHZG(NRMHZ)=PXR(NRM,JLM)
	RYHZG(NRMHZ)=PYR(NRM,JLM)
	END IF
C     Now have max. projected radius of eclipsing component in RMX and
C     minimum projected radius in RMN.
C     If the star with the accretion disk is being eclipsed 
C     (ISTAR.NE.IEC),
C     compare distance (RV) of given point on rim of accretion disk
C     from center of eclipsing star with RMX and RMN. If RV>RMX, the
C     point is otherwise visible. If RV<RMN, the point is hidden.
C     The radii in RSS refer to the eclipsing star.
C
C     I designates the star with the accretion disk.
C     The same star is ISTAR.
C     Note that the coods. of the center of the eclipsing star are XG,
C     YG, relative to the center of the eclipsed star, with XG measured
C     vertically.
C
C     Set projected coods. in arrays RMGX, RMGY, RMLX, RMLY.
C     Also locate maximum X value
      XGR=-1.D0
	XLS=1.D0
      DO K=1,NRM
      RMGY(K)=-1.D0
      RMLY(K)=1.D0
         DO 21 J=1,NSG
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(K,J)+DX
         Y=PYR(K,J)+DY
         ELSE
         X=PXR(K,J)+DDX
         Y=PYR(K,J)+DDY
         END IF
	   IF (X.GT.XGR) XGR=X
	   IF (X.LT.XLS) XLS=X
         IF (Y.LE.RMGY(K)) GO TO 23
         RMGX(K)=X
         RMGY(K)=Y
   23    IF (Y.GE.RMLY(K)) GO TO 21
         RMLX(K)=X
         RMLY(K)=Y
   21    CONTINUE
      END DO
C     Set rectangle approximation to accretion disk as exclusion area
      IF (RMGX(IFLAG).GT.RMLX(NRM)) XUL=RMGX(IFLAG)
      IF (RMLX(NRM).GT.RMGX(IFLAG)) XUL=RMLX(NRM)
      IF (RMGX(IFLAG).LT.RMLX(NRM)) XLL=RMGX(IFLAG)
      IF (RMLX(NRM).LT.RMGX(IFLAG)) XLL=RMLX(NRM)
      YUL=RMGY(IFLAG)
      YLL=RMLY(NRM)
	DO K=1,NRM
	IF (YUL.LT.RMGY(K)) YUL=RMGY(K)
	IF (YLL.GT.RMLY(K)) YLL=RMLY(K)
	END DO
C     Now must displace points back to their true positions
      IF (ISTAR.EQ.ISV) THEN
      XLL=XLL-DX
      YLL=YLL-DY
      XUL=XUL-DX
      YUL=YUL-DY
      ELSE
      XLL=XLL-DDX
      YLL=YLL-DDY
      XUL=XUL-DDX
      YUL=YUL-DDY
      END IF
      IF (I.EQ.IEC) GO TO 10
C
C     Arrive here if the star with the accretion disk is being eclipsed
C
      DO J=1,NRM
         DO 1 K=1,NSG
C     RV is measured from the center of the eclipsing star. Note that
C        PXR and PYR are the actual (C.M.) projected coods. of the 
C        accretion disk point. Since the accret. disk is being eclipsed,
C        the central star is at the origin of coods.
C
C     Set visibility keys for rim points, PRRH()
      IF (ISEQ.EQ.1) THEN
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(J,K)-DDX
         Y=PYR(J,K)-DDY
         ELSE
         X=PXR(J,K)-DX
         Y=PYR(J,K)-DY
         END IF
	   RELY=Y
	   RELX=X
      END IF	    	
      IF (ISEQ.EQ.2) THEN
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(J,K)-DDX
         Y=PYR(J,K)-DDY
         ELSE
         X=PXR(J,K)-DX
         Y=PYR(J,K)-DY
         END IF
	   RELY=Y
	   RELX=X
      END IF	    	
      IF (ISEQ.EQ.3) THEN
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(J,K)-DDX
         Y=PYR(J,K)-DDY
         ELSE
         X=PXR(J,K)-DX
         Y=PYR(J,K)-DY
         END IF
	   RELY=Y
	   RELX=X
      END IF	    	
      IF (ISEQ.EQ.4) THEN
         IF (ISTAR.EQ.ISV) THEN
         X=PXR(J,K)-DDX
         Y=PYR(J,K)-DDY
         ELSE
         X=PXR(J,K)-DX
         Y=PYR(J,K)-DY
         END IF
	   RELY=Y
	   RELX=X
      END IF
C     Calculate distance of rim point from center of eclipsing star.		    	
         RV=DSQRT(RELY**2+RELX**2)
C     If distance is greater than maximum proj. radius of eclipsing
C     star, then the point is not eclipsed.
         IF ((RV.GT.RMX).AND.(PRRH(J,K).GE.0.D0)) PRRH(J,K)=1.0
C     If distance is less than the minimum proj. radius of the eclipsing
C     star, then the point is surely eclipsed.
         IF (RV.LT.RMN) PRRH(J,K)=-1.0
C     If the distance is intermediate, then must check in greater detail.
         IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 1
C     Now must do detailed check using position angle data for the
C     points on the periphery of the eclipsing star.
         ANG=DASIN(DABS(RELX)/RV)
C     Set correct quadrant
         IF (DABS(RELY).LT.0.D0) ANG=PI-ANG
         IF ((RELX).LT.0.D0) ANG=-ANG
           IF (ANG.LT.0.D0) GO TO 3
C     In range covered by IFCS
           CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,
     $     RW)
           GO TO 4
C     In range covered by IRCS
    3      CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $     RW)
C     RV is the distance of a point on the rim of the accretion disk
C        from the center of the star eclipsing the accretion disk.
C     The position angle of the point is ANG.
C     The projected radius of the eclipsing star, for the same position
C     angle, is RW.
    4      ER=RV-RW
           IF ((PRRH(J,K).GE.0.D0).AND.(ER.LT.0.D0)) PRRH(J,K)=ER
    1    CONTINUE
      END DO
C
C     Now do test for face points, set visibility keys in STVS().
C
      DO J=1,NRING
         DO 2 K=1,NSG
		IF (ISEQ.EQ.1) THEN
			IF (PRFH(J,K).LT.0.D0) GO TO 2
			IF (ISTAR.EQ.ISV) THEN
				X=PXF(J,K)-DDX
				Y=PYF(J,K)-DDY
			ELSE
				X=PXF(J,K)-DX
				Y=PYF(J,K)-DY
			END IF
			RELX=X
			RELY=Y
		END IF
		IF (ISEQ.EQ.2) THEN
			IF (PRFH(J,K).LT.0.D0) GO TO 2
			IF (ISTAR.EQ.ISV) THEN
				X=PXF(J,K)-DDX
				Y=PYF(J,K)-DDY
			ELSE
				X=PXF(J,K)-DX
				Y=PYF(J,K)-DY
			END IF
			RELX=X
			RELY=Y
		END IF
		IF (ISEQ.EQ.3) THEN
			IF (PRFH(J,K).LT.0.D0) GO TO 2
			IF (ISTAR.EQ.ISV) THEN
				X=PXF(J,K)-DDX
				Y=PYF(J,K)-DDY
			ELSE
				X=PXF(J,K)-DX
				Y=PYF(J,K)-DY
			END IF
			RELX=X
			RELY=Y
		END IF
		IF (ISEQ.EQ.4) THEN
			IF (PRFH(J,K).LT.0.D0) GO TO 2
			IF (ISTAR.EQ.ISV) THEN
				X=PXF(J,K)-DDX
				Y=PYF(J,K)-DDY
			ELSE
				X=PXF(J,K)-DX
				Y=PYF(J,K)-DY
			END IF
			RELX=X
			RELY=Y
		END IF
         RV=DSQRT(RELX**2+RELY**2)
         IF ((RV.GT.RMX).AND.(PRFH(J,K).GE.0.D0)) PRFH(J,K)=1.0
         IF (RV.LT.RMN) PRFH(J,K)=-1.0
         IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 2
C     Must do detailed check.
         ANG=DASIN(DABS(RELX)/RV)
C     Set correct quadrant
         IF (DABS(RELY).LT.0.D0) ANG=PI-ANG
         IF ((RELX).LT.0.D0) ANG=-ANG
         IF (ANG.LT.0.D0) GO TO 5
C     In range covered by IFCS
         CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,
     $   RW)
         GO TO 6
C     In range covered by IRCS
    5    CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,
     $   RW)
    6    ER=RV-RW
         IF ((PRFH(J,K).GE.0.D0).AND.(ER.LT.0.D0)) PRFH(J,K)=ER
    2    CONTINUE
      END DO
C
C     Now do visibility keys for the vertical horizon boundaries of
C     the accretion disk.
C
C     Note that there are two possible representations of the vertical
C     horizon boundaries. If NTYPE=1, the boundary is cylindrical.
C     If NTYPE=2, the boundary is half toroidal.
C     For eclipse of the accretion disk vertical boundaries, the arrays
C     RXHZG, RYHZG, RXHXL, RYHZL all that is needed is to have the
C     actual projected coods. in those arrays. The procedure is the
C     same whether the vertical boundary is cylindrical or toroidal.
      DO 12 J=1,NRMHZ
	IF (ISEQ.EQ.1) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZG(J)-DDX
			Y=RYHZG(J)-DDY
		ELSE
			X=RXHZG(J)-DX
			Y=RYHZG(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.2) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZG(J)-DDX
			Y=RYHZG(J)-DDY
		ELSE
			X=RXHZG(J)-DX
			Y=RYHZG(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.3) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZG(J)-DDX
			Y=RYHZG(J)-DDY
		ELSE
			X=RXHZG(J)-DX
			Y=RYHZG(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.4) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZG(J)-DDX
			Y=RYHZG(J)-DDY
		ELSE
			X=RXHZG(J)-DX
			Y=RYHZG(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
      RV=DSQRT(RELX**2+RELY**2)
      IF (RV.GT.RMX) PRVFG(J)=1.0
      IF (RV.LT.RMN) PRVFG(J)=-1.0
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 12
C     Must do detailed check
      ANG=ASIN(DABS(RELX)/RV)
      IF (DABS(RELY).LT.0.D0) ANG=PI-ANG
      IF (RELX.LT.0.D0) ANG=-ANG
         IF (ANG.LT.0.D0) GO TO 7
         CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
         GO TO 8
    7    CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
    8    ER=RV-RW
         PRVFG(J)=ER
   12 CONTINUE
C     Do other vertical horizon
      DO 13 J=1,NRMHZ
	IF (ISEQ.EQ.1) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZL(J)-DDX
			Y=RYHZL(J)-DDY
		ELSE
			X=RXHZL(J)-DX
			Y=RYHZL(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.2) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZL(J)-DDX
			Y=RYHZL(J)-DDY
		ELSE
			X=RXHZL(J)-DX
			Y=RYHZL(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.3) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZL(J)-DDX
			Y=RYHZL(J)-DDY
		ELSE
			X=RXHZL(J)-DX
			Y=RYHZL(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
	IF (ISEQ.EQ.4) THEN
		IF (ISTAR.EQ.ISV) THEN
			X=RXHZL(J)-DDX
			Y=RYHZL(J)-DDY
		ELSE
			X=RXHZL(J)-DX
			Y=RYHZL(J)-DY
		END IF
		RELX=X
		RELY=Y
	END IF
      RV=DSQRT(RELX**2+RELY**2)
      IF (RV.GT.RMX) PRVFL(J)=1.0
      IF (RV.LT.RMN) PRVFL(J)=-1.0
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 13
C     Must do detailed check
         ANG=DASIN(DABS(RELX)/RV)
         IF (DABS(RELY).LT.DABS(YG)) ANG=PI-ANG
         IF ((X-XG).LT.0.D0) ANG=-ANG
         IF (ANG.LT.0.D0) GO TO 14
         CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
         GO TO 15
   14    CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
   15    ER=RV-RW
         PRVFL(J)=ER
   13 CONTINUE
      GO TO 60
C
C  *******************************************************************
C
C     Set visibility keys for star 3-I, if the star with the accretion
C     disk is the eclipsing component.
C
   10 CONTINUE
C
C     At this point, must distinguish between case of cylindrical rim 
C     and half toroidal rim.
C     First test for toroidal case.
      IF (NTYPE.EQ.2) GO TO 70
C     Now test for cylindrical case.
	IF (NTYPE.NE.1) CALL PMDSTOP	
	GO TO 90
C
C     Start of section for toroidal rim
   70 CONTINUE
C     Set visibility keys for face of companion.
C
   90 DO 25 ITH=1,NTH
		N=NPH(3-I,ITH)
         DO 24 K=1,N
C     Note that X,Y are coods. in center of mass system.
C     The coods. of the rectangle corners also are in the C. of M.
C     system.
         X=STX(3-I,ITH,K)
         Y=STY(3-I,ITH,K)
         IF (X.GT.XUL) GO TO 44
         IF (X.LT.XLL) GO TO 45
C     Distinguish between two types of rim boundary.
C     Case of cylindrical boundary first.
C
C     If point is within rectangle, set visibility key for blocking
         IF (NTYPE.EQ.1) THEN
         IF ((Y.LT.YUL).AND.(Y.GT.YLL)) STVS(3-I,ITH,K)=-1.0D0
         GO TO 24
	   END IF
	   IF (NTYPE.NE.2) CALL PMDSTOP
	   IF (X.LE.RXHZG(1)) GO TO 45
	   IF (X.GT.RXHZG(NRMHZ)) GO TO 44
         DO L=1,NRMHZ-1
			IF ((X.GT.RXHZG(L)).AND.(X.LE.RXHZG(L+1))) THEN
				AR2(1)=RYHZG(L)
				AR2(2)=RYHZG(L+1)
				AR2(3)=0.D0
				AR3(1)=RXHZG(L)
				AR3(2)=RXHZG(L+1)
				AR3(3)=0.D0
				CALL STLINE(AR3,AR2,0,IER)
				IF (IER.EQ.129) CALL PMDSTOP
				VY1=AR2(1)+AR2(2)*X
				AR2(1)=RYHZL(L)
				AR2(2)=RYHZL(L+1)
				AR2(3)=0.D0
				AR3(1)=RXHZL(L)
				AR3(2)=RXHZL(L+1)
				AR3(3)=0.D0
				CALL STLINE(AR3,AR2,0,IER)
				IF (IER.EQ.129) CALL PMDSTOP
				VY2=AR2(1)+AR2(2)*X
				IF ((Y.LT.VY1).AND.(Y.GT.VY2)) STVS(3-I,ITH,K)=-1.D0
			END IF
	   END DO
	   GO TO 24	
C     Test for blocking by lower face of accretion disk. Note that
C     XRMC and YRMC are measured relative to center of central star in
C     accretion disk.
C     RDS is distance from center of lower face of accretion disk to
C        point on projected photosphere of companion.
C     ANGL is position angle of that point, measured at center of
C        accretion disk face.
C     The coods. of X,Y already are in the center of mass system.
C     The coods. of the center of star ISTAR(=I), in the center of
C     mass system, are either -DX,-DY or -DDX,-DDY.
   44    IF (ISTAR.EQ.ISV) THEN
         X=X+DX
         Y=Y+DY
         ELSE
         X=X+DDX
         Y=Y+DDY
         END IF
         RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
         ANGL=DASIN(DABS(Y-YRMC)/RDS)
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 26
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=
     $   PI-ANGL
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=
     $   ANGL+PI
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=
     $   TWPI-ANGL
   26    CONTINUE
         IF ((ANGL.GE.0.D0).AND.(ANGL.LE.TWPI)) GO TO 27
         IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
         IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   27    DO J=1,NSG-1
         IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 28
         IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LT.AGLD(J+1))) GO TO 34
         END DO
	   J=NSG
         IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 28
         IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 200
         ANGMX=-1.D0
		ANGMN=10.D0
		DO J=1,NSG
			IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
			IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
		END DO
		DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 360
		END DO
		CALL PMDSTOP
  360		IF (J.GT.1) THEN
			IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
				RDRY(1)=RD(J-1)
				RDRY(2)=RD(J)
				RDRY(3)=RD(J+1)
				AGRY(1)=AGLD(J-1)
				AGRY(2)=AGLD(J)
				AGRY(3)=AGLD(J+1)+TWPI
				GO TO 201
		END IF
		END IF
		IF (J.EQ.1) THEN
			IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
				RDRY(1)=RD(J)
				RDRY(2)=RD(J+1)
				RDRY(3)=RD(J+2)
				AGRY(1)=AGLD(J)
				AGRY(2)=AGLD(J+1)+TWPI
				AGRY(3)=AGLD(J+2)+TWPI
				GO TO 201
			END IF
		END IF
		DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 361
		END DO
		CALL PMDSTOP
  361		IF (J.GT.1) THEN
			IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
				RDRY(1)=RD(J-1)
				RDRY(2)=RD(J)
				RDRY(3)=RD(J+1)
				AGRY(1)=AGLD(J-1)-TWPI
				AGRY(2)=AGLD(J)-TWPI
				AGRY(3)=AGLD(J+1)
				GO TO 201
			END IF
		END IF
		IF (J.EQ.1) THEN
			IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
				RDRY(1)=RD(J)
				RDRY(2)=RD(J+1)
				RDRY(3)=RD(J+2)
				AGRY(1)=AGLD(J)-TWPI
				AGRY(2)=AGLD(J+1)
				AGRY(3)=AGLD(J+2)
				GO TO 201
			END IF
		END IF
         CALL PMDSTOP
  200    RDRY(1)=RD(NSG)
         RDRY(2)=RD(1)
	   RDRY(3)=RD(2)
	   AGRY(1)=AGLD(NSG)
	   AGRY(2)=AGLD(1)
	   AGRY(3)=AGLD(2)
	   GO TO 201
   28    ER=RDS-RD(J)
C     Higher level test for X.GE.XGR
   36    IF (X.GT.XGR) GO TO 24			
         IF ((STVS(3-I,ITH,K).GE.0.D0).AND.(ER.LT.0.D0)) 
     $	STVS(3-I,ITH,K)=ER
         GO TO 24
C     Test for blocking by upper edge of accretion disk
C     The coods. X,Y are already on the center of mass system.
C     The coods. of the center of star ISTAR(=I), in the center
C     of mass system, are either -DX,-DY or -DDX,-DDY.
C     Note that radii (RVL values) and position angles (ANL values)
C     are measured from the projected center of the central star.
   45    IF (ISTAR.EQ.ISV) THEN
         X=X+DX
         Y=Y+DY
         ELSE
         X=X+DDX
         Y=Y+DDY
         END IF
         RDS=DSQRT(X**2+Y**2)
         ANGL=DASIN(DABS(Y)/RDS)
         DO 40 J=1,NSG-1
C     Restrict test to visible segment of upper rim
         IF (CSGR(IFLAG,J).LT.0.D0) GO TO 40
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 41
         IF ((ANGL.GT.ANL(J)).AND.(ANGL.LT.ANL(J+1))) GO TO 42
         IF ((ANGL.LT.ANL(J)).AND.(ANGL.GT.ANL(J+1))) GO TO 42
   40    CONTINUE
         J=NSG
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 41
C     Point lies outside boundaries of accretion disk.
         GO TO 24
   41    ER=RDS-RVL(J)
   43    IF ((STVS(3-I,ITH,K).GE.0.D0).AND.(ER.LT.0.D0)) 
     $	STVS(3-I,ITH,K)=ER
         GO TO 24
   42    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RVL(J+JJ-KK)
         AGRY(JJ)=ANL(J+JJ-KK)
         END DO
  201    CALL PARAB(AGRY,RDRY)
         IF (IER.EQ.129) THEN
			CALL PMDSTOP
	   END IF
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
C     RVV is the distance from the center of the accretion disk lower
C        face (projected) to the rim, at position angle ANGL.
         ER=RDS-RVV
         IF ((STVS(3-I,ITH,K).GE.0.D0).AND.(ER.LT.0.D0)) 
     $	STVS(3-I,ITH,K)=ER
         GO TO 24
   34    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RD(J+JJ-KK)
         AGRY(JJ)=AGLD(J+JJ-KK)
         END DO
         CALL PARAB(AGRY,RDRY)
         IF (IER.EQ.129) THEN
			CALL PMDSTOP
	   END IF
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
         ER=RDS-RVV
         GO TO 36
   24    CONTINUE
   25 CONTINUE
   60 RETURN
      END
