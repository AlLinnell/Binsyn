      SUBROUTINE VSBSB(I,PXR,PYR,PZR,AGLD,RD,PRSH,CSGR,
     $PRRH,CSI,
     $RMLX,RMLY,RMGX,RMGY,NRM,NTH,
     $IEC,IFC,IRC,IFCS,IRCS,NSG,VXP,VYP,XRMC,YRMC,XG,YG,DX,DY,
     $DDX,DDY,ISTAR,ISV,STX,ISEQ,IBDY,NTYPE,MRM,MSG,IRD,NOUT7)
      INCLUDE 'COMA.FOR'
      DIMENSION AGLD(MSG),RD(MSG),PXR(MRM,MSG),PYR(MRM,MSG),
     $CSGR(MRM,MSG),PRSH(MTW,MTHM,MTW),RVLD(MSG),RMLY(MRM),RMGY(MRM),
     $RMLX(MRM),PZR(MRM,MSG),
     $RMGX(MRM),PRRH(MRM,MSG),
     $ANL(MSG)
      DIMENSION RDRY(MTR),AGRY(MTR),VXP(MTW,MTHM,MTW),
     $VYP(MTW,MTHM,MTW),
     $VXPA(MTW,MTHM,MTW),VYPA(MTW,MTHM,MTW),PRSHA(MTW,MTHM,MTW),
     $STX(MTW,MTHM,MPHM)
      DIMENSION RXHZG(MRM),RYHZG(MRM),RXHZL(MRM),RYHZL(MRM),AR2(MTR),
     $AR3(MTR)
	DIMENSION RDRF(MSG),AGLRF(MSG)
C**********************************************************************
C     Subroutine to set visibility keys for central star boundary, based
C     on view blocking by accretion disk as well as by companion star.
C     Set visibility keys for companion star boundary based on view 
C     blocking by central star or accretion disk.
C**********************************************************************
C     List of defined quantities:
C		AGLD			Position angle of a point on the lower rim of the 
C					accretion disk. The origin is at the projected 
C					center of the ellipse representing the lower face.
C					The coods. of that center are (XRMC,YRMC). The 
C					position angle is 0.0 for a vector pointed along 
C					the negative X axis (straight up).
C		ANL			Position angle of a point on the upper visible rim
C					of the accretion disk. The origin is at the 
C					projected center of the ellipse representing the
C					lower accretion disk face.
C		DX,DY		Coods., prefixed by (-) sign, of projected center
C					of one star, in center of mass system.
C		DDX,DDY		Coods., prefixed by (-) sign, of projected center
C					of other star, in center of mass system.
C         IBDY        Input key to ACPGB specifying whether the 
C                     accretion disk is optically thick or thin.
C					If IBDY=0, the accretion disk is optically thin.
C					If IBDY=1, the accretion disk is optically thick.
C         IEC         The star that is the eclipsing component in the 
C					current orbital quadrant.
C		IFLAG		Integer designating the rim ring that is the 
C					uppermost visible ring.
C		ISEQ1,2,3,4 Designators of orbital phase quadrants.
C		ISV		    The star that is eclipsed at orbital phase 0.0
C		ISTAR		The gainer
C		NRING		The number of annuli boundaries representing the 
C					accretion disk face.
C		NRMHZ		Formally, NRMHZ=NRM-IFLAG+1. It is the number of 
C					visible rim rings. 
C		NSG			The number of dividing faces that segment the face
C					annuli and the rim rings.
C		NTYPE		Key specifying shape of rim boundary.
C					If NTYPE=1, boundary is cylindrical section.
C					This option no longer is implemented.
C					If NTYPE=2, boundary is half toroid.
C		PRFH		Visibility keys for the accretion disk face.
C		PRRH		Visibility keys for the accretion disk rim.
C		PRSH		Visibility keys for the star boundaries.
C		PXF,PYF		Projected coods. of face grid points. Origin 
C					initially at center of gainer, then transferred
C					to projected center of mass.
C		PXR,PYR		Projected coods. of rim grid points. Origin
C					initially at center of gainer, then transferred
C					to projected center of mass.  
C		RD			Distances of lower rim points from projected 
C					center of lower accretion disk face, at 
C					(XRMC,YRMC).
C		RDMN		The distance from (XRMC,YRMC) to the projected
C					accretion disk rim along the X axis. Can be 
C					measured in either (+) or (-) X-axis direction.
C                     A positive quantity.
C		RDTS		Formally, RDTS=XRMC-RDMN. It is the projected
C					distance along the positive X axis, from the
C					projected center of the gainer to the X axis
C					intersection with the outer edge of the bottom
C					face of the accretion disk. If RDTS is (-), the 
C					outer edge lies above the projected center of 
C					the gainer and that projected center is visible.
C					If RDTS is (+), the projected center is hidden by
C					the outer edge of the accretion disk.
C		RMGX,RMGY	For each visible rim ring, the coods. of that rim 
C					ring point with the greatest Y value. The coods. 
C					are relative to the projected center of the gainer.
C		RMLX,RMLY	For each visible rim ring, the coods. of that rim 
C					ring point with the smallest Y value. The coods.
C					are relative to the projected center of the gainer.   
C         RMN         The minimum projected radius of the eclipsing 
C					component. If the distance of a point on an
C					eclipsed component from the projected center of
C					the eclipsing component is less than RMN, then
C					that point is surely hidden.
C		RMX         The maximum projected radius of the eclipsing
C					component. If the distance of a point on an
C					eclipsed component from the projected center of
C					the eclipsing component is greater than RMX, then
C					that point is not hidden.
C		RVLD			Distances of points on upper visible rim of
C					accretion disk from projected center of ellipse
C					representing projection of accretion disk lower 
C					face.
C		RXHZG,RYHZG	Arrays associated with RMGX,RMGY. The array RMGX
C					ranges from IFLAG to NRM. The array RXHZG contains
C					the same data, but ranges from 1 to NRMHZ.
C		RXHZL,RYHZL	Same type of association with RMLX, RMLY as RXHZG
C					has with RMGX.
C		STVS		Visibility keys for the faces of the two stars.
C		STX,STY		Coods. of projections of star grids. At entry
C					to VSBSB, the values of STX, STY are in center of
C					mass coods.
C		VXP,VYP		Projected coods. of star boundaries.
C		XG,YG		Coods. of projected center of mass relative to
C					center of one component.
C		XGR			The lower rim point with the most positive X cood.
C		XLL,YLL		A corner of an area enclosed by two horizontal 
C					lines and two convex ends. The are is defined by
C					the accretion disk projection. The convex ends
C					are the projected rim boundaries. The point
C					(XLL,YLL) is the corner with the most negative
C					X cood. and the leftmost Y cood.
C		XLS			The projected VISIBLE upper rim point with the
C					most negative X cood.
C		XRMC    	The projected distance between the center of the
C					ellipse representing the projected bottom edge of
C					the accretion disk and the projected center of the
C					gainer.	A positive quantity.
C		XUL,YUL		Another corner of the enclosed area partly 
C					representing the projection of the accretion disk.
C					This point is the corner with the most positive
C					X cood., and with the rightmost Y cood. Note that
C					the enclosed area does not yet include the convex
C					regions at the top and bottom, representing the
C					projections of the top and bottom visible rims.
C		YMX			The maximum sideways (Y direction) extent of the
C					projection of the gainer.
C
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
	DATA PIO2/1.5707963267948966192D0/
	DATA ERLT/1.D-13/
C     Find min.projected radius of eclipsing component.
C     In the following statements, RSS comes from program PLTB.
C     PLTB processes the eclipsing component first (ICOUNT=1), and saves
C     the arrays in RSS and ALPS, so RSS refers to the eclipsing
C     component.
C
      IF (NRM.EQ.0) RETURN
C     Find minimum projected radius of eclipsing component.
C     If the distance of a point on an eclipsed component from the
C     projected center of the eclipsing component is less than
C     RMN, then that point surely is hidden. Initialize RMN to
C     a value larger than the scale size of the system.
      RMN=2.D0
      DO J=1,IFCS
      IF (RSS(J,1).LT.RMN) RMN=RSS(J,1)
      END DO
      DO J=1,IRCS
      IF (RSS(J,2).LT.RMN) RMN=RSS(J,2)
      END DO
C     Find max. projected radius of eclipsing component.
C     If the projected distance of a point on an eclipsed component
C     from the projected center of the eclipsing component is greater
C     than RMX, then that point is not hidden.
C     Initialize RMX to a distance of 0.D0.
      RMX=0.D0
      DO J=1,IFCS
      IF (RSS(J,1).GT.RMX) RMX=RSS(J,1)
      END DO
      DO J=1,IRCS
      IF (RSS(J,2).GT.RMX) RMX=RSS(J,2)
      END DO
C************************************************
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
C
C     IFLAG now designates the upper rim circle which is visible.
C**********************************************************************
C
C     Determine X cood. of projection of equatorial point of central
C     star that is on the horizon of the observer.
C     Note that ISTAR designates the gainer. NTH designates the 
C     equatorial colatitude circle of the gainer. KST designates
C     the ordinal location in the array of the start of the equatorial
C     circle points. N specifies the number of points on the equatorial
C     circle. KND specifies the ordinal location of the last point on
C     the equatorial circle.
      N=NPH(ISTAR,NTH/2+1)
C     The points in STX() for circle NTH/2+1,designating the equator, go
C     through an azimuth range of two pi. Variation in orbital 
C     inclination tips the star on an axis in the equatorial plane
C     perpendicular to the line of sight. The X coods. are displaced
C     symetrically in a positive and negative direction. Their average
C     continues to indicate the X coods. of the two horizon points
C     (both horizon points have the same X cood.).
C     I points at the star corrently under consideration. IEC designates
C     the star which is the eclipsing component in the current orbital
C     longitude quadrant. ISV is the star eclipsed at orbital longitude
C     0.0.
C     At this point, STX() and STY() are in center of mass coods. Then
C     XAV is in center of mass coods.
      XAV=0.D0
      VN=N
      DO K=1,N
	IF (I.EQ.IEC) THEN
		IF (ISTAR.EQ.ISV) THEN
			XAD=STX(I,NTH/2+1,K)
		ELSE
			XAD=STX(3-I,NTH/2+1,K)
		END IF
	ELSE
		IF (ISTAR.EQ.ISV) THEN
			XAD=STX(I,NTH/2+1,K)
		ELSE
			XAD=STX(3-I,NTH/2+1,K)
		END IF
	END IF
      XAV=XAV+XAD
      END DO
      XAV=XAV/VN
C************************************************
C     XLS is center of mass coordinate of negative X axis intersection
C     with uppermost visible rim circle.
      XLS=1.D0
      DO K=1,NSG
      X=PXR(IFLAG,K)
      IF (X.LT.XLS) XLS=X
	IF (X.EQ.XLS) KLS=K
      END DO
C     Find center of mass coordinate of most positive X point on bottom 
C     rim.
      XGR=-1.D0
      DO K=1,NSG
      X=PXR(NRM,K)
      IF (X.GT.XGR) XGR=X
      END DO
C*********************************************		   
C      
C     Calculate radii and position angles of top rim of accretion disk
C     The radii are RVLD(K).
C     The position angles are ANL(K).
C     Note that, at input to this SRT, PXR and PYR are in the center of
C     mass coordinate system. To calculate RVLD, must transform to origin
C     at projected center of gainer.
      KMN=1
      DO K=1,NSG
      IF (ISTAR.EQ.ISV) THEN
      X=PXR(IFLAG,K)+DX
      Y=PYR(IFLAG,K)+DY
      ELSE
      X=PXR(IFLAG,K)+DDX
      Y=PYR(IFLAG,K)+DDY
      END IF
      RVLD(K)=DSQRT(X**2+Y**2)
      ANL(K)=DASIN(Y/RVLD(K))
      END DO
C************************************************			 
C     Find RDMN, the minimum radius on outer edge of bottom face of acc. disk
C     Do this by first finding maximum radius, RDMX, and then multiplying by
C     cos(i). The value -cos(i) is in CSI.
      RDMX=0.D0
      DO J=1,NSG
      IF (RD(J).GT.RDMX) RDMX=RD(J)
	IF (RD(J).EQ.RDMX) JRF=J
      END DO
	RDMN=DABS(RDMX*CSI)
C     Calculate test distance RDTS. Determines whether projected
C     center of gainer is visible or hidden by outer edge of accretion
C     disk.
C     RDTS is distance along neg. X axis (vertically
C     upward) to intersection with outer edge of bottom face.
      RDTS=XRMC-RDMN
C**************************************************
C     Determine the rim horizon points. Store in arrays RMGX, RMGY
C     for one horizon profile, and in arrays RMLX, RMLY for other
C     horizon profile.
C*************************************************
C     Because the horizon points may be visible for otherwise hidden
C     rim circles, first establish the horizon points for those rim
C     circles prior to circle IFLAG.
      IF (IFLAG.GT.1) THEN
		KFLAG=IFLAG-1
	ELSE
		KFLAG=1
	END IF
      DO K=1,KFLAG
		RMGY(K)=-1.0D0
		RMLY(K)=1.D0
		DO 110 J=1,NSG
			IF (ISTAR.EQ.ISV) THEN
				X=PXR(K,J)+DX
				Y=PYR(K,J)+DY
			ELSE
				X=PXR(K,J)+DDX
				Y=PYR(K,J)+DDY
			END IF
			IF (Y.LE.RMGY(K)) GO TO 109
				RMGX(K)=X
				RMGY(K)=Y
  109			IF (Y.GE.RMLY(K)) GO TO 110
				RMLX(K)=X
				RMLY(K)=Y
  110		CONTINUE
	END DO
C     Now, beginning with IFLAG, test each rim circle for horizon by
C     finding where CSGR changes sign in one direction at one horizon,
C     and in the other direction at the other horizon.
	DO K=IFLAG,NRM
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
C     XR1 and XR2 are X coods. of points on the bottom rim of the accretion
C     disk in center of mass coods. 
C     Determine array of radii and position angles for rim points,
C     referenced to the projected center of the gainer.
C     XXR1, YYR1 are coods. relative to the projected center of the gainer.
C     RDRF(K) are radial distances, on the plane of the sky, from the
C     projected center of the gainer, to the point in question.
C     AGLRF(K)  are the corresponding position angles for bottom rim points
      DO K=1,NSG
		XR1=PXR(NRM,K)
		YR1=PYR(NRM,K)
		ZR1=PZR(NRM,K)
		IF (ISTAR.EQ.ISV) THEN
			XXR1=XR1+DX
			YYR1=YR1+DY
		ELSE
			XXR1=XR1+DDX
			YYR1=YR1+DDY
		END IF
		RDRF(K)=DSQRT(XXR1**2+YYR1**2)
		IF (XXR1.LE.0.D0) THEN
			IF (IRD.EQ.-1) THEN
				IF (YYR1.LT.0.D0) AGLRF(K)=DATAN(XXR1/DABS(YYR1))
				IF (DABS(YYR1).LE.ERLT) AGLRF(K)=-PIO2
				IF (YYR1.GT.0.D0) AGLRF(K)=-PI-DATAN(XXR1/DABS(YYR1))	
			ELSE
				IF (YYR1.LT.0.D0) AGLRF(K)=-PI-DATAN(XXR1/DABS(YYR1))
				IF (DABS(YYR1).LE.ERLT) AGLRF(K)=-PIO2
				IF (YYR1.GT.0.D0) AGLRF(K)=DATAN(XXR1/DABS(YYR1))
			END IF
		ELSE
			IF (IRD.EQ.-1) THEN
				IF (YYR1.LT.0.D0) AGLRF(K)=DATAN(XXR1/DABS(YYR1))
				IF (DABS(YYR1).LE.ERLT) AGLRF(K)=PIO2
				IF (YYR1.GT.0.D0) AGLRF(K)=PI-DATAN(XXR1/DABS(YYR1))	
			ELSE
				IF (YYR1.LT.0.D0) AGLRF(K)=PI-DATAN(XXR1/DABS(YYR1))
				IF (DABS(YYR1).LE.ERLT) AGLRF(K)=PIO2
				IF (YYR1.GT.0.D0) AGLRF(K)=DATAN(XXR1/DABS(YYR1))
			END IF
		END IF
C		AGLRF(K)=DACOS(YYR1/RDRF(K))
C		IF (XXR1.LT.0.D0) AGLRF(K)=-AGLRF(K)
c			WRITE (NOUT7,1000) PXR(NRM,K),PYR(NRM,K),PZR(NRM,K),
c     $			xxr1,yyr1,RDRF(K),AGLRF(K)
		IF (K.GT.1) THEN
C			IF ((AGLRF(K).GE.0.D0).AND.(AGLRF(K-1).LT.0.D0))
C     $			AGLRF(K)=AGLRF(K)-TWPI 
		END IF
	END DO
	DO K=1,NSG
c	WRITE (NOUT7,1000) PXR(NRM,K),PYR(NRM,K),RDRF(K),AGLRF(K)
	END DO
c	IF (I.LT.1000) STOP
 1000 FORMAT (7F12.8)
 1001 FORMAT (' ')
 1002 FORMAT (5I5)
C     End of test for rim circles, tested by horizon condition.
C
C     For the bottom rim circle, some points may lie outside horizon
C     test but still need to be plotted. Test separately.
	RMGY(NRM)=-1.D0
	RMLY(NRM)=1.D0
	DO J=1,NSG
		IF (ISTAR.EQ.ISV) THEN
			X=PXR(NRM,J)+DX
			Y=PYR(NRM,J)+DY
		ELSE
			X=PXR(NRM,J)+DDX
			Y=PYR(NRM,J)+DDY
		END IF
		IF (Y.GE.RMGY(NRM)) THEN
			RMGX(NRM)=X
			RMGY(NRM)=Y
		END IF
		IF (Y.LE.RMLY(NRM)) THEN
			RMLX(NRM)=X
			RMLY(NRM)=Y
		END IF
	END DO
C**********************************************************************
C     Determine corner points of enclosed area produced by projection of
C     accretion disk. The rim horizon will be a convex profile at each
C     horizon. The top and bottom will be horizontal straight lines
C     connecting the corners. This omits the convex curves at the top
C     and bottom, produces by the projected top and bottom rim edges.
C     These corrections will be determined later. Note that an 
C     inclination	of 90 degrees produces straight lines at the top and 
C     bottom.	The point (XUL,YUL) is the one with the most positive X
C     coordinate and the rightmost Y cood. The point (XLL,YLL) is the
C     one with the most negative X cood. and the leftmost Y cood.
C
      IF (RMGX(IFLAG).GT.RMLX(NRM)) XUL=RMGX(IFLAG)
      IF (RMLX(NRM).GT.RMGX(IFLAG)) XUL=RMLX(NRM)
      IF (RMGX(IFLAG).LT.RMLX(NRM)) XLL=RMGX(IFLAG)
      IF (RMLX(NRM).LT.RMGX(IFLAG)) XLL=RMLX(NRM)
      YUL=RMGY(IFLAG)
      YLL=RMLY(NRM)
C**********************************************************************
C     Now must displace points back to their true positions in the 
C     center of mass coordinate system.
      IF ((ISEQ.EQ.2).OR.(ISEQ.EQ.3)) THEN
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
	ELSE
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
      END IF
C
C**********************************************************************
C     For projection information describing the convex ends of the 
C     projection box describing the accretion disk, to evaluate its
C     eclipsing properties, need arrays containing the visible horizon
C     points for the convex ends, with index values running from 1 to
C     the bottom of the accretion disk. NRMHZ is the last entry in the
C     seqwuential list of visible horizon points.
C	Do the two ends separately.
C
	NRMHZ=NRM-IFLAG+1
	DO J=IFLAG,NRM
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
C     Now set YLL and YUL, based on the visibility data just determined.
	DO J=1,NRMHZ
		IF (YLL.GT.RYHZL(J)) YLL=RYHZL(J)
		IF (YUL.LT.RYHZG(J)) YUL=RYHZG(J)
	END DO
C**********************************************************************
C     Save the current RVLD that starts the list.
C     This will eventually be used for restoration.
 	RVLSVA=RVLD(1)
C     Shift cycically	until entries corresponding to
C     XLS start the sequence
      DO J=1,KLS-1
		RVLSV=RVLD(1)
		ANLSV=ANL(1)
		CSGRSV=CSGR(IFLAG,1)
		PRRHSV=PRRH(IFLAG,1)
		DO K=1,NSG-1
			RVLD(K)=RVLD(K+1)
			ANL(K)=ANL(K+1)
			CSGR(IFLAG,K)=CSGR(IFLAG,K+1)
			PRRH(IFLAG,K)=PRRH(IFLAG,K+1)
		END DO
		RVLD(NSG)=RVLSV
		ANL(NSG)=ANLSV
		CSGR(IFLAG,NSG)=CSGRSV
		PRRH(IFLAG,NSG)=PRRHSV
	END DO
C     Now shift backwards until entry corresponding to
C     either YLL or YUL starts the sequence
      DO J=1,NSG
		IF (RVLD(NSG).GT.RVLD(1)) THEN
			RVLSV=RVLD(NSG)
			ANLSV=ANL(NSG)
			CSGRSV=CSGR(IFLAG,NSG)
			PRRHSV=PRRH(IFLAG,NSG)
			DO K=1,NSG-1
				RVLD(NSG-K+1)=RVLD(NSG-K)
				ANL(NSG-K+1)=ANL(NSG-K)
				CSGR(IFLAG,NSG-K+1)=CSGR(IFLAG,NSG-K)
				PRRH(IFLAG,NSG-K+1)=PRRH(IFLAG,NSG-K)
			END DO
			RVLD(1)=RVLSV
			ANL(1)=ANLSV
			CSGR(IFLAG,1)=CSGRSV
			PRRH(IFLAG,1)=PRRHSV
		ELSE
			GO TO 7
		END IF
      END DO
    7 CONTINUE
C**********************************************************************
C
C     Find the range of index values corresponding to visible points.
      KLS=1
	KK=0
	KL=0
	DO KM=1,NSG-1
		IF ((CSGR(IFLAG,KM).GE.0.D0).AND.(CSGR(IFLAG,KM+1).LT.0.D0)) THEN
			KK=KM
		END IF
		IF ((CSGR(IFLAG,KM).LT.0.D0).AND.(CSGR(IFLAG,KM+1).GE.0.D0)) THEN
			KL=KM
		END IF
	END DO
	IF (KL.EQ.0) KL=1
	IF (KK.EQ.0) KK=1
	IF ((KK.LT.KL).AND.(KK.GT.NSG/4)) THEN
	KSTRT=1
	KSTOP=KK
	END IF
	IF ((KK.LT.KL).AND.((NSG-KL).GT.NSG/4)) THEN
	KSTRT=KL
	KSTOP=NSG
	END IF
	IF ((KK.GE.KL).AND.(KK.NE.1)) THEN
	KSTRT=KL
	KSTOP=KK
	END IF
	IF ((KL.EQ.1).AND.(KK.EQ.1)) THEN
	KSTRT=1
	KSTOP=NSG/2
	END IF
	KLS=KSTOP
    8 CONTINUE
C     Last permissible index value stored in KLS
C*********************************************
C
C
      IF (I.EQ.IEC) GO TO 90
C**********************************************************************
C**********************************************************************
C
C     This program section corresponds to object I being eclipsed.
C
C     Knock out points on boundary of star I 
C     (central star in accretion disk)
C     hidden by companion.
C     Note that star I is being eclipsed by the companion, so it was
C     processed during ICOUNT=2 in PLTB. Hence, it is referenced by
C     IFC,IRC,RS and ALP.
C
C     Do IFC branch. This is the large theta branch.
C
      DO K=1,IFC
      ANGLE=ALP(K,1)
      RDS=RS(K,1)
C     Set rectangular coods. of boundary points of the central star,
C     relative to its own center, in VXP, VYP. First for IFC branch.
C     Calculate RV, the distance of the point in question from the 
C     center of the companion.XG, YG are the (-) coods. of the center
C     of the companion relative to the central star.
C     In the following, REFX and REFY are X-displacements and Y-displacements
C     from the center of the companion to the point in question on the
C     boundary of the central star.
C     Note that we reset the values of VXP,VYP, based on the fit to the
C     object boundary in arrays ALP and RS.
C     The successive values of ISEQ designate the succession of orbital
C     phase quadrants.
      IF (ISEQ.EQ.1) THEN
		VYP(I,K,1)=RDS*DCOS(ANGLE)
		VXP(I,K,1)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		ELSE
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		END IF
	END IF
	IF (ISEQ.EQ.2) THEN
		VYP(I,K,1)=-RDS*DCOS(ANGLE)
		VXP(I,K,1)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		ELSE
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		END IF
	END IF
	IF (ISEQ.EQ.3) THEN
		VYP(I,K,1)=RDS*DCOS(ANGLE)
		VXP(I,K,1)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		ELSE
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		END IF
	END IF
	IF (ISEQ.EQ.4) THEN
		VYP(I,K,1)=-RDS*DCOS(ANGLE)
		VXP(I,K,1)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		ELSE
			REFX=VXP(I,K,1)+XG
			REFY=VYP(I,K,1)+YG
		END IF
	END IF
	RV=DSQRT(REFX**2+REFY**2)
C     If that dist. is less than the min. projected radius of the 
C     companion,then it is surely hidden. In that case, set 
C     visibility key to hidden.
      IF (RV.LT.RMN) PRSH(I,K,1)=-1.D0
C     Jump if further test is not needed. If RV.GT.RMX, the 
C     companion surely does not hide that point.
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 2
C     Test further. ANG is position angle of point in question, 
C     measured at the center of the companion.
      ANG=DASIN(DABS(REFX)/RV)
C     Set correct quadrant
      IF (REFY.LT.0.D0) ANG=PI-ANG
      IF (REFX.LT.0.D0) ANG=-ANG
      IF (ANG.LT.0.D0) GO TO 1
C     In range covered by IFCS
      CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
      GO TO 3
C     In range covered by IRCS
    1 CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
C     RW is the projected radius of the eclipsing component at the 
C     position angle ANG. If RW.GT.RV, the point is hidden by the 
C     companion.
    3 ER=RV-RW
      IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
    2 END DO
C      WRITE (NOUT7,1002) I
C	WRITE (NOUT7,1000) (PRSH(I,K,1),K=1,IFC)
C***********************************************
C     Now do the IRC branch. This is the small theta branch.
C
      DO K=1,IRC
      ANGLE=ALP(K,2)
      RDS=RS(K,2)
	IF (ISEQ.EQ.1) THEN
		VYP(I,K,2)=RDS*DCOS(ANGLE)
		VXP(I,K,2)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		ELSE
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		END IF
	END IF
	IF (ISEQ.EQ.2) THEN
		VYP(I,K,2)=RDS*DCOS(ANGLE)
		VXP(I,K,2)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		ELSE
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		END IF
	END IF
	IF (ISEQ.EQ.3) THEN
		VYP(I,K,2)=-RDS*DCOS(ANGLE)
		VXP(I,K,2)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		ELSE
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		END IF
	END IF
	IF (ISEQ.EQ.4) THEN
		VYP(I,K,2)=-RDS*DCOS(ANGLE)
		VXP(I,K,2)=RDS*DSIN(ANGLE)
		IF (ISTAR.EQ.ISV) THEN
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		ELSE
			REFX=VXP(I,K,2)+XG
			REFY=VYP(I,K,2)+YG
		END IF
	END IF
      RV=DSQRT(REFX**2+REFY**2)
      IF (RV.LT.RMN) PRSH(I,K,2)=-1.D0
C     Jump if further test is not needed
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 6
C     Test further
      ANG=DASIN(DABS(REFX)/RV)
C     Set correct quadrant
      IF (REFY.LT.0.D0) ANG=PI-ANG
      IF (REFX.LT.0.D0) ANG=-ANG
      IF (ANG.LT.0.D0) GO TO 4
C     In range covered by IFCS
      CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
      GO TO 5
C     In range covered by IRCS
    4 CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
    5 ER=RV-RW
      IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
    6 CONTINUE
      END DO
C	WRITE (NOUT7,1002) I
C	WRITE (NOUT7,1000) (PRSH(I,K,2),K=1,IRC)
C	IF (I.LT.1000) STOP
C*************************************************
C
C
C     Set visibility keys for boundary of central star as affected by
C     accretion disk
C     Do IFC branch. This is the large theta branch.
C
C     Use IBDY key to leave upper boundary visible if
C     inner accretion disk is optically thin.
C     IBDY is supplied by ACPGB.INF.
      DO 22 K=1,IFC
C        Note following assignments. They are correct, for the assumed
C        axes.
C     The values of X,Y are in star center coods.
         Y=VYP(I,K,1)
         X=VXP(I,K,1)
C     XAV is the X cood. of the horizon pts. on the equator of
C     the central star, in center of mass coods.
C     The projected X cood. of the center of the central star,
C     in center of mass coods., is -DX.
C     If X.LT.(XAV+DX) the point lies above the horizon points
C     on the equator of the central star.
C     Apply alternative test for visibility depending on IBDY.
C     If IBDY=0, the accretion disk is optically thin.
C     If IBDY=1, the accretion disk is optically thick.
	  IF (IBDY.NE.0) THEN
         IF (X.LT.(XAV+DX)) PRSH(ISTAR,K,1)=-1.D0
	  END IF
C     Exit if PRSH has been set.
         IF (PRSH(ISTAR,K,1).EQ.-1.D0) GO TO 22
C     If X.GT.RDTS the point lies below the intersection of the (-X)
C     axis with the bottom rim circle. Must test further.
         IF (X.GT.RDTS) GO TO 10
C     If X.LT.XLL the point lies above the top "corner" of the accretion
C     disk projection. Test further for blocking by upper visible
C     rim circle.
C     Note that X must be referred to center of mass coods. 
	   IF (ISTAR.EQ.ISV) THEN
		XX=X-DX
	   ELSE
		XX=X-DDX
	   END IF 
         IF (XX.LT.XLL) GO TO 15
         IF (IBDY.NE.0) PRSH(I,K,1)=-1.D0
         GO TO 21
C     Test for blocking by lower face of accretion disk
C     Note that AGLD and RD have been set in ACPGB.
C     AGLD has been cyclically shifted to align phase angle 0.0
C     with the minimum value of RD.
C     The angle AGLD is measured from the negative "X" axis
C     (vertically upward on the plane of the sky) in the CCW
C     direction to the vector RDS.
C     Note that X,Y must be measured from the projected center
C     of the gainer. RDS is the distance from the projected center
C     of the ellipse representing the face of the accretion disk.
   10    RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
         ANGL=DASIN(DABS(Y-YRMC)/RDS)
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 11
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   11    CONTINUE
         IF ((ANGL.GE.0.D0).AND.(ANGL.LE.TWPI)) GO TO 12
         IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
         IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   12    DO J=1,NSG-1
         IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 13
         IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LT.AGLD(J+1))) GO TO 20
         END DO
	   J=NSG
	   IF (DABS(ANGL-AGLD(NSG)).LT.1.D-9) GO TO 13
         IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 200
		ANGMX=-1.D0
		ANGMN=10.D0
		DO J=1,NSG
			IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
			IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
		END DO
		DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 260
		END DO
		CALL PMDSTOP
  260		IF (J.GT.1) THEN
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
			IF (AGLD(J).EQ.ANGMX) GO TO 261
		END DO
		CALL PMDSTOP
  261		IF (J.GT.1) THEN
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
   13    ER=RDS-RD(J)
   14    IF ((IBDY.NE.0).AND.(X.LT.(XAV+DX))) THEN
		   IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
	   END IF
         GO TO 21
C     Test for blocking by upper edge of accretion disk.
C     Note that X,Y must be expressed relative to the projected
C     center of the gainer at this point.
   15    RDS=DSQRT(X*X+Y*Y)
         ANGL=DASIN(DABS(Y)/RDS)
         DO 16 J=1,NSG-1
C     Restrict test to visible segment of upper rim
         IF (CSGR(IFLAG,J).LT.0.D0) GO TO 16
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 17
         IF ((ANGL.GT.ANL(J)).AND.(ANGL.LT.ANL(J+1))) GO TO 19
         IF ((ANGL.LT.ANL(J)).AND.(ANGL.GT.ANL(J+1))) GO TO 19
   16    CONTINUE
         J=NSG
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 17
C     Point lies outside boundaries of accretion disk.
         GO TO 21
   17    ER=RDS-RVLD(J)
   18    IF (IBDY.NE.0) THEN
		   IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
	   END IF
         GO TO 21
   19    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RVLD(J+JJ-KK)
         AGRY(JJ)=ANL(J+JJ-KK)
         END DO
		IF (DABS(AGRY(1)-AGRY(2)).LT.1.D-12) THEN
			RDRY(1)=RVLD(J-KK)
			AGRY(1)=ANL(J-KK)
		END IF
		IF (DABS(AGRY(2)-AGRY(3)).LT.1.D-12) THEN
			RDRY(3)=RVLD(J+4-KK)
			AGRY(3)=ANL(J+4-KK)
		END IF
  201    CALL PARAB(AGRY,RDRY)
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
         ER=RDS-RVV
	   IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
	   END IF
         GO TO 21
   20    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RD(J+JJ-KK)
         AGRY(JJ)=AGLD(J+JJ-KK)
         END DO
		IF (DABS(AGRY(1)-AGRY(2)).LT.1.D-12) THEN
			RDRY(1)=RD(J-KK)
			AGRY(1)=AGLD(J-KK)
		END IF
		IF (DABS(AGRY(2)-AGRY(3)).LT.1.D-12) THEN
			RDRY(3)=RD(J+4-KK)
			AGRY(3)=AGLD(J+4-KK)
		END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 711
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(2)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  711		CALL PARAB(AGRY,RDRY)
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
         ER=RVV-RDS
         GO TO 14
   21    CONTINUE
   22 CONTINUE
c      DO K=1,IRC
c		WRITE (NOUT7,1000) VXP(I,K,2),VYP(I,K,2),RS(K,2),
c     $		ALP(K,2)
c	END DO
c	IF (I.LT.1000) STOP
C************************************************
C     Now do IRC branch
      DO 35 K=1,IRC
C     Note following assignments.They are correct, for the assumed
C     axes.
C     The values of X,Y are in star center coods.
	X=VXP(I,K,2)
	Y=VYP(I,K,2)
	ANGLE=ALP(K,2)
	IF (IBDY.NE.0) THEN
		DO J=1,NSG-1
		XREF1=10.D0
		IF ((AGLRF(J).LE.ANGLE).AND.(AGLRF(J+1).GT.ANGLE).AND.
     $		(DABS(AGLRF(J+1)-AGLRF(J)).LT.PIO2/2.D0)) THEN
			IF (ISTAR.EQ.ISV) THEN
				XREF1=PXR(IFLAG,J)+DX
				XREF2=PXR(IFLAG,J+1)+DX
			ELSE
				XREF1=PXR(IFLAG,J)+DDX
				XREF2=PXR(IFLAG,J)+DDX
			END IF
		END IF
		IF ((AGLRF(J).GE.ANGLE).AND.(AGLRF(J+1).LT.ANGLE).AND.
     $		(DABS(AGLRF(J+1)-AGLRF(J)).LT.PIO2/2.D0)) THEN
			IF (ISTAR.EQ.ISV) THEN
				XREF1=PXR(IFLAG,J)+DX
				XREF2=PXR(IFLAG,J+1)+DX
			ELSE
				XREF1=PXR(IFLAG,J)+DDX
				XREF2=PXR(IFLAG+1,2)+DDX
			END IF
		END IF
		IF (XREF1.LT.10.D0) GO TO 47
		END DO
   47		IF (X.GE.XREF1) PRSH(ISTAR,K,2)=-1.D0
		IF ((X.LE.XREF1).AND.(X.GE.XREF2)) THEN
			XREF=XREF1+(XREF2-XREF1)*(ANGLE-AGLRF(J))/(AGLRF(J+1)-
     $			AGLRF(J))
			IF (X.GE.XREF) PRSH(ISTAR,K,2)=-1.D0
		END IF
		IF ((X.GE.XREF1).AND.(X.LE.XREF2)) THEN
			XREF=XREF1+(XREF2-XREF1)*(ANGLE-AGLRF(J))/(AGLRF(J+1)-
     $			AGLRF(J))
			IF (X.GE.XREF) PRSH(ISTAR,K,2)=-1.D0
		END IF
		IF (K.EQ.2) THEN
			IF ((PRSH(ISTAR,1,2).EQ.0.D0).AND.
     $			(PRSH(ISTAR,2,2).EQ.-1.D0)) PRSH(ISTAR,1,2)=-1.D0
		END IF
		IF (K.EQ.IRC) THEN
			IF ((PRSH(ISTAR,K,2).EQ.0.D0).AND.
     $			(PRSH(ISTAR,K-1,2).EQ.-1.D0)) PRSH(ISTAR,K,2)=-1.D0
		END IF
	END IF
C	WRITE (NOUT7,1000) PRSH(I,K,2)
      IF (PRSH(ISTAR,K,2).EQ.-1.D0) GO TO 35
C     If X.LT.RDTS the point lies below the intersection of the (-X)
C     axis with the bottom rim circle. Must test further.
      IF (X.GT.RDTS) GO TO 23
C     If X.LT.XLL the point lies above the top "corner" of the accretion
C     disk projection. Test further for blocking by upper visible
C     rim circle.
C     Note that X must be referred to center of mass coods.
      IF (ISTAR.EQ.ISV) THEN
		XX=X-DX
	ELSE
		XX=X-DDX
	END IF
      IF (XX.LT.XLL) GO TO 28
      IF (IBDY.NE.0) PRSH(I,K,2)=-1.D0
      GO TO 35
   23 RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
      ANGL=DASIN(DABS(Y-YRMC)/RDS)
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 24
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   24 CONTINUE
      IF ((ANGL.GE.0.D0).AND.(ANGL.LE.TWPI)) GO TO 25
      IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
      IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   25 DO J=1,NSG-1
      IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 26
      IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LT.AGLD(J+1))) GO TO 33
      END DO
	J=NSG
      IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 26
      IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 202
      ANGMX=-1.D0
	ANGMN=10.D0
	DO J=1,NSG
		IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
		IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
	END DO
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 460
	END DO
	CALL PMDSTOP
  460 IF (J.GT.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)
			AGRY(2)=AGLD(J)
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 203
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
			GO TO 203
		END IF
	END IF
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 461
	END DO
	CALL PMDSTOP
  461 IF (J.GT.1) THEN
		IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)-TWPI
			AGRY(2)=AGLD(J)-TWPI
			AGRY(3)=AGLD(J+1)
			GO TO 203
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
			GO TO 203
		END IF
	END IF
      CALL PMDSTOP
  202 RDRY(1)=RD(NSG)
      RDRY(2)=RD(1)
	RDRY(3)=RD(2)
	AGRY(1)=AGLD(NSG)
	AGRY(2)=AGLD(1)
	AGRY(3)=AGLD(2)
	GO TO 203
   26 ER=RDS-RD(J)
   27 IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
	END IF
      GO TO 34
C    Test for blocking by upper edge of accretion disk.
C     Note that X,Y must be expressed relative to the
C     projected center of the gainer at this point.
   28 RDS=DSQRT(X*X+Y*Y)
      ANGL=DASIN(DABS(Y)/RDS)
      DO 29 J=1,NSG-1
      IF (CSGR(IFLAG,J).LT.0.D0) GO TO 29
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 30
      IF ((ANGL.GT.ANL(J)).AND.(ANGL.LT.ANL(J+1))) GO TO 32
      IF ((ANGL.LT.ANL(J)).AND.(ANGL.GT.ANL(J+1))) GO TO 32
   29 CONTINUE
      J=NSG
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 30
      GO TO 34
   30 ER=RDS-RVLD(J)
   31 IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
      END IF
      GO TO 34
   32 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RVLD(J+JJ-KK)
      AGRY(JJ)=ANL(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RVLD(J-KK)
		AGRY(1)=ANL(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RVLD(J+4-KK)
		AGRY(3)=ANL(J+4-KK)
	END IF
  203 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
	IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
	END IF
      GO TO 34
   33 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RD(J+JJ-KK)
      AGRY(JJ)=AGLD(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RD(J-KK)
		AGRY(1)=AGLD(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RD(J+4-KK)
		AGRY(3)=AGLD(J+4-KK)
	END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 340
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(2)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  340 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RVV-RDS
      GO TO 27
   34 CONTINUE
   35 CONTINUE
C      WRITE (NOUT7,1000) (PRSH(I,K,2),K=1,IRC)
C	IF (I.LT.1000) STOP
C**********************************************************************
C     End of program section for I.NE.IEC
      GO TO 91
C**********************************************************************
C**********************************************************************
C
C     Reach here if I.EQ.IEC.
C     Note that star I was processed during ICOUNT=1 of PLTB, so it is
C     referenced by IFCS,IRCS, etc. The companion is referenced by IFC,
C     IRC, etc.
C
   90 CONTINUE
C     Calculate eclipsed boundary of companion as affected by central
C     star
C
C     Do IFC branch. This is the large theta branch through the positive
C     X axis.
C
      DO K=1,IFC
      ANGLE=ALP(K,1)
      RDS=RS(K,1)
	IF (ISEQ.EQ.1) THEN
		VYP(3-I,K,1)=RDS*DCOS(ANGLE)
		VXP(3-I,K,1)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,1)-XG
		REFY=VYP(3-I,K,1)-YG
	END IF
	IF (ISEQ.EQ.2) THEN
		VYP(3-I,K,1)=RDS*DCOS(ANGLE)
		VXP(3-I,K,1)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,1)-XG
		REFY=VYP(3-I,K,1)-YG
	END IF
	IF (ISEQ.EQ.3) THEN
		VYP(3-I,K,1)=RDS*DCOS(ANGLE)
		VXP(3-I,K,1)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,1)-XG
		REFY=VYP(3-I,K,1)-YG
	END IF
	IF (ISEQ.EQ.4) THEN
		VYP(3-I,K,1)=RDS*DCOS(ANGLE)
		VXP(3-I,K,1)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,1)-XG
		REFY=VYP(3-I,K,1)-YG
	END IF
C     RV is the distance of the point in question from the center of the
C     companion.
      RV=DSQRT(REFX**2+REFY**2)
      IF (RV.LT.RMN) PRSH(3-I,K,1)=-1.D0
C     Jump if further test is not needed
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 42
C     Test further
      ANG=DASIN(DABS(REFX)/RV)
C     Set correct quadrant
      IF (REFY.LT.0.D0) ANG=PI-ANG
      IF (REFX.LT.0.D0) ANG=-ANG
      IF (ANG.LT.0.D0) GO TO 41
C     In range covered by IFCS
      CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
      GO TO 43
C     In range covered by IRCS
   41 CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
   43 ER=RV-RW
      IF ((PRSH(3-I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,1)=ER
   42 END DO
C     Check to see that the points sequence properly.
C     First calculate the transverse cood. of the center of component
C        3-I.
      YC=(VYP(3-I,1,1)+VYP(3-I,IFC,1))
      IF (3-I.EQ.ISV) THEN
      YC=YC-DY
      ELSE
      YC=YC-DDY
      END IF
C     Now sort into proper order. If YC is (+), the boundary points 
C     should be in descending order. If YC is (-), the points should 
C     be in ascending order.
      IF (YC.LT.0.D0) THEN
         IF (VYP(3-I,IFC,1).GT.VYP(3-I,1,1)) GO TO 100
         DO K=1,IFC
         VYPA(3-I,K,1)=VYP(3-I,IFC-K+1,1)
         VXPA(3-I,K,1)=VXP(3-I,IFC-K+1,1)
		PRSHA(3-I,K,1)=PRSH(3-I,IFC-K+1,1)
         END DO
         DO K=1,IFC
         VXP(3-I,K,1)=VXPA(3-I,K,1)
         VYP(3-I,K,1)=VYPA(3-I,K,1)
		PRSH(3-I,K,1)=PRSHA(3-I,K,1)
         END DO
  100    CONTINUE
      ELSE
         IF (VYP(3-I,IFC,1).LT.VYP(3-I,1,1)) GO TO 101
         DO K=1,IFC
         VXPA(3-I,K,1)=VXP(3-I,IFC-K+1,1)
         VYPA(3-I,K,1)=VYP(3-I,IFC-K+1,1)
		PRSHA(3-I,K,1)=PRSH(3-I,IFC-K+1,1)
         END DO
         DO K=1,IFC
         VXP(3-I,K,1)=VXPA(3-I,K,1)
         VYP(3-I,K,1)=VYPA(3-I,K,1)
		PRSH(3-I,K,1)=PRSHA(3-I,K,1)
         END DO
  101    CONTINUE
      END IF
C**************************************************
C
C     Now do the IRC branch. This is the small theta branch, through
C     the negative X axis.
C
c      DO K=1,IRC
c		WRITE (NOUT7,1000) RS(K,2),ALP(K,2)
c	END DO
c	IF (I.LT.1000) STOP
      DO K=1,IRC
      ANGLE=ALP(K,2)
      RDS=RS(K,2)
C	XREL=RDS*DSIN(ANGLE)
C	YREL=RDS*DCOS(ANGLE)
C	XRELA=XREL-DDX
C	YRELA=YREL-DDY
C	WRITE (NOUT7,1000) XRELA,YRELA,XREL,YREL,RDS,ANGLE
	IF (ISEQ.EQ.1) THEN
		VYP(3-I,K,2)=RDS*DCOS(ANGLE)
		VXP(3-I,K,2)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,2)-XG
		REFY=VYP(3-I,K,2)-YG
	END IF
	IF (ISEQ.EQ.2) THEN
		VYP(3-I,K,2)=RDS*DCOS(ANGLE)
		VXP(3-I,K,2)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,2)-XG
		REFY=VYP(3-I,K,2)-YG
	END IF
	IF (ISEQ.EQ.3) THEN
		VYP(3-I,K,2)=RDS*DCOS(ANGLE)
		VXP(3-I,K,2)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,2)-XG
		REFY=VYP(3-I,K,2)-YG
	END IF
	IF (ISEQ.EQ.4) THEN
		VYP(3-I,K,2)=RDS*DCOS(ANGLE)
		VXP(3-I,K,2)=RDS*DSIN(ANGLE)
		REFX=VXP(3-I,K,2)-XG
		REFY=VYP(3-I,K,2)-YG
	END IF
      RV=DSQRT(REFX**2+REFY**2)
      IF (RV.LT.RMN) PRSH(3-I,K,2)=-1.D0
C     Jump if further test is not needed
      IF ((RV.GT.RMX).OR.(RV.LT.RMN)) GO TO 46
C     Test further
      ANG=DASIN(DABS(REFX)/RV)
C     Set correct quadrant
      IF (REFY.LT.0.D0) ANG=PI-ANG
      IF (REFX.LT.0.D0) ANG=-ANG
      IF (ANG.LT.0.D0) GO TO 44
C     In range covered by IFCS
      CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,1,IRCS,IFCS,NKPS,RW)
      GO TO 45
C     In range covered by IRCS
   44 CALL RHO(ANG,ARYAS,ARYBS,ARYCS,ALPS,RSS,2,IRCS,IFCS,NKPS,RW)
   45 ER=RV-RW
      IF ((PRSH(3-I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,2)=ER
   46 END DO
C      IF (I.LT.1000) STOP
C
      IF (YC.LT.0.D0) THEN
         IF (VYP(3-I,IRC,2).GT.VYP(3-I,1,2)) GO TO 102
         DO K=1,IRC
         VXPA(3-I,K,2)=VXP(3-I,IRC-K+1,2)
         VYPA(3-I,K,2)=VYP(3-I,IRC-K+1,2)
		PRSHA(3-I,K,2)=PRSH(3-I,IRC-K+1,2)
         END DO
         DO K=1,IRC
         VXP(3-I,K,2)=VXPA(3-I,K,2)
         VYP(3-I,K,2)=VYPA(3-I,K,2)
		PRSH(3-I,K,2)=PRSHA(3-I,K,2)
         END DO
  102    CONTINUE
      ELSE
         IF (VYP(3-I,IRC,2).LT.VYP(3-I,1,2)) GO TO 103
         DO K=1,IRC
         VXPA(3-I,K,2)=VXP(3-I,IRC-K+1,2)
         VYPA(3-I,K,2)=VYP(3-I,IRC-K+1,2)
		PRSHA(3-I,K,2)=PRSH(3-I,IRC-K+1,2)
         END DO
         DO K=1,IRC
         VXP(3-I,K,2)=VXPA(3-I,K,2)
         VYP(3-I,K,2)=VYPA(3-I,K,2)
		PRSH(3-I,K,2)=PRSHA(3-I,K,2)
         END DO
  103    CONTINUE
      END IF
C***********************************************
C
C     Set visibility keys for boundary of central star as affected by
C     accretion disk
C     Do the large theta branch first. This goes through the positive
C     X axis.
      DO K=1,IFCS
		ANGLE=ALPS(K,1)
		RDS=RSS(K,1)
		VYP(I,K,1)=RDS*DCOS(ANGLE)
		VXP(I,K,1)=RDS*DSIN(ANGLE)
      END DO
C
C     Now do the small theta branch, through the negative X axis.
      DO K=1,IRCS
		ANGLE=ALPS(K,2)
		RDS=RSS(K,2)
		VYP(I,K,2)=RDS*DCOS(ANGLE)
		VXP(I,K,2)=RDS*DSIN(ANGLE)
      END DO
C     *****************************************************************
C     Determine visibility key values along the large theta branch,
C     through the positive X axis.
C     *****************************************************************
      DO 62 K=1,IFCS
C        Note following assignments. They are correct, for the assumed
C        axes.
         Y=VYP(I,K,1)
         X=VXP(I,K,1)
C     XAV is the X cood. of the horizon pts. on the equator of
C     the central star, in center of mass coods.
C     The projected X cood. of the center of the central star,
C     in center of mass coods., is -DX.
C     If X.LT.(XAV+DX) the point lies above the horizon points
C     on the equator of the central star.
C     Apply alternative test for visibility depending on IBDY.
C     If IBDY=0, the accretion disk is optically thin.
C     If IBDY=1, the accretion disk is optically thick.
         IF (IBDY.NE.0) THEN
		IF ((X.LT.(XAV+DX)).AND.(X.GE.XLS)) PRSH(ISTAR,K,1)=-1.D0
	   END IF
C     Exit if PRSH has been set.
         IF (PRSH(ISTAR,K,1).EQ.-1.D0) GO TO 62
C     The value of RDTS depends on the value of RDMN, which is the
C     smallest value of RD(). Because of the finite number of points
C     defining RD(), the smallest value may be inaccurate for the
C     true value of orbital inclination. For i=90 deg., RDTS should
C     equal XUL and RDMN should equal 0.0.
C     Note that X must be referred to center of mass coods.
         IF (ISTAR.EQ.ISV) THEN
		XX=X-DX
	   ELSE
		XX=X-DDX
	   END IF
         IF (X.GT.RDTS) GO TO 50
C     If X.LT.XLL the point lies above the top "corner" of the accretion
C     disk projection. Test further for blocking by upper visible
C     rim circle.
C     Note that X must be referred to center of mass coods.
         IF (XX.LT.XLL) GO TO 55
         IF (IBDY.NE.0) PRSH(I,K,1)=-1.D0
         GO TO 61
C     Test for blocking by lower face of accretion disk
   50 CONTINUE
         RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
         ANGL=DASIN(DABS(Y-YRMC)/RDS)
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 51
         IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
         IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   51    CONTINUE
         IF ((ANGL.GE.0.D0).AND.(ANGL.LE.TWPI)) GO TO 52
         IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
         IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   52    CONTINUE
         DO J=1,NSG-1
         IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 53
         IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LT.AGLD(J+1))) GO TO 60
         END DO
	   J=NSG
         IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 53
	   IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 205
C     Note: May have situation in which the rim points do not
C     happen to hit the point for which the position angle = TWPI.
C     Consequently, a calculated value of ANGL may fall through
C     the cracks. Need to identify the largest entry in AGLD and
C     see if it is less than TWPI.
          ANGMX=-1.0D0
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
			IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
				RDRY(1)=RD(J-1)
				RDRY(2)=RD(J)
				RDRY(3)=RD(J+1)
				AGRY(1)=AGLD(J-1)
				AGRY(2)=AGLD(J)
				AGRY(3)=AGLD(J+1)+TWPI
				GO TO 206
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
				GO TO 206
			END IF
		END IF
C     Correspondingly, the value of ANGL may fall between the largest
C     value of AGLD() and the smallest, but be smaller than the smallest.
          DO J=1,NSG
			IF (AGLD(J).EQ.ANGMX) GO TO 221
		END DO
		CALL PMDSTOP
  221 IF (J.GT.1) THEN
		IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
				RDRY(1)=RD(J-1)
				RDRY(2)=RD(J)
				RDRY(3)=RD(J+1)
				AGRY(1)=AGLD(J-1)-TWPI
				AGRY(2)=AGLD(J)-TWPI
				AGRY(3)=AGLD(J+1)
				GO TO 206
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
				GO TO 206
		END IF
	END IF
         CALL PMDSTOP
  205    RDRY(1)=RD(NSG)
         RDRY(2)=RD(1)
	   RDRY(3)=RD(2)
	   AGRY(1)=AGLD(NSG)
	   AGRY(2)=AGLD(1)
	   AGRY(3)=AGLD(2)
	   GO TO 206
   53    ER=RDS-RD(J)
   54    IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
         GO TO 61
C     Test for blocking by upper edge of accretion disk
   55    RDS=DSQRT(X*X+Y*Y)
         ANGL=DASIN(Y/RDS)
         DO 56 J=KSTRT,KSTOP
C     Restrict test to visible segment of upper rim
         IF (CSGR(IFLAG,J).LT.0.D0) GO TO 56
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 57
         IF ((ANGL.GT.ANL(J)).AND.(ANGL.LT.ANL(J+1))) GO TO 59
         IF ((ANGL.LT.ANL(J)).AND.(ANGL.GT.ANL(J+1))) GO TO 59
   56    CONTINUE
         J=KLS
         IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 57
C     Point lies outside boundaries of accretion disk.
         GO TO 61
   57    ER=RDS-RVLD(J)
   58    IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
         GO TO 61
   59    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RVLD(J+JJ-KK)
         AGRY(JJ)=ANL(J+JJ-KK)
         END DO
		IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
			RDRY(1)=RVLD(J-KK)
			AGRY(1)=ANL(J-KK)
		END IF
		IF(DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
			RDRY(3)=RVLD(J+4-KK)
			AGRY(3)=ANL(J+4-KK)
		END IF
  206    CALL PARAB(AGRY,RDRY)
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
         ER=RDS-RVV
         IF ((PRSH(I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,1)=ER
         GO TO 61
   60    KK=1
         IF ((NSG-J).LT.2) KK=2
         DO JJ=1,3
         RDRY(JJ)=RD(J+JJ-KK)
         AGRY(JJ)=AGLD(J+JJ-KK)
         END DO
		IF (DABS(AGRY(2)-AGRY(1)).LT.1.D-12) THEN
			RDRY(1)=RD(J-KK)
			AGRY(1)=AGLD(J-KK)
		END IF
		IF (DABS(AGRY(2)-AGRY(3)).LT.1.D-12) THEN
			RDRY(3)=RD(J+4-KK)
			AGRY(3)=AGLD(J+4-KK)
		END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 601
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  601    CALL PARAB(AGRY,RDRY)
         RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
C     Test for point below the lower rim.
		IF (XX.LT.XUL) THEN
			ER=RVV-RDS
		ELSE
			ER=0.D0
		END IF
         GO TO 54
   61    CONTINUE
   62 CONTINUE
C***********************************************
C     Now determine visibility key values along the small theta branch,
C     through the negative X axis.
      DO 75 K=1,IRCS
C     Note following assignments.	VXP() and VYP() are projected coods.
C     of the star boundaries, relative to the star center.
      Y=VYP(I,K,2)
      X=VXP(I,K,2)
C     Note that X must be referred to center of mass coods. for test
C     against XLL. Set the coordinate in XX.
      IF (ISTAR.EQ.ISV) THEN
		XX=X-DX
	ELSE
		XX=X-DDX
	END IF
      IF (IBDY.NE.0) THEN
		IF (XX.GT.XLL) THEN
		IF (ISTAR.EQ.ISV) THEN
			XTST=XAV+DX
		ELSE
			XTST=XAV+DDX
		END IF
		IF ((X.LT.XTST).AND.(X.GE.XLS)) PRSH(ISTAR,K,2)=-1.D0
		END IF
	END IF
	IF (PRSH(ISTAR,K,2).EQ.-1.D0) GO TO 75
      IF (X.GT.RDTS) GO TO 63
      IF (XX.LT.XLL) GO TO 68
      IF (IBDY.NE.0) PRSH(I,K,2)=-1.D0
      GO TO 75
   63 RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
      ANGL=DASIN(DABS(Y-YRMC)/RDS)
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 64
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   64 CONTINUE
      IF ((ANGL.GE.0.D0).AND.(ANGL.LE.TWPI)) GO TO 65
      IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
      IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   65 CONTINUE
      DO J=1,NSG-1
      IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 66
      IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LT.AGLD(J+1))) GO TO 73
      END DO
	J=NSG
      IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 66
      IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 207
	ANGMX=-1.D0
	ANGMN=10.D0
	DO J=1,NSG
		IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
		IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
	END DO
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 120
	END DO
	CALL PMDSTOP
  120 IF (J.GT.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)
			AGRY(2)=AGLD(J)
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 208
		END IF
	END IF
	IF (J.EQ.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J)
			RDRY(2)=RD(J+1)
			RDRY(3)=RD(J+2)
			AGRY(1)=AGLD(J)
			AGRY(2)=AGLD(J+1)+TWPI
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 208
		END IF
	END IF
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 121
	END DO
	CALL PMDSTOP
  121 IF (J.GT.1) THEN
		IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)-TWPI
			AGRY(2)=AGLD(J)-TWPI
			AGRY(3)=AGLD(J+1)
			GO TO 208
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
			GO TO 208
		END IF
	END IF
      CALL PMDSTOP
  207 RDRY(1)=RD(NSG)
      RDRY(2)=RD(1)
	RDRY(3)=RD(2)
	AGRY(1)=AGLD(NSG)
	AGRY(2)=AGLD(1)
	AGRY(3)=AGLD(2)
	GO TO 208
   66 ER=RDS-RD(J)
   67 IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
      END IF
      GO TO 74
C     Test for blocking by upper edge of accretion disk.
C     Note that X,Y are expressed relative to the
C     projected center of the gainer at this point.
   68 RDS=DSQRT(X*X+Y*Y)
      ANGL=DASIN(DABS(Y)/RDS)
	IF (VI.GT.1.57D0) THEN
		IF ((X.GT.XLS).AND.(X.LT.XGR)) THEN
				IF (PRSH(I,K,2).GE.0.D0) PRSH(I,K,2)=-1.D0
				GO TO 75
		END IF
	END IF
      DO 69 J=KSTRT,KSTOP
      IF (CSGR(IFLAG,J).LT.0.D0) GO TO 69
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 70
      IF ((ANGL.GT.ANL(J)).AND.(ANGL.LT.ANL(J+1))) GO TO 72
      IF ((ANGL.LT.ANL(J)).AND.(ANGL.GT.ANL(J+1))) GO TO 72
   69 CONTINUE
      J=KLS
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 70
	CALL PMDSTOP
   70 ER=RDS-RVLD(J)
   71 IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
      END IF
      GO TO 74
   72 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RVLD(J+JJ-KK)
      AGRY(JJ)=ANL(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
	RDRY(1)=RVLD(J-KK)
	AGRY(1)=ANL(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
	RDRY(3)=RVLD(J+4-KK)
	AGRY(3)=ANL(J+4-KK)
	END IF
  208 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
	IF (IBDY.NE.0) THEN
		IF ((PRSH(I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(I,K,2)=ER
      END IF
      GO TO 74
   73 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RD(J+JJ-KK)
      AGRY(JJ)=AGLD(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RD(J-KK)
		AGRY(1)=AGLD(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RD(J+4-KK)
		AGRY(4)=AGLD(J+4-KK)
	END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 740
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(2)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  740 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
      GO TO 67
   74 CONTINUE
   75 CONTINUE
C************************************************
C     Now determine effect of accretion disk on companion boundary
C     Note that VXP,VYP in following are not in center of mass coods.
C
C     Do IFC branch for companion. This is the large theta branch,
C     passing through the positive X axis.
      DO 360 K=1,IFC
      IF (ISTAR.NE.ISV) THEN
C     Note that VXP() and VYP() are coods. relative to center of
C     companion.
C     X and Y are in center of mass coods.
		Y=VYP(3-I,K,1)-DY
		X=VXP(3-I,K,1)-DX
      ELSE
		Y=VYP(3-I,K,1)-DDY
		X=VXP(3-I,K,1)-DDX
      END IF
C     The following quantities all must be in center of mass coordinates.
C     If any one of the following four tests is true, the point is 
C     outside the projected image of the accretion disk.
      IF (X.LT.XLS) GO TO 36
      IF (X.GT.XGR) GO TO 36
      IF (Y.LT.YLL) GO TO 36
      IF (Y.GT.YUL) GO TO 36
C     If the following test is true, the point may be hidden by the
C     lower rim of the accretion disk.
      IF (X.GT.XUL) GO TO 37
C     If the foillowing test is true, the point may be hidden by the
C     upper rim of the accretion disk.
      IF (X.LT.XLL) GO TO 38
C     NTYPE specifies the shape of the outer rim.
C     If NTYPE=1, the outer rim is cylindrical. This case no longer
C     is implemented.
C     If NTYPE=2, the outer rim is a half torus.
C
C     If we arrive here, the point X cood. would place it within the
C     shadow of the accretion disk. Must test further.
	IF (NTYPE.EQ.1) THEN
      IF ((Y.LT.YUL).AND.(Y.GT.YLL)) PRSH(3-I,K,1)=-1.D0
      GO TO 360
	END IF
	IF (NTYPE.NE.2) CALL PMDSTOP
	IF (X.LE.RXHZG(1)) GO TO 36
	IF (X.GT.RXHZG(NRMHZ)) GO TO 37
C     Must allow for convex projected boundary of rim.
	DO L=1,NRM-1
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
	IF ((Y.LT.VY1).AND.(Y.GT.VY2)) PRSH(3-I,K,1)=-1.D0
      END IF
	END DO
	GO TO 360
C
C
C     Test for blocking by lower face of accretion disk
C     RDS is the distance from the center of the accretion disk
C     to the point on the boundary of the companion.
C     Note that XRMC, YRMC are relative to the center of the
C     accretion disk. Currently, X and Y are in center of mass
C     coods. Must set them to coods. relative to the center
C     of the accretion disk. Note that (X,Y) represent a point on 
C     the horizon of the companion. To produce coods. for use with
C     XRMX, YRMC, we must set the origin at the center of the gainer.
C     This means applying (DX,DY) and (DDX,DDY) in the opposite sense
C     from that expected.
   37 IF (ISTAR.EQ.ISV) THEN
		X=X+DX
		Y=Y+DY
	ELSE
		X=X+DDX
		Y=Y+DDY
	END IF
C     RDS is the distance from the projected center of the lower face.
      RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
C     ANGL measured relative to X axis. At this point ambiguous relative
C     to direction.
      ANGL=DASIN(DABS(Y-YRMC)/RDS)
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 39
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   39 CONTINUE
C     Return X and Y to center of mass coods.
C     Note counterintuitive use of (DX,DY) and (DDX,DDY) is correct.
      IF (ISTAR.EQ.ISV) THEN
		X=X-DX
		Y=Y-DY
	ELSE
		X=X-DDX
		Y=Y-DDY
	END IF
C
C     For eventual comparison with AGLD, measure ANGL from negative
C     X axis. This accords with definition of AGLD set in ACPGB.
	IF (ANGL.GE.0.D0.AND.(ANGL.LE.TWPI)) GO TO 40
	IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
	IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   40 CONTINUE
      DO J=1,NSG-1
      IF (DABS(ANGL-AGLD(J)).LT.1.D-8) GO TO 76
      IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LE.AGLD(J+1))) GO TO 77
	END DO
	J=NSG
      IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 76
      IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 209
      ANGMX=-1.D0
	ANGMN=10.D0
	DO J=1,NSG
		IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
		IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
	END DO
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 320
	END DO
      CALL PMDSTOP
  320 IF (J.GT.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)
			AGRY(2)=AGLD(J)
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 210
		END IF
	END IF
	IF (J.EQ.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J)
			RDRY(2)=RD(J+1)
			RDRY(3)=RD(J+2)
			AGRY(1)=AGLD(J)
			AGRY(2)=AGLD(J+1)+TWPI
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 210
		END IF
	END IF
	DO J=1,NSG
	IF (AGLD(J).EQ.ANGMX) GO TO 321
	END DO
	CALL PMDSTOP
  321 IF (J.GT.1) THEN
		IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)-TWPI
			AGRY(2)=AGLD(J)-TWPI
			AGRY(3)=AGLD(J+1)
			GO TO 210
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
			GO TO 210
		END IF
	END IF
	CALL PMDSTOP
  209 RDRY(1)=RD(NSG)
      RDRY(2)=RD(1)
	RDRY(3)=RD(2)
	AGRY(1)=AGLD(NSG)
	AGRY(2)=AGLD(1)
	AGRY(3)=AGLD(2)
	GO TO 210
   76 ER=RDS-RD(J)
   78 IF ((PRSH(3-I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,1)=ER
      GO TO 360
C     Test for blocking by upper edge of accretion disk
   38 RDS=DSQRT((X+DDX)**2+(Y+DDY)**2)
      ANGL=DASIN((Y+DDY)/RDS)
      DO 79 J=KSTRT,KSTOP
C     Restrict test to visible segment of upper rim
      IF (CSGR(IFLAG,J).LT.0.D0) GO TO 79
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 80
      IF ((ANGL.GT.ANL(J)).AND.(ANGL.LE.ANL(J+1))) GO TO 81
      IF ((ANGL.LT.ANL(J)).AND.(ANGL.GE.ANL(J+1))) GO TO 81
   79 CONTINUE
      J=KLS
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 80
      CALL PMDSTOP
   80 ER=RDS-RVLD(J)
   82 IF ((PRSH(3-I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,1)=ER
      GO TO 360
   81 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RVLD(J+JJ-KK)
      AGRY(JJ)=ANL(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RVLD(J-KK)
		AGRY(1)=ANL(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RVLD(J+4-KK)
		AGRY(3)=ANL(J+4-KK)
	END IF
  210 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
      IF ((PRSH(3-I,K,1).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,1)=ER
      GO TO 360
   77 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RD(J+JJ-KK)
      AGRY(JJ)=AGLD(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RD(J-KK)
		AGRY(1)=AGLD(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RD(J+4-KK)
		AGRY(3)=AGLD(J+4-KK)
	END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 361
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(2)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  361 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
      GO TO 78
   36 CONTINUE
  360 CONTINUE
C************************************************
C
C     Now do IRC branch of companion. Small theta branch, through 
C     (-) X axis.
C
      DO 89 K=1,IRC
      IF (ISTAR.NE.ISV) THEN
C     Note that VXP() and VYP() are coods. relative to center of 
C     companion.
C     After the following transformations
C     X and Y will be coods. in center of mass system.
		Y=VYP(3-I,K,2)-DY
		X=VXP(3-I,K,2)-DX
      ELSE
		Y=VYP(3-I,K,2)-DDY
		X=VXP(3-I,K,2)-DDX
      END IF
C     The following quantities must all be in center of mass coods.
C     If any one of following four tests is true, the point is outside
C     the projected limits of the accretion disk. The point is not
C     hidden.
      IF (X.LT.XLS) GO TO 89
      IF (X.GT.XGR) GO TO 89
      IF (Y.LT.YLL) GO TO 89
      IF (Y.GT.YUL) GO TO 89
	IF (VI.GT.1.57D0) THEN
		IF ((VXP(3-I,K,2).GT.XLS).AND.(VXP(3-I,K,2).LT.XGR)) THEN
			IF (PRSH(3-I,K,2).GE.0.D0) PRSH(3-I,K,2)=-1.D0
			GO TO 89
		END IF
	END IF
C     If the following test is true, must test further for blocking by
C     lower rim of accretion disk.
      IF (X.GT.XUL) GO TO 83
C     If the following test is true, must test further for blocking by
C     upper rim of accretion disk.
      IF (X.LT.XLL) GO TO 86
C     NTYPE specifies the shape of the outer rim.
C     If NTYPE=1, the outer rim is cylindrical. This case no longer
C     is implemented.
C     If NTYPE=2, the outer rim is half torus.
C
C     If we arrive here, the point X cood. would place it within the
C     shadow of the accretion disk. Must test further.
	IF (NTYPE.EQ.1) THEN
      IF ((Y.LT.YUL).AND.(Y.GT.YLL)) PRSH(3-I,K,2)=-1.D0
      GO TO 89
	END IF
	IF (NTYPE.NE.2) CALL PMDSTOP
	IF (X.LE.RXHZG(1)) GO TO 89
	IF (X.GT.RXHZG(NRMHZ)) GO TO 83
C     Must allow for convex projected boundary of rim.
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
	IF ((Y.LT.VY1).AND.(Y.GT.VY2)) PRSH(3-I,K,2)=-1.D0	
      END IF
	END DO
	GO TO 89
C
C
C     Test for blocking by lower face of accretion disk
C     RDS is the distance of the point from the center of the
C     accretion disk. ANGL is the position angle of the point
C     relative to the negative "X" axis.
C     Note that XRMC, YRMC are relative to the center of the
C     accretion disk. Note that (X,Y) represent a point on
C     the horizon of the companion. To produce coods. for use with
C     (XRMC,YRMC) we must set the origin at the center of the gainer.
C     This means applying (DX,DY) and (DDX,DDY) in the opposite sense
C     from that expected. 
C     Currently, X and Y are in center of mass coods.
   83 IF (ISTAR.EQ.ISV) THEN
		X=X+DX
		Y=Y+DY
	ELSE
		X=X+DDX
		Y=Y+DDY
	END IF
C     RDS is the distance of the point on the horizon of the companion
C     from the center of the lower face of the accretion disk.
      RDS=DSQRT((X-XRMC)**2+(Y-YRMC)**2)
C     ANGL is measured relative to the X axis. Currently ambiguous on
C     which branch of X axis.
      ANGL=DASIN(DABS(Y-YRMC)/RDS)
C     For eventual comparison with AGLD, measure ANGL from negative
C     X axis. This accords with definition of AGLD established in ACPGB.
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).LT.0.D0)) GO TO 84
      IF (((Y-YRMC).LT.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=PI-ANGL
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).GE.0.D0)) ANGL=ANGL+PI
      IF (((Y-YRMC).GE.0.D0).AND.((X-XRMC).LT.0.D0)) ANGL=TWPI-ANGL
   84 CONTINUE
C     Return X,Y to center of mass coods.
C     Note counterintuitive use of (DX,DY) and (DDX,DDY)
      IF (ISTAR.EQ.ISV) THEN
		X=X-DX
		Y=Y-DY
	ELSE
		X=X-DDX
		Y=Y-DDY
	END IF
C     For eventual comparison with AGLD, measure ANGL from negative
C     X axis. This accords with definition of AGLD set in ACPGB      
	IF (ANGL.GE.0.D0.AND.(ANGL.LE.TWPI)) GO TO 85      
	IF (ANGL.LT.0.D0) ANGL=ANGL+TWPI
	IF (ANGL.GT.TWPI) ANGL=ANGL-TWPI
   85 CONTINUE
      DO J=1,NSG-1
      IF (DABS(ANGL-AGLD(J)).LT.2.D-6) GO TO 87
      IF ((ANGL.GT.AGLD(J)).AND.(ANGL.LE.AGLD(J+1))) GO TO 88
      END DO
	J=NSG
      IF (DABS(ANGL-AGLD(NSG)).LT.1.D-8) GO TO 87
      IF ((ANGL.GT.AGLD(NSG)).AND.(ANGL.LT.AGLD(1))) GO TO 211
      ANGMX=-1.D0
	ANGMN=10.D0
	DO J=1,NSG
		IF (AGLD(J).GT.ANGMX) ANGMX=AGLD(J)
		IF (AGLD(J).LT.ANGMN) ANGMN=AGLD(J)
	END DO
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 330
	END DO
	CALL PMDSTOP
  330 IF (J.GT.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)
			AGRY(2)=AGLD(J)
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 212
		END IF
      END IF
      IF (J.EQ.1) THEN
		IF ((AGLD(J).LT.ANGL).AND.(AGLD(J+1).LT.ANGL)) THEN
			RDRY(1)=RD(J)
			RDRY(2)=RD(J+1)
			RDRY(3)=RD(J+2)
			AGRY(1)=AGLD(J)
			AGRY(2)=AGLD(J+1)+TWPI
			AGRY(3)=AGLD(J+1)+TWPI
			GO TO 212
		END IF
      END IF
	DO J=1,NSG
		IF (AGLD(J).EQ.ANGMX) GO TO 331
	END DO
	CALL PMDSTOP
  331 IF (J.GT.1) THEN
		IF ((AGLD(J).GT.ANGL).AND.(AGLD(J+1).GT.ANGL)) THEN
			RDRY(1)=RD(J-1)
			RDRY(2)=RD(J)
			RDRY(3)=RD(J+1)
			AGRY(1)=AGLD(J-1)-TWPI
			AGRY(2)=AGLD(J)-TWPI
			AGRY(3)=AGLD(J+1)
			GO TO 212
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
			GO TO 212
		END IF
	END IF
	CALL PMDSTOP
  211 RDRY(1)=RD(NSG)
      RDRY(2)=RD(1)
	RDRY(3)=RD(2)
	AGRY(1)=AGLD(NSG)
	AGRY(2)=AGLD(1)
	AGRY(3)=AGLD(2)
	GO TO 212
   87 ER=RDS-RD(J)
   93 IF ((PRSH(3-I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,2)=ER
      GO TO 89
C
C
C**********************************************************************
C     Test for blocking by upper edge of accretion disk
C     Calculate dist. of point from center of accretion disk.
   86 RDS=DSQRT((X+DDX)**2+(Y+DDY)**2)
      ANGL=DASIN((Y+DDY)/RDS)
      DO 94 J=KSTRT,KSTOP
C     Restrict test to visible segment of upper rim
      IF (CSGR(IFLAG,J).LT.0.D0) GO TO 94
      IF (DABS(ANGL-ANL(J)).LT.1.D-7) GO TO 95
      IF ((ANGL.GT.ANL(J)).AND.(ANGL.LE.ANL(J+1))) GO TO 96
      IF ((ANGL.LT.ANL(J)).AND.(ANGL.GE.ANL(J+1))) GO TO 96
   94 CONTINUE
	IF (KSTRT.EQ.1) THEN
		IF ((ANGL.GT.ANL(1)).AND.(ANGL.LE.ANL(NSG)))THEN
			RDRY(1)=RVLD(NSG)
			RDRY(2)=RVLD(1)
			RDRY(3)=RVLD(3)
			AGRY(1)=ANL(NSG)
			AGRY(2)=ANL(1)
			AGRY(3)=ANL(2)
			GO TO 212
		END IF
	END IF
	IF ((ANL(1).LT.ANL(NSG)).AND.(ANGL.GT.ANL(NSG)))THEN
		J=NSG-1
		GO TO 95
	END IF
	IF ((ANGL.LT.ANL(KSTOP)).AND.(KSTOP.LT.NSG).AND.
     $	(ANL(KSTOP).GT.ANL(KSTOP+1))) THEN
		J=KSTOP+1
		GO TO 95
	END IF
      ER=-0.99999
	GO TO 97
   95 ER=RDS-RVLD(J)
   97 IF ((PRSH(3-I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,2)=ER
      GO TO 89
   96 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RVLD(J+JJ-KK)
      AGRY(JJ)=ANL(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RVLD(J-KK)
		AGRY(1)=ANL(J-KK)
	END IF
	IF (DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RVLD(J+4-KK)
		AGRY(3)=ANL(J+4-KK)
	END IF
  212 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
      IF ((PRSH(3-I,K,2).GE.0.D0).AND.(ER.LT.0.D0)) PRSH(3-I,K,2)=ER
      GO TO 89
C**********************************************************************
C
   88 KK=1
      IF ((NSG-J).LT.2) KK=2
      DO JJ=1,3
      RDRY(JJ)=RD(J+JJ-KK)
      AGRY(JJ)=AGLD(J+JJ-KK)
      END DO
	IF (DABS(RDRY(1)-RDRY(2)).LT.1.D-12) THEN
		RDRY(1)=RD(J-KK)
		AGRY(1)=AGLD(J-KK)
	END IF
	IF(DABS(RDRY(2)-RDRY(3)).LT.1.D-12) THEN
		RDRY(3)=RD(J+4-KK)
		AGRY(3)=AGLD(J+4-KK)
	END IF
	IF ((AGRY(1).LT.AGRY(2)).AND.(AGRY(2).LT.AGRY(3))) GO TO 890
	IF ((AGRY(2).LT.AGRY(1)).AND.(AGRY(3).LT.AGRY(1))) THEN
		IF (ANGL.GT.AGRY(1)) THEN
			AGRY(2)=AGRY(2)+TWPI
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
		END IF
	END IF
	IF ((AGRY(2).GT.AGRY(1)).AND.(AGRY(3).LT.AGRY(2))) THEN
		IF (ANGL.GT.AGRY(2)) THEN
			AGRY(3)=AGRY(3)+TWPI
		END IF
		IF (ANGL.LT.AGRY(3)) THEN
			AGRY(1)=AGRY(1)-TWPI
			AGRY(2)=AGRY(2)-TWPI
		END IF
	END IF
  890 CALL PARAB(AGRY,RDRY)
      RVV=RDRY(1)+RDRY(2)*ANGL+RDRY(3)*ANGL*ANGL
      ER=RDS-RVV
      GO TO 93
   89 CONTINUE
C**********************************************************************
C     End of program section for I.EQ.IEC.
C     The program section for I.NE.IEC rejoins the program continuation
C     here.
C**********************************************************************
C**********************************************************************
   91 CONTINUE
   92 CONTINUE
C     Now return arrays to original (unshifted) sequence
      DO J=1,NSG
		IF ((DABS(RVLD(1)-RVLSVA).LT.1.D-10).AND.
     $		(DABS(RVLD(NSG)-RVLSVA).LT.1.D-10)) THEN
			GO TO 9
		ELSE
			RVLSV=RVLD(NSG)
			ANLSV=ANL(NSG)
			CSGRSV=CSGR(IFLAG,NSG)
			PRRHSV=PRRH(IFLAG,NSG)
			DO K=1,NSG-1
				RVLD(NSG-K+1)=RVLD(NSG-K)
				ANL(NSG-K+1)=ANL(NSG-K)
				CSGR(IFLAG,NSG-K+1)=CSGR(IFLAG,NSG-K)
				PRRH(IFLAG,NSG-K+1)=PRRH(IFLAG,NSG-K)
			END DO
			RVLD(1)=RVLSV
			ANL(1)=ANLSV
			CSGR(IFLAG,1)=CSGRSV
			PRRH(IFLAG,1)=PRRHSV
		END IF
      END DO
	  CALL PMDSTOP
    9 CONTINUE
C     Test bottom rim for intersection with limb of central star.
C     XGR marks the most positive X value on the lower accretion disk 
C     rim. RSS() are radii to the large theta branch of the central
C     star, and ALPS() are the corresponding position angles, in the
C     first and fourth quadrants. In the second and third quadrants
C     the radii are in RS() and the corresponding position angles are
C     in ALP().
C     Since we use RSS(J,1) and ALPS(J,1),
C     we automatically select the proper branch of the lower rim for test.
	IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) THEN
		ILMT=IFCS
	ELSE
		ILMT=IFC
	END IF
C     Choose successive points on the lower branch of the lower rim.
C     Test each one for possible blocking from view by the limb of
C     the gainer.
	DO K=1,NSG
C     Sequence through the points on the large theta branch of the
C     gainer. These are the points that can overlap the lower rim and
C     block the observer's view of the lower rim.
C     Note that AGLRF(K) are position angles, on the plane of the sky,
C     of points on the lower rim of the accretion disk, and RDRF(K) are
C     the corresponding radial distances to the points, on the plane of
C     the sky, from the projected center of the gainer.
      DO J=1,ILMT-1
		IF ((ISEQ.EQ.1).OR.(ISEQ.EQ.4)) THEN
			ANGL1=ALP(J,1)
			RD1=RS(J,1)
			ANGL2=ALP(J+1,1)
			RD2=RS(J+1,1)
		ELSE
			ANGL1=ALPS(J,1)
			RD1=RSS(J,1)
			ANGL2=ALPS(J+1,1)
			RD2=RSS(J+1,1)
		END IF
C     If the following test is met, the two position angles, ANGL1 and
C     ANGL2, include between them the position angle of the rim point.
		IF ((ANGL1.LE.AGLRF(K)).AND.(ANGL2.GE.AGLRF(K))) THEN
			IF ((RD1.GT.RDRF(K)).OR.(RD2.GT.RDRF(K))) THEN
C     Only the far part of the lower rim is blocked by the lower edge
C     of the gainer.
C				IF (K.LT.NSG/2+1) THEN
					PRRH(NRM,K)=-1.D0
C				END IF
			END IF
		END IF
	END DO
	END DO
      RETURN
      END
