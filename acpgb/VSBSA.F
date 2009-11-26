      SUBROUTINE VSBSA(DX,DY,DDX,DDY,PXF,PYF,PRFH,IFC,ALP,RS,XRMC,ISTAR,
     $ISV,NRING,NSG,MRING,MSG,MPHM,NOUT7)
C     Subroutine to set visibility data for face pixels, based on view
C     blocking by central star.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (MTR    =    3)
      DIMENSION PXF(MRING,MSG),PYF(MRING,MSG),PRFH(MRING,MSG),
     $RS(MPHM,MTR),ALP(MPHM,MTR)
      DATA PIO2/1.5707963267948966192D0/
      IF (NRING.EQ.0) RETURN
      YMX=-1.D0
C     The arrays PXF(), PYF() contain the coordinates of the lower
C     face pixels.
C     First index in PXF( , ) or PYF( , ) refers to lower face circle.
C     Lower face circles are numbered from the inner face circle
C     outward.
C     The second index refers to the azimuthal coordinate.
C     At entry to this SRT, the origin for PXF(), PYF() is at the system
C     center of mass.
C     Calculate Y cood. relative to center of gainer.
C     Consider the innermost circle (first index=1).
      DO J=1,NSG
      IF (ISTAR.EQ.ISV) THEN
      Y=PYF(1,J)+DY
      ELSE
      Y=PYF(1,J)+DDY
      END IF
      IF (DABS(Y).LT.YMX) GO TO 5
      YMX=DABS(Y)
C     Set XREF to the X cood. corresponding to YMX.
      IF (ISTAR.EQ.ISV) THEN
      XREF=PXF(1,J)+DX
      ELSE
      XREF=PXF(1,J)+DDX
      END IF
   5  END DO
C     XREF now contains the X cood. corres. to the maximum
C     sideways extent of the innermost face circle, YMX.
C     YMX approximately marks the projection of the transverse axis
C     of the lower accretion disk face. XREF approximately marks
C     the projected location of the center of the gainer. Since the
C     positive X axis points downward, any face point whose X cood.
C     is less than XREF will be visible.
C     The algorithm is to determine whether the projected radius of the
C     central star is greater than the projected distance of the face
C     point in question from the center of the gainer. The comparison
C     must be for the same position angles. If the projected radius is 
C     greater, then the point is hidden, and the visibility key is set 
C     to a negative error value..
C
C     Now loop over all face pixels to set visibility keys.
      DO K=1,NRING
         DO J=1,NSG
		RR=RS(1,1)
		XI=ALP(1,1)
C     If face point has X cood. above uppermost proj. point on equator
C     of central star, the star does not block that point.
C     Obtain X,Y coods. relative to the center of the gainer.
         IF (ISTAR.EQ.ISV) THEN
         X=PXF(K,J)+DX
         Y=PYF(K,J)+DY
         ELSE
         X=PXF(K,J)+DDX
         Y=PYF(K,J)+DDY
         END IF
C     RR is proj.dist. of face point from proj. center of system
		RR=DSQRT(X*X+Y*Y)
C     If X is more negative than XREF, the point is not hidden by the
C     gainer.
         IF (X.LE.XREF) GO TO 3
         IF (K.EQ.1) GO TO 4
C     XI is the value of the position angle of the face point
C     relative to the horizontal axis.
C     Chose the absolute value so we can compare with the array ALP().
		XI=DASIN(X/RR)
		IF (ALP(IFC,1).LT.0.D0) XI=-DABS(XI)
    4		DO L=2,IFC
			IF ((ALP(L-1,1).LE.XI).AND.(ALP(L,1).GE.XI)) GO TO 2
			IF ((ALP(L-1,1).GE.XI).AND.(ALP(L,1).LE.XI)) GO TO 2
		END DO
C     Do linear interpolation between bracketing values of ALP().
    2		T1=XI-ALP(L-1,1)
		T2=ALP(L,1)-ALP(L-1,1)
		R1=RS(L-1,1)
		R2=RS(L,1)-RS(L-1,1)
C     R is interpolated projected radius of central star at position 
C     angle XI. Note that the central star may be rotationally 
C     distorted.
		R=R1+T1/T2*R2
C     ER is (-) for view to be blocked		
         ER=RR-R
         IF ((ER.GE.0.D0).AND.(K.NE.NRING)) GO TO 3
C     Guarantee that the upper edge of the outermost face ring will 
C     be plotted.
         IF ((X.LE.XRMC).AND.(K.EQ.NRING)) GO TO 3
         PRFH(K,J)=ER
    3    END DO
      END DO
   11 RETURN
      END
