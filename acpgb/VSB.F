      SUBROUTINE VSB(PXF,PYF,PRFH,XRMC,YRMC,ANG,RD,
     $NRING,NSG,DX,DY,DDX,DDY,XLSL,ISTAR,ISV,VI,NOUT7)
C     Subroutine to set visibility data for face pixels, based on view
C     blocking by disk itself.
C     XRMC is the projected distance between the center of the ellipse
C     representing the projected bottom edge of the accretion disk
C     and the projected center of the gainer.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INCLUDE 'ACOMA.FOR'
	PARAMETER (MTR   =   3)
      DIMENSION PXF(MRING,MSG),PYF(MRING,MSG),PRFH(MRING,MSG),
     $ANG(MSG),RD(MSG),VX(MTR),VR(MTR)
      DATA PI,TWPI/3.1415926535898D0,6.2831853071796D0/
	DATA PIO2/1.5707963267948966192D0/ 
      IF (NRING.EQ.0) RETURN
C     Find smallest X cood. on outer rim of accretion disk
C     Express coods. relative to the center of the gainer.
      IF (VI.LT.PIO2) THEN
      XLSL=1.D0
	DO J=1,NSG
		IF (ISTAR.EQ.ISV) THEN
			X=PXF(NRING,J)+DX
			Y=PYF(NRING,J)+DY
		ELSE
			X=PXF(NRING,J)+DDX
			Y=PYF(NRING,J)+DDY
		END IF
		IF (X.LT.XLSL) XLSL=X
	END DO
      DO K=1,NRING-1
         DO J=1,NSG
         IF (ISTAR.EQ.ISV) THEN
			X=PXF(K,J)+DX-XRMC
			Y=PYF(K,J)+DY-YRMC
         ELSE
			X=PXF(K,J)+DDX-XRMC
			Y=PYF(K,J)+DDY-YRMC
         END IF
         R=DSQRT(X*X+Y*Y)
         IF (DABS(Y).GT.1.0D-12) GO TO 1
         IF ((DABS(Y).LT.1.0D-12).AND.(J.EQ.NSG)) GO TO 1
         IF (X.LT.0.D0) AGL=0.D0
         IF (X.GT.0.D0) AGL=PI
         GO TO 2
    1    AGL=DASIN(DABS(Y)/R)
         IF ((Y.LT.0.D0).AND.(X.LT.0.D0)) GO TO 2
         IF ((Y.LT.0.D0).AND.(X.GE.0.D0)) AGL=PI-AGL
         IF ((Y.GE.0.D0).AND.(X.GE.0.D0)) AGL=AGL+PI
         IF ((Y.GE.0.D0).AND.(X.LT.0.D0)) AGL=TWPI-AGL
    2    CONTINUE
         IF ((AGL.GE.0.D0).AND.(AGL.LE.TWPI)) GO TO 8
	   IF (AGL.LT.0.D0) AGL=AGL+TWPI
	   IF (AGL.GT.TWPI) AGL=AGL-TWPI
    8       DO I=1,NSG-1
			LPCT=0
			IF (DABS(AGL-ANG(I)).LT.1.D-8) GO TO 5
			IF ((AGL.GT.ANG(I)).AND.(AGL.LT.ANG(I+1))) GO TO 3
            END DO
		  I=NSG
            IF (DABS(AGL-ANG(NSG)).LT.1.D-9) GO TO 5
		  IF ((AGL.GT.ANG(NSG)).AND.(AGL.LT.ANG(1))) GO TO 200
            ANGMX=-1.D0
		  ANGMN=10.D0
		DO I=1,NSG
			IF (ANG(I).GT.ANGMX) ANGMX=ANG(I)
			IF (ANG(I).LT.ANGMN) ANGMN=ANG(I)
		END DO
		DO I=1,NSG
			IF (ANG(I).EQ.ANGMX) GO TO 220
		END DO
		CALL PMDSTOP
  220		IF (I.GT.1) THEN
		IF ((ANG(I).LT.AGL).AND.(ANG(I+1).LT.AGL)) THEN
			VX(1)=ANG(I-1)
			VX(2)=ANG(I)
			VX(3)=ANG(I+1)+TWPI
			VR(1)=RD(I-1)
			VR(2)=RD(I)
			VR(3)=RD(I+1)
			GO TO 201
		END IF
		IF ((ANG(I).GT.AGL).AND.(ANG(I+1).GT.AGL)) THEN
			VX(1)=ANG(I-1)-TWPI
			VX(2)=ANG(I)-TWPI
			VX(3)=ANG(I+1)
			VR(1)=RD(I-1)
			VR(2)=RD(I)
			VR(3)=RD(I+1)
			GO TO 201
		END IF
		END IF
		IF (I.EQ.1) THEN
		IF ((ANG(I).LT.AGL).AND.(ANG(I+1).LT.AGL)) THEN
			VX(1)=ANG(I)
			VX(2)=ANG(I+1)+TWPI
			VX(3)=ANG(I+2)+TWPI
			VR(1)=RD(I)
			VR(2)=RD(I+1)
			VR(3)=RD(I+2)
			GO TO 201
		END IF
		IF ((ANG(I).GT.AGL).AND.(ANG(I+1).GT.AGL)) THEN
			VX(1)=ANG(I)-TWPI
			VX(2)=ANG(I+1)
			VX(3)=ANG(I+2)
			VR(1)=RD(I)
			VR(2)=RD(I+1)
			VR(3)=RD(I+2)
			GO TO 201
		END IF
		END IF
		DO I=1,NSG
		IF (ANG(I).EQ.ANGMN) GO TO 221
			END DO
		CALL PMDSTOP
  221		IF (I.GT.1) THEN
		IF ((ANG(I).GT.AGL).AND.(ANG(I+1).GT.AGL)) THEN
			VX(1)=ANG(I-1)
			VX(2)=ANG(I)
			VX(3)=ANG(I+1)-TWPI
			VR(1)=RD(I-1)
			VR(2)=RD(I)
			VR(3)=RD(I+1)
			GO TO 201
		END IF
		IF ((ANG(I).LT.AGL).AND.(ANG(I+1).LT.AGL)) THEN
			VX(1)=ANG(I-1)+TWPI
			VX(2)=ANG(I)+TWPI
			VX(3)=ANG(I+1)
			VR(1)=RD(I-1)
			VR(2)=RD(I)
			VR(3)=RD(I+1)
			GO TO 201
		END IF
		END IF
		IF (I.EQ.1) THEN
		IF ((ANG(I).GT.AGL).AND.(ANG(I+1).GT.AGL)) THEN
			VX(1)=ANG(NSG)-TWPI
			VX(2)=ANG(I)
			VX(3)=ANG(I+1)
			VR(1)=RD(NSG)
			VR(2)=RD(I)
			VR(3)=RD(I+1)
			GO TO 201
		END IF
		END IF
            CALL PMDSTOP
  200       VR(1)=RD(NSG-1)
            VR(2)=RD(NSG)
		  VR(3)=RD(1)
		  VX(1)=ANG(NSG-1)
		  VX(2)=ANG(NSG)
		  VX(3)=ANG(1)
		  GO TO 201
    5       RR=RD(I)
            GO TO 4
    3       M=2
            IF (I.EQ.1) M=1
    6       DO L=1,3
			VX(L)=ANG(I-M+L)
			VR(L)=RD(I-M+L)
            END DO
C     Check sequencing
          IF ((VX(1).LT.VX(2)).AND.(VX(2).LT.VX(3))) GO TO 201
		IF ((VX(1).GT.VX(3)).AND.(M.EQ.2)) THEN
			IF (I.LT.NSG-1) THEN
				M=1
				GO TO 6
			END IF
			IF (I.EQ.NSG-1) THEN
				VX(1)=ANG(NSG-1)
				VX(2)=ANG(NSG)
				VR(1)=RD(NSG-1)
				VR(2)=RD(NSG)
				IF (DABS(ANG(1)-ANG(NSG)).GT.1.D-12)THEN
					VX(3)=ANG(1)
					VR(3)=RD(1)
				ELSE
					VX(3)=ANG(2)
					VR(3)=RD(2)
				END IF
				IF ((VX(1).LT.VX(2)).AND.(VX(2).LT.VX(3)))GO TO 201
			END IF
		IF (I.EQ.1) THEN
			IF (VX(3).LT.VX(2)) VX(3)=VX(3)+TWPI
			IF ((VX(1).LT.VX(2)).AND.(VX(2).LT.VX(3))) GO TO 201
		END IF
		END IF
		IF (I.EQ.1) THEN
			IF (VX(3).LT.VX(2)) VX(3)=VX(3)+TWPI
			IF ((VX(1).LT.VX(2)).AND.(VX(2).LT.VX(3))) GO TO 201
		END IF
	CALL PMDSTOP
  201    CALL INTEP(AGL,RR,VX,VR,3,IER)
         IF (IER.EQ.1) GO TO 4
         RR=VR(3)
         DF=VX(3)-AGL
         IF (DABS(DF).LT.0.25D0) GO TO 4
         M=M+3
         LPCT=LPCT+1
         IF ((LPCT.LE.1).AND.((I-M+1).GT.0)) GO TO 6
         DF=VX(2)-AGL
         IF (DABS(DF).GT.0.25D0) GO TO 7
         RR=VR(2)
         GO TO 4
    7    DF=VX(1)-AGL
         IF (DABS(DF).GT.0.25D0) CALL PMDSTOP
         RR=VR(1)
C        RR is the radius to the outer rim for position angle AGL
C        R is the radius to the face profile at position angle AGL
C        If ER is (-), the point is hidden by the outer rim
    4    ER=RR-R
         IF ((PRFH(K,J).GE.0.D0).AND.(ER.LT.0.D0)) THEN
		PRFH(K,J)=ER
	   END IF
		IF ((X+XRMC).LT.XLSL) PRFH(K,J)=X+XRMC-XLSL
         END DO
      END DO
	ELSE
		DO K=1,NRING
			DO J=1,NSG
				PRFH(K,J)=-1.D0
			END DO
		END DO
		XLSL=1.D0
		DO J=1,NSG
			IF (ISTAR.EQ.ISV) THEN
				X=PXF(NRING,J)+DX
				Y=PYF(NRING,J)+DY
			ELSE
				X=PXF(NRING,J)+DDX
				Y=PYF(NRING,J)+DDY
			END IF
			IF (X.LT.XLSL) XLSL=X
		END DO
	END IF
   11 RETURN
      END
