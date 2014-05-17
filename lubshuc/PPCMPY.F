      SUBROUTINE PPCMPY(X,Y,Z,VMU,VMOMU,PPOTY)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate, in center of mass coordinates,
C     the first derivative of the potential with respect to
C     the coordinate Y.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU
C     PPOTY is the dimensionless derivative.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	PPOTY=VMU*Y/C**1.5D0+VMOMU*Y/D**1.5D0-Y
	RETURN
	END