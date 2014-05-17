      SUBROUTINE PPCMPX(X,Y,Z,VMU,VMOMU,PPOTX)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate, in center of mass coordinates,
C     the first derivative of the potential with respect to
C     the coordinate X.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU
C     PPOTX is the dimensionless derivative.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	PPOTX=VMU*A/C**1.5D0+VMOMU*B/D**1.5D0-X
	RETURN
	END