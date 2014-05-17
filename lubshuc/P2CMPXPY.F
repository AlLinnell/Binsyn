      SUBROUTINE P2CMPXPY(X,Y,Z,VMU,VMOMU,PP2TXTY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate, in center of mass coordinates,
C     the second derivative of the potential with respect to
C     the coordinates X and Y.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU
C     PP2TXTY is the dimensionless derivative.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	PP2TXTY=-3.D0*VMU*Y*A/C**2.5D0-3.D0*VMOMU*Y*B/D**2.5D0
	RETURN
	END