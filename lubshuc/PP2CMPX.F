      SUBROUTINE PP2CMPX(X,Y,Z,VMU,VMOMU,PP2TX)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate, in center of mass coordinates,
C     the second derivative of the potential with respect to
C     the coordinate X.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU
C     PP2TX is the dimensionless derivative.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	E=2.D0*A**2
	F=2.D0*B**2
	PP2TX=VMU*(Y**2+Z**2-E)/C**2.5D0+VMOMU*(Y**2+Z**2-F)/D**2.5D0-1.D0
	RETURN
	END