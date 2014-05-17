      SUBROUTINE PP2CMPY(X,Y,Z,VMU,VMOMU,PP2TY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate, in center of mass coordinates,
C     the second derivative of the potential with respect to
C     the coordinate Y.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU
C     PP2TY is the dimensionless derivative.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	PP2TY=VMU*(A**2-2.D0*Y**2+Z**2)/C**2.5D0+
     $VMOMU*(B**2-2.D0*Y**2+Z**2)/D**2.5D0-1.D0
	RETURN
	END