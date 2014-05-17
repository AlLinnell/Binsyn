      SUBROUTINE POTCM(X,Y,Z,VMU,VMOMU,POT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the potential, in center of mass
C     coordinates, given the X, Y, and Z coordinates of the point.
C     The potential is in dimensionless units, and includes centripetal
C     effects.
C     VMU is the mass of the detached component in system mass units.
C     VMOMU is 1.0-VMU.
C     POT is the dimensionless potential.
      A=X-1.D0+VMU
	B=X+VMU
	C=A**2+Y**2+Z**2
	D=B**2+Y**2+Z**2
	POT=-VMU/DSQRT(C)-VMOMU/DSQRT(D)-0.5D0*(X**2+Y**2)
	RETURN
	END