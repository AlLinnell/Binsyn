      SUBROUTINE XL2(VMU,X,VXL1,DVXL1)
C     Subroutine to calculate function and derivative for locating
C     the coordinate values for the L2 and L3 points.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	B=X+VMU
	A=B-1.D0
	VXL1=VMU/A**2+(1.D0-VMU)/B**2-X
	DVXL1=-2.D0*VMU/A**3-2.D0*(1.D0-VMU)/B**3-1.D0
	RETURN
	END