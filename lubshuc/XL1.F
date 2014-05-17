      SUBROUTINE XL1(VMU,X,VXL1,DVXL1)
C     Subroutine to calculate function and its derivative for locating
C     the coordinate value for the L1 point
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=X-1.D0+VMU
	B=A+1.D0
	VXL1=X+VMU/A**2-(1.D0-VMU)/B**2
	DVXL1=1.D0-2.D0*VMU/A**3+2.D0*(1.D0-VMU)/B**3
	RETURN
	END