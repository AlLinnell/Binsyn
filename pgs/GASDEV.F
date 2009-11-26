      FUNCTION GASDEV(IFLAG)
	USE PORTLIB
C     Function to return a normally distributed deviate with zero 
C     mean and unit variance. Adapted from Numerical Recipes.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DATA ISET/0/
	IF (ISET.EQ.0) THEN
    1		V1=2.D0*DRAND(IFLAG)-1.D0
		V2=2.D0*DRAND(IFLAG)-1.D0
		R=V1**2+V2**2
		IF (R.GE.1.D0) GO TO 1
		FAC=DSQRT(-2.D0*DLOG(R)/R)
		GSET=V1*FAC
		GASDEV=V2*FAC
		ISET=1
	ELSE
		GASDEV=GSET
		ISET=0
	END IF
	RETURN
	END