      SUBROUTINE FNDLT(THETA,OMRAD,ECC,SVI,ALPHA,FNVL)
C     SRT to calculate function whose roots determine first through
c     fourth contacts in eclipses
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=1.D0-SVI*(DCOS(THETA))**2
      B=1.D0-ECC*DSIN(THETA-OMRAD)
	C=DSQRT(1.D0-ECC**2)
      FNVL=ALPHA-DSQRT(A)/B*C
      RETURN
      END
