      SUBROUTINE FNDLTA(THETA,OMRAD,ECC,SVI,FNVL)
C     SRT to calculate derivative of function for SRT FNDLT with
C     respect to THETA
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=1.D0-SVI*(DCOS(THETA))**2
      B=1.D0-ECC*DSIN(THETA-OMRAD)
      C=DSIN(2.D0*THETA)
      D=2.D0*ECC*DCOS(THETA-OMRAD)
      E=B*SVI*C+D*A
      FNVL=-0.5D0*E/B/B/DSQRT(A)
      RETURN
      END
