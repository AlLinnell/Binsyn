      SUBROUTINE FMEANAN(ANG,ECC,FMN)
C     SRT to calculate mean anomaly, given orbital eccentricity and
C     true anomaly
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=1.D0-ECC
      B=1.D0+ECC
      C=DSQRT(A*B)
      D=DSQRT(A/B)
      E=ECC*C*DSIN(ANG)/(1.D0+ECC*DCOS(ANG))
      F=2.D0*(DATAN(D*DTAN(ANG/2.D0)))
      FMN=F-E
      RETURN
      END
