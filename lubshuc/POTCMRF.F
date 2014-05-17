      SUBROUTINE POTCMRF(X,Y,Z,VMU,VMOMU,POTCR,POTERR)
	  IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the difference between the potential at
C     point X,Y,Z and the critical potential POTCR.
      CALL POTCM(X,Y,Z,VMU,VMOMU,POT)
	  POTERR=POT-POTCR
	  RETURN
	  END