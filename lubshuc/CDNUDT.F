      SUBROUTINE CDNUDT(VNU,OMGZ,XI,DNUDT)
C     Subroutine to calculate time derivative of nu
C     From Lubow & Shu, paper II, eq. 3
C     Note that u0(s)d(nu)/ds=d(nu)/dt
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  DNUDT=-VNU**2-OMGZ**2+XI
	  RETURN 
	  END