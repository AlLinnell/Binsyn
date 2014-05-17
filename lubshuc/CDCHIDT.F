      SUBROUTINE CDCHIDT(VNU,CHI,DCHIDT)
C     Subroutine to calculate the time derivative of xi
C     From Lubow & Shu, paper II, eq. 3
C     Note that u0(s)d(xi)/ds=d(xi)dt
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	  DCHIDT=-2.D0*VNU*CHI
	  RETURN
	  END