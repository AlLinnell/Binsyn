      SUBROUTINE CDGMDT(BETA,GAMMA,DGMDT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate time derivative of gamma.
C     From Lubow & Shu, paper I, eq. 56a
C     Note that u0(s)d(gamma)/ds=d(gamma)/dt
	DGMDT=-2.D0*BETA*GAMMA
	RETURN
	END