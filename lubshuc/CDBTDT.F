      SUBROUTINE CDBTDT(BETA,GAMMA,ALPHA,UV,RDC,PP2TN,DBTDT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the time derivative of beta.
C     From Lubow & Shu, paper I, eq. 56c
C     Note that u0(s)d(beta)/ds=d(beta)/dt
      DBTDT=-BETA**2-(UV/RDC)**2+2.D0*UV/RDC*ALPHA-PP2TN+GAMMA
     $-2.D0*ALPHA
	RETURN
	END