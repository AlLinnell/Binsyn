      SUBROUTINE CDALDT(DUSDT,ALPH,U,RD,BET,PP2TNTS,DALDT)
C     Subroutine to calculate first derivative of ALPHA w/resp to time
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	TERM1=(U/RD+ALPH)*BET
	TERM2=DUSDT*ALPH/U
	DALDT=-TERM2-TERM1-PP2TNTS+2.D0*BET
C      DALDT=-TERM2-PP2TNTS
	RETURN
	END