      SUBROUTINE SDUXDT(PPOTX,UY,DUXDT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the first time derivative of the stream
C     velocity in the X direction.
C     PPOTX is the first derivative of the potential with respect to X.
C     UY is the stream velocity in the Y direction.
C     DUXDT is the first time derivative of the stream
C     velocity in the X direction.
      DUXDT=-PPOTX+2.D0*UY
	RETURN
	END