      SUBROUTINE SDUYDT(PPOTY,UX,DUYDT)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to calculate the first time derivative of the stream
C     velocity in the Y direction.
C     PPOTY is the first derivative of the potential with respect to Y.
C     UX is the stream velocity in the X direction.
C     DUYDT is the first time derivative of the stream
C     velocity in the Y direction.
      DUYDT=-PPOTY-2.D0*UX
	RETURN
	END