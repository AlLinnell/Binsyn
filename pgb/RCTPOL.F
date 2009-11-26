      SUBROUTINE RCTPOL(IRD,X,Y,R,ANG,IBRANCH)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA PIO2/1.5707963267948966192D0/
      DATA PI/3.141592653589793238463D0/
	DATA ERLT/1.D-13/
      R=DSQRT(X**2+Y**2)
C     Jump if it is the large theta branch
      IF (IBRANCH.EQ.2) GO TO 10
C     We have the small theta branch, using IRC.
C     The angles always go through increasingly negative values.
      IF (IBRANCH.NE.1) CALL PMDSTOP
      IF (IRD.EQ.-1) THEN
         IF (Y.LT.0.D0) ANG=DATAN(X/DABS(Y))
         IF (DABS(Y).LE.ERLT) ANG=-PIO2
         IF (Y.GT.0.D0) ANG=-PI-DATAN(X/DABS(Y))
      ELSE
        IF (Y.LT.0.D0) ANG=-PI-DATAN(X/DABS(Y))
        IF (DABS(Y).LE.ERLT) ANG=-PIO2
        IF (Y.GT.0.D0) ANG=DATAN(X/DABS(Y))
      END IF
      GO TO 99
C     We have the large theta branch, using IFC.
C     The angles go through increasingly positive values.
   10 IF (IRD.EQ.-1) THEN
         IF (Y.LT.0.D0) ANG=DATAN(X/DABS(Y))
         IF (DABS(Y).LE.ERLT) ANG=PIO2
         IF (Y.GT.0.D0) ANG=PI-DATAN(X/DABS(Y))
      ELSE
         IF (Y.LT.0.D0) ANG=PI-DATAN(X/DABS(Y))
         IF (DABS(Y).LE.ERLT) ANG=PIO2
         IF (Y.GT.0.D0) ANG=DATAN(X/DABS(Y))
      END IF
   99 RETURN
      END
