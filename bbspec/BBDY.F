      SUBROUTINE BBDY(T,W,B) 
C     SRT TO CALCULATE PLANCK FUNCTION
C     T IS THE TEMPERATURE IN DEGREES KELVIN
C     W IS THE WAVELENGTH IN CM.
C     THE OUTPUT UNIT, B, IS FOR A SPECTRAL WIDTH OF ONE ANGSTROM
C     B is the intensity, measured in 
C     erg/cm^(2)/sec/angstrom/steradian
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA C1,C2/1.191043934D-5,1.43876866D0/
      V=C2/W/T
      IF (V.LE.690.D0) GO TO 1
      B=0.D0
      GO TO 2
   1  B=C1/W**5/(DEXP(V)-1.D0)/1.0D8
   2  RETURN
      END      
