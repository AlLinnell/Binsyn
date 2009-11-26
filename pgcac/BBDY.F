      SUBROUTINE BBDY(T,W,B) 
C     SRT TO CALCULATE PLANCK FUNCTION
C     T IS THE TEMPERATURE IN THOUSANDS OF DEGREES KELVIN
C     W IS THE WAVELENGTH IN MICRONS  !!INPUT UNIT!!
C     THE OUTPUT UNIT, B, IS FOR A SPECTRAL WIDTH OF ONE MICRON!
C     B is the intensity, measured in 
C     erg/cm^(2)/sec/micron/steradian
C     Note that the division by PI produces the intensity.
C     See Allen, Astrophysical Quantities, third edition, p.104ff
C     The output quantity, B, is consistent with B(lambda), but
C     for a spectral width of 1 micron. Multiply by PI to get
C     the Allen script F(lambda).
C     See the program BBSPEC to get the conversion to units for
C     comparison with Hubeny output of SYNSPEC. 
C     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA C1,C2/3.74185D11,14.3883D0/
	DATA PI/3.14159265D0/
      V=C2/W/T
      IF (V.LE.690.D0) GO TO 1
      B=0.D0
      GO TO 2
   1  B=C1/W**5/(DEXP(V)-1.D0)/PI
   2  RETURN
      END      
