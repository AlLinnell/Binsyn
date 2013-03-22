      SUBROUTINE TPWL(FL,WL,RWL,LWL,NWL,FTP)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     Subroutine to produce an interpolated flux value at wavelength
C        RWL
C     FL=array of flux values of length NWL, synthetic spectrum
C     WL=corresponding array of wavelengths in ascending order
C     RWL=reference wavelength for which an interpolated flux value
C        is needed
C     LWL=ordinal value of WL entry immediately preceding the RWL of
C        the last previous interpolation
C     NWL=number of entries in arrays FL and WL
C     FTP=interpolated flux value
      DIMENSION FL(NWL),WL(NWL)
      FTP=0.0
      WL1=WL(1)
      WL2=WL(NWL)
      IF ((RWL.LT.WL1).OR.(RWL.GT.WL2)) GO TO 99
      IF ((LWL.LE.1).OR.(LWL.GE.NWL)) LWL=1
      IF (RWL.GE.WL(LWL)) THEN
C     If RWL is beyond WL(LWL) follow this sequence
         DO J=1,NWL
C     If RWL coincides with later entry, no interpolation is needed
            IF (ABS(RWL-WL(LWL+J)).LT.1.E-6) THEN
			LWL=LWL+J
			GO TO 30
		  END IF
C     If we reach the end of the array and there is neither wavelength
C     coincidence nor interpolation, then error
C            IF (J.EQ.NWL) CALL PMDSTOP
            IF (LWL+J.GE.NWL) CALL PMDSTOP
C     If successive WL entries bracket RWL, interpolate
            IF ((WL(LWL+J-1).LE.RWL).AND.(WL(LWL+J).GT.RWL)) GO TO 10
         END DO
C     If we reach the end of the synthetic spectrum array without
C     either coinciding with RWL or bracketing it, error
         CALL PMDSTOP
C     Interpolate for synthetic spectrum flux at RWL
   10    LWL=LWL+J-1
         CALL LNTER1(WL(LWL),FL(LWL),WL(LWL+1),FL(LWL+1),RWL,FTP)
         GO TO 99
C     If RWL is before WL(LWL) step back to locate it
      ELSE
         DO J=1,NWL
C     If RWL coincides with an earlier entry, set flux and reset LWL
            IF (ABS(RWL-WL(LWL-J)).LT.1.0E-6) GO TO 50
C     If an earlier entry pair brackets RWL, interpolate and reset LWL
C            IF (LWL-J.LE.1) CALL PMDSTOP
            IF ((WL(LWL-J).LE.RWL).AND.(WL(LWL-J+1).GT.RWL)) GO TO 20
         END DO
C     If the scan does not find an interpolating pair, error
         CALL PMDSTOP
C     Reset LWL and interpolate
   20    LWL=LWL-J
         CALL LNTER1(WL(LWL),FL(LWL),WL(LWL+1),FL(LWL+1),RWL,FTP)
      END IF
      GO TO 99
   30 FTP=FL(LWL)
      GO TO 99
   40 FTP=FL(1)
      GO TO 99
   50 FTP=FL(LWL-J)
      LWL=LWL-J
   99 RETURN
      END
