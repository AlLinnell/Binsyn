      SUBROUTINE FTHETA(ECC,OM,TH,VI,FN)
C     Function whose root gives desired value of TH.
C     Used to find position angle associated with true anomaly at
C     midprimary minimum.
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      A=TH-OM
      SNDF=DSIN(A)
      CSDF=DCOS(A)
      SQI=DSIN(VI)**2
      S2TH=DSIN(2.D0*TH)
      CSQ=DCOS(TH)**2
      B=(1.D0-ECC*SNDF)
      C=B*S2TH*SQI
      D=1.D0-CSQ*SQI
      E=2.D0*ECC*CSDF*D
      FN=C+E
      RETURN
      END
