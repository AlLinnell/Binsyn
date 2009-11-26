      SUBROUTINE FTHETAP(ECC,OM,TH,VI,FN)
C     SRT which gives the partial derivative of the function evaluated
C     in SRT FTHETA withg respect to the variable TH.
C     Used inconnection with finding root of the FTHETA function.
          IMPLICIT DOUBLE PRECISION (A-H,O-Z)
          A=TH-OM
          SNDF=DSIN(A)
          CSDF=DCOS(A)
          SQI=DSIN(VI)**2
          C2TH=DCOS(2.D0*TH)
          SSQ=DSIN(TH)**2
          B=2.D0*SQI*C2TH
          C=2.D0*ECC*SNDF
          D=ECC*SQI*SSQ
          E=2.D0*SNDF+CSDF
          FN=B-C+D*E
          RETURN
          END
